;;; netz-query.el --- Query DSL for netz graphs -*- lexical-binding: t; -*-

;;; Commentary:
;; Cypher-inspired keyword-clause query DSL for netz.

;;; Code:

(require 'cl-lib)
(require 'netz)

(defun netz-query--binding-key (var)
  (intern (concat ":" (symbol-name var))))

(defun netz-query--anonymous-p (var)
  (eq var '_))

(defun netz-query--collect-vars-from-pattern (pattern)
  (pcase pattern
    (`(:node ,var . ,_) (unless (netz-query--anonymous-p var) (list var)))
    (`(:edge ,from ,to . ,rest)
     (delq nil (list (unless (netz-query--anonymous-p from) from)
                     (unless (netz-query--anonymous-p to) to)
                     (plist-get rest :as))))
    (`(:path ,from ,to . ,opts)
     (delq nil (list (unless (netz-query--anonymous-p from) from)
                     (unless (netz-query--anonymous-p to) to)
                     (plist-get opts :as))))
    (_ nil)))

(defun netz-query--collect-vars (clauses)
  (delete-dups
   (cl-loop for clause in clauses append
            (pcase clause
              (`(:match . ,patterns) (cl-loop for pattern in patterns append (netz-query--collect-vars-from-pattern pattern)))
              (`(:create . ,patterns) (cl-loop for pattern in patterns append (netz-query--collect-vars-from-pattern pattern)))
              (`(:merge . ,patterns) (cl-loop for pattern in patterns append (netz-query--collect-vars-from-pattern pattern)))
              (`(:set ,var . ,_) (list var))
              (`(:delete . ,vars) vars)
              (`(:detach-delete . ,vars) vars)
              (`(:return . ,items) (cl-remove-if #'keywordp items))
              (_ nil)))))

(defmacro netz-query (graph &rest clauses)
  "Run keyword-clause query against GRAPH."
  (let* ((vars (netz-query--collect-vars clauses))
         (bindings (mapcar (lambda (var)
                             `(,var (plist-get row ',(netz-query--binding-key var))))
                           vars))
         (compiled
          (mapcar (lambda (clause)
                    (pcase clause
                      (`(:where . ,forms)
                       `(list :where-fn
                              (lambda (row)
                                (let ,bindings
                                  ,@forms))))
                      (_ `',clause)))
                  clauses)))
    `(netz-query--execute ,graph (list ,@compiled))))

(defun netz-query--validate-clauses (clauses)
  (let (bound-vars)
    (dolist (clause clauses)
      (pcase clause
        (`(:match . ,patterns)
         (dolist (pattern patterns)
           (pcase pattern
             (`(:node ,var . ,_)
              (unless (netz-query--anonymous-p var) (push var bound-vars)))
             (`(:edge ,from ,to . ,props)
              (let ((direction (or (plist-get props :direction) :any)))
                (unless (netz--valid-direction-p direction)
                  (error "Invalid edge direction: %S" direction)))
              (unless (netz-query--anonymous-p from) (push from bound-vars))
              (unless (netz-query--anonymous-p to) (push to bound-vars))
              (when-let ((as (plist-get props :as))) (push as bound-vars)))
             (`(:path ,from ,to . ,opts)
              (let ((direction (or (plist-get opts :direction) :any)))
                (unless (netz--valid-direction-p direction)
                  (error "Invalid path direction: %S" direction)))
              (unless (netz-query--anonymous-p from) (push from bound-vars))
              (unless (netz-query--anonymous-p to) (push to bound-vars))
              (when-let ((as (plist-get opts :as))) (push as bound-vars))))))
        (`(:create . ,patterns)
         (dolist (pattern patterns)
           (setq bound-vars (append (netz-query--collect-vars-from-pattern pattern) bound-vars))))
        (`(:merge . ,patterns)
         (dolist (pattern patterns)
           (setq bound-vars (append (netz-query--collect-vars-from-pattern pattern) bound-vars))))
        (`(:return . ,items)
         (dolist (var (cl-remove-if #'keywordp items))
           (unless (memq var bound-vars)
             (error "Unbound variable: %S" var))))))))

(defun netz-query--execute (graph clauses)
  (setq graph (netz--coerce-graph graph))
  (netz-query--validate-clauses clauses)
  (let ((rows (list nil))
        return-clause)
    (dolist (clause clauses)
      (pcase clause
        (`(:match . ,patterns)
         (setq rows (netz-query--match-patterns graph rows patterns)))
        (`(:where-fn ,fn)
         (setq rows (cl-remove-if-not fn rows)))
        (`(:create . ,patterns)
         (setq rows (netz-query--create-patterns graph rows patterns)))
        (`(:merge . ,patterns)
         (setq rows (netz-query--merge-patterns graph rows patterns)))
        (`(:set ,var . ,props)
         (setq rows (netz-query--set graph rows var props)))
        (`(:delete . ,vars)
         (netz-query--delete graph rows vars nil))
        (`(:detach-delete . ,vars)
         (netz-query--delete graph rows vars t))
        (`(:return . ,_)
         (setq return-clause clause))
        (_ (error "Unknown query clause: %S" clause))))
    (if return-clause
        (netz-query--return graph rows return-clause)
      rows)))

(defun netz-query--plist-match-p (entity props)
  (cl-loop for (key value) on props by #'cddr
           always (equal (plist-get entity key) value)))

(defun netz-query--bind (row var value)
  (if (netz-query--anonymous-p var)
      row
    (let* ((key (netz-query--binding-key var))
           (existing (plist-get row key)))
      (cond ((null existing) (plist-put (copy-sequence row) key value))
            ((equal existing value) row)
            (t nil)))))

(defun netz-query--require-bound (row var)
  (let ((value (plist-get row (netz-query--binding-key var))))
    (unless value (error "Unbound variable: %S" var))
    value))

(defun netz-query--node-candidates (graph props)
  (if-let ((id (plist-get props :id)))
      (let ((node (netz-get-node graph id)))
        (if (and node (netz-query--plist-match-p node props)) (list node) nil))
    (cl-remove-if-not (lambda (node) (netz-query--plist-match-p node props))
                      (netz-get-nodes graph))))

(defun netz-query--match-node (graph row pattern)
  (pcase pattern
    (`(:node ,var . ,props)
     (unless var (error "Malformed node pattern: %S" pattern))
     (let ((existing (unless (netz-query--anonymous-p var)
                       (plist-get row (netz-query--binding-key var)))))
       (if existing
           (when (netz-query--plist-match-p existing props) (list row))
         (mapcar (lambda (node)
                   (if (netz-query--anonymous-p var)
                       row
                     (netz-query--bind row var node)))
                 (netz-query--node-candidates graph props)))))
    (_ (error "Malformed node pattern: %S" pattern))))

(defun netz-query--edge-endpoint-p (edge bound-var candidate-var row direction)
  (let ((bound-id (plist-get (netz-query--require-bound row bound-var) :id))
        (candidate-id (plist-get candidate-var :id)))
    (pcase direction
      (:out (and (equal (plist-get edge :source) bound-id)
                 (equal (plist-get edge :target) candidate-id)))
      (:in (and (equal (plist-get edge :target) bound-id)
                (equal (plist-get edge :source) candidate-id)))
      (:any (or (and (equal (plist-get edge :source) bound-id)
                     (equal (plist-get edge :target) candidate-id))
                (and (equal (plist-get edge :target) bound-id)
                     (equal (plist-get edge :source) candidate-id)))))))

(defun netz-query--edge-neighbor (graph edge node-id direction)
  (pcase direction
    (:out (when (equal (plist-get edge :source) node-id)
            (netz-get-node graph (plist-get edge :target))))
    (:in (when (equal (plist-get edge :target) node-id)
           (netz-get-node graph (plist-get edge :source))))
    (:any (cond ((equal (plist-get edge :source) node-id)
                 (netz-get-node graph (plist-get edge :target)))
                ((equal (plist-get edge :target) node-id)
                 (netz-get-node graph (plist-get edge :source)))))))

(defun netz-query--edge-candidates (graph row from to props direction)
  (let ((from-node (unless (netz-query--anonymous-p from)
                     (plist-get row (netz-query--binding-key from))))
        (to-node (unless (netz-query--anonymous-p to)
                   (plist-get row (netz-query--binding-key to)))))
    (cond
     (from-node
      (let ((edge-ids (netz-node-edge-ids graph (plist-get from-node :id) :direction direction)))
        (cl-loop for edge-id in edge-ids
                 for edge = (netz-get-edge graph edge-id)
                 for neighbor = (netz-query--edge-neighbor graph edge (plist-get from-node :id) direction)
                 when (and neighbor (netz-query--plist-match-p edge props)
                           (or (not to-node) (equal neighbor to-node)))
                 collect (list edge neighbor))))
     (to-node
      (let* ((reverse-direction (pcase direction (:out :in) (:in :out) (_ :any)))
             (edge-ids (netz-node-edge-ids graph (plist-get to-node :id) :direction reverse-direction)))
        (cl-loop for edge-id in edge-ids
                 for edge = (netz-get-edge graph edge-id)
                 for neighbor = (netz-query--edge-neighbor graph edge (plist-get to-node :id) reverse-direction)
                 when (and neighbor (netz-query--plist-match-p edge props))
                 collect (list edge neighbor))))
     (t
      (cl-loop for edge in (netz-get-edges graph)
               when (netz-query--plist-match-p edge props)
               collect (list edge nil))))))

(defun netz-query--match-edge (graph row pattern)
  (pcase pattern
    (`(:edge ,from ,to . ,raw-props)
     (unless (and from to) (error "Malformed edge pattern: %S" pattern))
     (let* ((as (plist-get raw-props :as))
            (direction (or (plist-get raw-props :direction) :any))
            (props (copy-sequence raw-props)))
       (unless (netz--valid-direction-p direction)
         (error "Invalid edge direction: %S" direction))
       (setq props (plist-put props :as nil)
             props (plist-put props :direction nil))
       (setq props (cl-loop for (k v) on props by #'cddr
                            unless (or (eq k :as) (eq k :direction) (null v))
                            append (list k v)))
       (delq nil
             (cl-loop for (edge neighbor) in (netz-query--edge-candidates graph row from to props direction)
                      collect
                      (let ((new-row row))
                        (when as (setq new-row (netz-query--bind new-row as edge)))
                        (when new-row
                          (cond
                           ((and (not (netz-query--anonymous-p from))
                                 (not (plist-get new-row (netz-query--binding-key from))))
                            (setq new-row (netz-query--bind new-row from neighbor)))
                           ((and (not (netz-query--anonymous-p to))
                                 (not (plist-get new-row (netz-query--binding-key to))))
                            (setq new-row (netz-query--bind new-row to neighbor))))
                          new-row))))))
    (_ (error "Malformed edge pattern: %S" pattern))))

(defun netz-query--match-pattern (graph rows pattern)
  (pcase pattern
    (`(:node . ,_) (cl-loop for row in rows append (netz-query--match-node graph row pattern)))
    (`(:edge . ,_) (cl-loop for row in rows append (netz-query--match-edge graph row pattern)))
    (`(:path . ,_) (cl-loop for row in rows append (netz-query--match-path graph row pattern)))
    (_ (error "Unknown match pattern: %S" pattern))))

(defun netz-query--match-patterns (graph rows patterns)
  (dolist (pattern patterns rows)
    (setq rows (netz-query--match-pattern graph rows pattern))))

(defun netz-query--edge-traversal-direction (edge node-id direction)
  "Return the actual traversal direction across EDGE from NODE-ID."
  (pcase direction
    (:out :out)
    (:in :in)
    (:any (if (equal (plist-get edge :source) node-id) :out :in))))

(defun netz-query--path-graph (nodes edges)
  "Return a graph containing path NODES and EDGES."
  (let ((path-graph (netz-create-graph (gensym "path"))))
    (dolist (node nodes)
      (netz-add-node path-graph node))
    (dolist (edge edges)
      (netz-add-edge path-graph edge))
    path-graph))

(defun netz-query--make-path (nodes edges steps)
  "Build a public path object from ordered NODES, EDGES, and STEPS."
  (let ((start (car nodes))
        (end (car (last nodes))))
    (list :start start
          :end end
          :nodes nodes
          :edges edges
          :steps steps
          :length (length edges)
          :graph (netz-query--path-graph nodes edges))))

(defun netz-query--path-routes (graph start-node edge-props direction min-depth max-depth)
  "Return path route objects reachable from START-NODE."
  (let ((queue (list (list start-node 0 nil (list start-node) nil nil)))
        results)
    (while queue
      (pcase-let ((`(,node ,depth ,visited ,nodes ,edges ,steps) (pop queue)))
        (let ((node-id (plist-get node :id)))
          (when (and (>= depth min-depth) (<= depth max-depth))
            (push (netz-query--make-path nodes edges steps) results))
          (when (< depth max-depth)
            (dolist (edge-id (netz-node-edge-ids graph node-id :direction direction))
              (let* ((edge (netz-get-edge graph edge-id))
                     (neighbor (netz-query--edge-neighbor graph edge node-id direction))
                     (traversed (and neighbor
                                     (netz-query--edge-traversal-direction edge node-id direction))))
                (when (and neighbor
                           (not (member edge-id visited))
                           (netz-query--plist-match-p edge edge-props))
                  (push (list neighbor
                              (1+ depth)
                              (cons edge-id visited)
                              (append nodes (list neighbor))
                              (append edges (list edge))
                              (append steps (list (list :from node
                                                        :edge edge
                                                        :to neighbor
                                                        :traversed traversed))))
                        queue))))))))
    (nreverse results)))

(defun netz-query--match-path (graph row pattern)
  (pcase pattern
    (`(:path ,from ,to . ,opts)
     (unless (and from to) (error "Malformed path pattern: %S" pattern))
     (let* ((as (plist-get opts :as))
            (direction (or (plist-get opts :direction) :any))
            (depth (or (plist-get opts :depth) '(1 1)))
            (edge-props (or (plist-get opts :edge) nil))
            (min-depth (car depth))
            (max-depth (cadr depth))
            (start (netz-query--require-bound row from)))
       (unless (netz--valid-direction-p direction)
         (error "Invalid path direction: %S" direction))
       (delq nil
             (mapcar (lambda (route)
                       (let ((new-row (netz-query--bind row to (plist-get route :end))))
                         (when (and new-row as)
                           (setq new-row (netz-query--bind new-row as route)))
                         new-row))
                     (netz-query--path-routes graph start edge-props direction min-depth max-depth)))))
    (_ (error "Malformed path pattern: %S" pattern))))

(defun netz-query--create-patterns (graph rows patterns)
  (dolist (pattern patterns rows)
    (setq rows
          (cl-loop for row in rows append
                   (pcase pattern
                     (`(:node ,var . ,props)
                      (let ((id (plist-get props :id)))
                        (unless id (error "Created node requires :id"))
                        (when (netz-get-node graph id) (error "Node already exists: %S" id))
                        (netz-add-node graph props)
                        (list (netz-query--bind row var (netz-get-node graph id)))))
                     (`(:edge ,from ,to . ,raw-props)
                      (let* ((source (plist-get (netz-query--require-bound row from) :id))
                             (target (plist-get (netz-query--require-bound row to) :id))
                             (id (plist-get raw-props :id))
                             (props (append raw-props (list :source source :target target))))
                        (unless id (error "Created edge requires :id"))
                        (when (netz-get-edge graph id) (error "Edge already exists: %S" id))
                        (netz-add-edge graph props)
                        (list row)))
                     (_ (error "Unknown create pattern: %S" pattern)))))))

(defun netz-query--merge-patterns (graph rows patterns)
  (dolist (pattern patterns rows)
    (setq rows
          (cl-loop for row in rows append
                   (pcase pattern
                     (`(:node ,var . ,props)
                      (let ((id (plist-get props :id)))
                        (unless id (error "Merged node requires :id"))
                        (unless (netz-get-node graph id) (netz-add-node graph props))
                        (list (netz-query--bind row var (netz-get-node graph id)))))
                     (`(:edge ,from ,to . ,raw-props)
                      (let* ((source (plist-get (netz-query--require-bound row from) :id))
                             (target (plist-get (netz-query--require-bound row to) :id))
                             (id (plist-get raw-props :id))
                             (props (append raw-props (list :source source :target target))))
                        (unless id (error "Merged edge requires :id"))
                        (unless (netz-get-edge graph id) (netz-add-edge graph props))
                        (list row)))
                     (_ (error "Unknown merge pattern: %S" pattern)))))))

(defun netz-query--set (graph rows var props)
  (dolist (row rows)
    (let* ((entity (netz-query--require-bound row var))
           (id (plist-get entity :id)))
      (cond ((and (plist-get entity :source) (plist-get entity :target))
             (netz-add-edge graph (apply #'netz-merge-plists (list entity props))))
            ((netz-get-node graph id)
             (netz-add-node graph (apply #'netz-merge-plists (list entity props))))
            (t (error "Cannot set unknown entity: %S" var)))))
  rows)

(defun netz-query--delete (graph rows vars detach)
  (dolist (row rows)
    (dolist (var vars)
      (let ((entity (netz-query--require-bound row var)))
        (if (and (plist-get entity :source) (plist-get entity :target))
            (netz-delete-edge graph (plist-get entity :id))
          (netz-delete-node graph (plist-get entity :id) :detach detach))))))

(defun netz-query--return (graph rows clause)
  (pcase clause
    (`(:return :count) (length rows))
    (`(:return :ids . ,vars)
     (cl-loop for row in rows append
              (mapcar (lambda (var)
                        (plist-get (netz-query--require-bound row var) :id))
                      vars)))
    (`(:return :pluck ,var ,prop)
     (mapcar (lambda (row) (plist-get (netz-query--require-bound row var) prop)) rows))
    (`(:return :graph ,name)
     (let ((result (netz-create-graph name)))
       (dolist (row rows)
         (cl-loop for (_key value) on row by #'cddr
                  when (and (consp value) (plist-get value :id))
                  do (if (and (plist-get value :source) (plist-get value :target))
                         (progn
                           (netz-add-node result (netz-get-node graph (plist-get value :source)))
                           (netz-add-node result (netz-get-node graph (plist-get value :target)))
                           (netz-add-edge result value))
                       (netz-add-node result value))))
       result))
    (`(:return . ,vars)
     (mapcar (lambda (row)
               (cl-loop for var in vars
                        append (list (netz-query--binding-key var)
                                     (netz-query--require-bound row var))))
             rows))
    (_ (error "Malformed return clause: %S" clause))))

(provide 'netz-query)

;;; netz-query.el ends here
