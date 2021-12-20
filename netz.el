(require 'ht)
(require 'dash)

(defvar *netz-graph-store* (concat user-emacs-directory ".netz-graph"))
(defvar *netz-graphs* (ht-create 'equal))

;; '(:name :test
;; :path *netz-graph-store* + name
;; :nodes (ht-create 'equal)
;; :edges (ht-create 'equal))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; graph management
;;;;;;;;;;;;;;;;;;;;;;;;
;; loading, saving, etc.

(defun netz--get-path (name &optional path)
  (let ((name (cond ((keywordp name)
		     (symbol-name (intern (substring (symbol-name name) 1))))
		    ((numberp name)
		     (number-to-string name))
		    ((stringp name)
		     name)
		    (t (error "Name must be a string, keyword, or number.")))))
    (if (not path)
	(concat *netz-graph-store* "/" name)
      path)))

(defun netz-make-graph (name &optional path)
  (let* ((path (netz--get-path name path))
	 (graph `(:name ,name
			:path ,path
			:nodes ,(ht-create 'equal)
			:edges ,(ht-create 'equal))))
    (ht-set! *netz-graphs* name graph)
    (netz-save-graph name)
    ))

(defun netz-save-graph (graph)
  (with-graph graph
	      (make-directory (file-name-directory (plist-get graph :path)) t)
	      (ht-set! *netz-graphs* (plist-get graph :name) graph)
	      (with-temp-buffer
		(print graph (current-buffer))
		(write-file (plist-get graph :path) nil))
	      graph))

(defun netz-load-graph (path)
  (if (file-exists-p path)
      (let* ((graph (read (netz-file-to-string path))))
	(ht-set! *netz-graphs* (plist-get graph :name) graph)
	graph)
    (error "File not found")))

(defun netz-get-graph-from-cache (name)
  (ht-get *netz-graphs* name))

(defun netz-file-to-string (file)
  "File to string function"
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; node mangement
;;;;;;;;;;;;;;;;;;;;;;;;
;; creating, adding, modifying, etc

(defun netz-add-node (node graph)
  (unless (plist-get node :id)
    (error "Node must include an :id parameter"))
  (with-nodes graph
	      (ht-set! nodes (plist-get node :id) node)
	      graph))

(defun netz-get-node (node-id graph)
  (with-nodes graph
	      (ht-get nodes node-id)))

(defun netz-get-nodes (graph)
  (with-nodes graph
	      nodes))

(defun netz-add-edge-to-node (node edge)
  "get current edges from `node', if `edge' doesn't exist
add it to existing list of edges"
  (let ((edges (plist-get node :edges)))
    (unless (member edge edges)
      (plist-put node :edges (cons edge (plist-get node :edges))))))

(defun netz-delete-node (node graph)
  (with-nodes-edges graph
		    (let ((node-id (plist-get node :id))
			  (node-edges (plist-get node :edges)))
		      (ht-remove! nodes node-id)
		      (mapc (lambda (edge)
			      (ht-remove! edges edge)
			      (netz--delete-edge-from-node-edges
			       (car (remove node-id edge)) edge graph))
			    node-edges))))

(defun netz--delete-edge-from-node-edges (node-id edge-id graph)
  (with-graph graph
	      (let ((node (netz-get-node node-id graph)))
		(plist-put node :edges (remove edge-id (plist-get node :edges))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; edge management
;;;;;;;;;;;;;;;;;;;;;;;;
;; creating, adding, modifying etc

(defun netz-add-edge (edge graph)
  (unless (plist-get edge :id)
    (error "Edge must include an :id parameter"))
  (with-edges graph
	      (ht-set! edges (plist-get edge :id) edge)
	      graph))

(defun netz-get-edge (edge-id graph)
  (with-edges graph
	      (ht-get edges edge-id)))

(defun netz-get-edges (graph)
  (with-edges graph
	      edges))

(defun netz-delete-edge (id graph)
  "delete edge and clean up node edges"
  (with-edges graph
	      (ht-remove! edges id)
	      (netz--delete-edge-from-node-edges (car id) id graph)
	      (netz--delete-edge-from-node-edges (cadr id) id graph)))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; relationships
;;;;;;;;;;;;;;;;;;;;;;;;

(defun netz-connect-nodes (source target edge-params graph)
  "add edge with `edge-params' connecting `source' and `target'"
  (with-nodes-edges graph
		    (let ((edge `(,(plist-get source :id) ,(plist-get target :id))))
		      (netz-add-edge (plist-put edge-params :id edge) graph)
		      (netz-add-node (netz-add-edge-to-node target edge) graph)
		      (netz-add-node (netz-add-edge-to-node source edge) graph))))

(defun netz-get-node-hood (id graph new-graph)
  (with-graph graph
	      (let* ((source-node (netz-get-node id graph))
		     (new-graph (netz-make-graph new-graph))
		     (edges (netz-get-edges-hash-for-node
			     source-node
			     graph))
		     (nodes (ht-select-keys
			     (netz-get-nodes graph)
			     (delete-dups (-flatten (ht-keys edges))))))
		;; (list nodes edges)
		(plist-put new-graph :nodes nodes)
		(plist-put new-graph :edges edges)
		new-graph)))

(defun netz-get-edges-hash-for-node (node graph)
  (with-graph graph
	      (let ((edge-keys (plist-get node :edges))
		    (edges (netz-get-edges graph)))
		(ht-select-keys edges edge-keys))))

;; TODO this works, but surely there is a cleaner way
(defun netz-bfs-shortest-path (source target graph)
  (let* ((source-id (plist-get source :id))
	 (target-id (plist-get target :id))
	 (queue `((,source-id)))
	 (visited nil)
	 (result nil))
    (catch 'found
      (while queue
	(let* ((path (pop queue))
	       (node (-last-item path)))
	  (unless (member node visited)
	    (let ((neighbors (netz-node-neighbors (netz-get-node node graph))))
	      (dolist (neighbor neighbors)
		(let ((new-path path))
		  (setq new-path (-snoc new-path neighbor))
		  (setq queue (-snoc queue new-path))
		  (when (equal neighbor target-id)
		    (setq result new-path)
		    (throw 'found result))))
	      (setq visited (-snoc visited node)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; utilities
;;;;;;;;;;;;;;;;;;;;;;;;

(defun netz-node-ids-to-graph (ids old-graph new-name)
  "takes a list of `ids' and returns a new graph
containing the nodes and related edges."
  (let ((new-graph (netz-make-graph new-name))
	(old-graph (netz-get-graph-from-cache old-graph)))
    (with-graph old-graph
		(dolist (node-id ids)
		  (netz-add-node (netz-get-node node-id old-graph) new-graph)
		  (dolist (edge-id (plist-get (netz-get-node node-id old-graph) :edges))
		    (when (netz--both-edges-in-node-list ids edge-id)
		      (netz-add-edge (netz-get-edge edge-id old-graph) new-graph)))))
    new-graph))

(defun netz--both-edges-in-node-list (node-ids edge-id)
  (and (-contains? node-ids (car edge-id)) (-contains? node-ids (cadr edge-id))))

(defun netz-get-edge-property (edge-id property graph)
  (let ((edge (netz-get-edge edge-id graph)))
    (plist-get edge property)))

(defun netz-node-neighbors (node)
  (delete-dups (remove (plist-get node :id) (-flatten (plist-get node :edges)))))

(defun netz-loaded-graph-guard ()
  (unless *netz-graph*
    (error "Netz graph not loaded")))

(defmacro with-update-cache (&rest body)
  "manage handling checking for graph and saving etc."
  `(progn (netz-loaded-graph-guard)
	  ,@body
	  (netz-save-graph)))

(defmacro with-guard (&rest body)
  `(progn (netz-loaded-graph-guard)
	  ,@body))

(defmacro netz (func &rest args)
  (let ((func-name (intern (concat "netz-" (symbol-name func)))))
    `(progn
       (netz-loaded-graph-guard)
       (,func-name ,@args *netz-graph*))))

(defmacro with-graph (graph &rest body)
  `(let ((,graph (if (not (consp ,graph))
		     (ht-get *netz-graphs* ,graph)
		   ,graph)))
     ,@body)
  ;; (print graph)
  ;; (let ((graph-name (if (consp graph)
  ;; 			(when (stringp (car graph))
  ;; 			  (car graph))
  ;; 		      graph))
  ;; 	(graph (if (consp graph)
  ;; 		   (when (stringp (car graph))
  ;; 		     (cadr graph))
  ;; 		 graph)))
  ;;   `(let ((,graph-name (if (not (consp ,graph))
  ;; 			    (ht-get *netz-graphs* ,graph)
  ;; 			  ,graph)))
  ;;      ,@body))
  )

(defmacro with-nodes (graph &rest body)
  `(with-graph ,graph
	      (let ((nodes (plist-get ,graph :nodes)))
		,@body)))

(defmacro with-edges (graph &rest body)
  `(with-graph ,graph
	       (let ((edges (plist-get ,graph :edges)))
		 ,@body)))

(defmacro with-nodes-edges (graph &rest body)
  `(with-graph ,graph
	       (let ((edges (plist-get ,graph :edges))
		     (nodes (plist-get ,graph :nodes)))
		 ,@body)))

(provide 'netz)
