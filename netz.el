(require 'ht)

(defvar *netz-graph* nil)
(defvar *netz-graph-store* (concat user-emacs-directory ".netz-graph"))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; graph management
;;;;;;;;;;;;;;;;;;;;;;;;
;; loading, saving, etc.

(defun netz-init-graph (&rest g)
  (cond (*netz-graph* t)
	((file-exists-p *netz-graph-store*)
	 (netz-load-graph))
	(t (netz-save-graph)))
  "Netz graph ready")

(defun netz-load-graph (&rest g)
  (if (file-exists-p *netz-graph-store*)
      (setq *netz-graph* (read (netz-file-to-string *netz-graph-store*)))
    "No graph found"))

(defun netz-save-graph (&rest g)
  (with-temp-buffer
    (unless *netz-graph*
      (setq *netz-graph*
	    `(,(make-hash-table :test 'equal)
	      ,(make-hash-table :test 'equal))))
    (print *netz-graph* (current-buffer))
    (write-file *netz-graph-store*))
  "Netz graph saved")

(defun netz-file-to-string (file)
  "File to string function"
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; node mangement
;;;;;;;;;;;;;;;;;;;;;;;;
;; creating, adding, modifying, etc

(defun netz--add-node (node graph)
  "low level function to update nodes hashmap"
  (ht-set! (car graph) (plist-get node :id) node))

(defun netz-add-node (node graph)
  "add a `node' to the provided `graph'
`node' should be a plist that contains an :id property"
  (unless (plist-get node :id)
    (error "Node must include an :id parameter"))
  (with-update-cache
   (netz--add-node node graph)))

(defun netz-add-node-no-save (node graph)
  "add a node but don't save the graph
This is useful when adding large numbers of nodes
or when dealing with multiple graphs etc."
  (netz--add-node node graph))

(defun netz--get-node (id graph)
  "low level functon to get node from graph"
  (gethash id (car graph)))

(defun netz-get-node (id graph)
  "return node with `id' from `graph'"
  (with-guard
   (netz--get-node id graph)))

(defun netz-add-edge-to-node (node edge)
  "get current edges from `node', if `edge' doesn't exist
add it to existing list of edges"
  (let ((edges (plist-get node :edges)))
    (unless (member edge edges)
      (plist-put node :edges (cons edge (plist-get node :edges))))))

(defun netz-delete-node (node graph)
  (let ((nodes (car graph))
	(edges (cadr graph))
	(node-id (plist-get node :id))
	(node-edges (plist-get node :edges)))
    (ht-remove! nodes node-id)
    (mapc (lambda (e)
	    (ht-remove! edges e)
	    (netz--delete-edge-from-node-edges (car (remove node-id e)) e graph)
	    ) node-edges)))

(defun netz--delete-edge-from-node-edges (node-id edge-id graph)
  (let ((node (netz-get-node node-id graph)))
    (plist-put node :edges (remove edge-id (plist-get node :edges)))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; edge management
;;;;;;;;;;;;;;;;;;;;;;;;
;; creating, adding, modifying etc

(defun netz--add-edge (edge graph)
  "low level function to add and edge to edge hashmap"
  (puthash (plist-get edge :id) edge (cadr graph)))

;; is this how i want to name add-edge and connect-nodes?
;; maybe not??
(defun netz-add-edge (edge graph)
  "add an `edge' to the provided `graph'
You should probably use `netz-connect-nodes' unless you
know you shouldn't. It will handle bookkeeping etc. This
will not."
  (unless (plist-get edge :id)
    (error "Edge must include an :id parameter"))
  (with-update-cache
   (netz--add-edge edge graph)))

(defun netz-add-edge-no-save (edge graph)
  "add and edge but don't save the graph
This is useful when adding large numbers of edges
or when dealing with multiple graphs etc."
  (netz--add-edge edge graph))

(defun netz--get-edge (id graph)
  "low level functon to get edge from graph"
  (gethash id (cadr graph)))

(defun netz-get-edge (id graph)
  "return edge with `id' from `graph'"
  (with-guard
   (netz--get-edge id graph)))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; relationships
;;;;;;;;;;;;;;;;;;;;;;;;

(defun netz-connect-nodes (source target edge-params graph)
  "add edge with `edge-params' connecting `source' and `target'"
  (let ((nodes (car graph))
	(edges (cadr graph))
	(edge `(,(plist-get source :id) ,(plist-get target :id))))
    ;; store edge id in edge value plist too, lazy but it's easier later to dump the full value
    (with-update-cache
     (netz-add-edge-no-save (plist-put edge-params :id edge) graph)
     (netz-add-node-no-save (netz-add-edge-to-node target edge) graph)
     (netz-add-node-no-save (netz-add-edge-to-node source edge) graph))))

(defun netz-connect-nodes-no-save (source target edge-params graph)
  "add edge with `edge-params' connecting `source' and `target'"
  (let ((nodes (car graph))
	(edges (cadr graph))
	(edge `(,(plist-get source :id) ,(plist-get target :id))))
    ;; store edge id in edge value plist too, lazy but it's easier later to dump the full value
    (netz-add-edge-no-save (plist-put edge-params :id edge) graph)
    (netz-add-node-no-save (netz-add-edge-to-node target edge) graph)
    (netz-add-node-no-save (netz-add-edge-to-node source edge) graph)))

(defun netz-get-node-hood (id graph)
  (let* ((source-node (netz-get-node id graph))
	 (edges (netz-get-edges-hash-for-node
		 source-node
		 graph))
	 (nodes (ht-select-keys
		 (car graph)
		 (netz-flatten (ht-keys edges)))))
    (list nodes edges)))

(defun netz-get-edges-hash-for-node (node graph)
  (let ((edge-keys (plist-get node :edges))
	(edges (cadr graph)))
    (ht-select-keys edges edge-keys)))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; utilities
;;;;;;;;;;;;;;;;;;;;;;;;

(defun netz-flatten (list-of-lists)
  (delete-dups (apply #'append list-of-lists)))

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

(provide 'netz)
