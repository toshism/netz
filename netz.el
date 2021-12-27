(require 'cl-lib)
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


(defun netz-make-graph (name &optional path save)
  (let* ((path (netz-get-path name path))
	 (graph `(:name ,name
			:path ,path
			:nodes ,(ht-create 'equal)
			:edges ,(ht-create 'equal))))
    (ht-set! *netz-graphs* name graph)
    (when save
      (netz-save-graph name))
    graph))

(defun netz-save-graph (graph)
  (with-graph graph
	      (make-directory (file-name-directory (plist-get graph :path)) t)
	      (ht-set! *netz-graphs* (plist-get graph :name) graph)
	      (with-temp-buffer
		(prin1 graph (current-buffer))
		(write-file (plist-get graph :path) nil))
	      graph))

(defun netz-load-graph (path)
  (if (file-exists-p path)
      (let* ((graph (read (netz-file-to-string path))))
	(ht-set! *netz-graphs* (plist-get graph :name) graph)
	graph)
    (error "File not found")))

(defun netz-reload-graph (graph)
  (with-graph graph
	      (netz-load-graph (plist-get graph :path))))

(defun netz-get-graph (name)
  "get graph from cache"
  (ht-get *netz-graphs* name))

(defun netz-copy-graph (graph new-name &optional new-path)
  (with-graph graph
	      (let ((new-graph (netz-make-graph new-name new-path)))
		(plist-put new-graph :nodes (netz-get-nodes graph))
		(plist-put new-graph :edges (netz-get-edges graph))
		new-graph)))

(defun netz-file-to-string (file)
  "File to string function"
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun netz-get-path (name &optional path)
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

;;;;;;;;;;;;;;;;;;;;;;;;
;;; node mangement
;;;;;;;;;;;;;;;;;;;;;;;;
;; creating, adding, modifying, etc

(defun netz-add-node (node graph)
  "Add `node' to `graph'.
If node already exists it will be merged with new values.
see `netz-merge-plists' for details."
  (unless (plist-get node :id)
    (error "Node must include an :id parameter"))
  (with-nodes graph
	      (ht-set!
	       nodes
	       (plist-get node :id)
	       (netz-merge-plists (netz-get-node (plist-get node :id) graph) node))
	      graph))

(defun netz-get-node (node-id graph)
  (with-nodes graph
	      (ht-get nodes node-id)))

(defun netz-get-nodes (graph)
  (with-nodes graph
	      nodes))

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

(defun netz-add-edge-to-node (node edge)
  "get current edges from `node', if `edge' doesn't exist
add it to existing list of edges"
  (let ((edges (plist-get node :edges)))
    (if (member edge edges)
	node
      (plist-put node :edges (cons edge (plist-get node :edges))))))

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
  (with-graph graph
	      (let ((edge-id (plist-get edge :id)))
		(netz-add-node `(:id ,(car edge-id)) graph)
		(netz-add-node `(:id ,(cadr edge-id)) graph)
		(netz-connect-nodes
		 (netz-get-node (car edge-id) graph)
		 (netz-get-node (cadr edge-id) graph)
		 edge
		 graph))))

(defun netz-get-edge (edge-id graph)
  (with-edges graph
	      (ht-get edges edge-id)))

(defun netz-get-edges (graph)
  (with-edges graph
	      edges))

(defun netz-delete-edge (edge graph)
  "delete edge and clean up node edges"
  (with-edges graph
	      (let ((edge-id (plist-get edge :id)))
		(ht-remove! edges edge-id)
		(netz--delete-edge-from-node-edges (car edge-id) edge-id graph)
		(netz--delete-edge-from-node-edges (cadr edge-id) edge-id graph))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; relationships
;;;;;;;;;;;;;;;;;;;;;;;;

(defun netz-connect-nodes (source target edge-params graph)
  "add edge with `edge-params' connecting `source' and `target'"
  (with-nodes-edges graph
		    (let ((edge `(,(plist-get source :id) ,(plist-get target :id))))
		      (ht-set! edges edge (plist-put edge-params :id edge))
		      ;; (netz-add-edge (plist-put edge-params :id edge) graph)
		      (netz-add-node (netz-add-edge-to-node target edge) graph)
		      (netz-add-node (netz-add-edge-to-node source edge) graph))))

;; (cl-defun netz-get-node-hood (id graph &key new-graph-name edge-filter node-filter)
;; then use ht-reject! for filtering if new-graph-name is nil
(defun netz-get-node-hood (id graph &optional new-graph edge-filter directed)
  "edge-filter = '(key value)"
  (with-graph graph
	      (let* ((source-node (netz-get-node id graph))
		     (new-graph (if new-graph
				    (netz-make-graph new-graph)
				  graph))
		     (edges (if directed
				(netz--filter-edges-directed id (netz-get-edges-hash-for-node
								 source-node
								 graph
								 edge-filter))
			      (netz-get-edges-hash-for-node
			       source-node
			       graph
			       edge-filter)))
		     (nodes (ht-select-keys
			     (netz-get-nodes graph)
			     (delete-dups (-flatten (ht-keys edges))))))
		(plist-put new-graph :nodes nodes)
		(plist-put new-graph :edges edges)
		new-graph)))

;; TODO this works, but surely there is a cleaner way
(defun netz-bfs-shortest-path (source target graph &optional directed)
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
	    (let ((neighbors (netz-node-neighbors (netz-get-node node graph) directed)))
	      (dolist (neighbor neighbors)
		(let ((new-path path))
		  (setq new-path (-snoc new-path neighbor))
		  (setq queue (-snoc queue new-path))
		  (when (equal neighbor target-id)
		    (setq result new-path)
		    (throw 'found result))))
	      (setq visited (-snoc visited node)))))))))

;; just a trial, not sure how i want this yet
(cl-defun netz-get-related-by (node graph &key by new-name directed)
  "return graph related to `node' by edge containing `by' properties"
  (with-graph graph
	      (netz-get-node-hood (plist-get node :id) graph new-name by directed)))

(defun netz--filter-edges-directed (node-id edges)
  (ht-reject! (lambda (key edge)
		(not (equal node-id (car key)))) edges)
  edges)

(defun netz-get-edges-hash-for-node (node graph &optional filter)
  (with-graph graph
	      (let* ((edge-keys (plist-get node :edges))
		     (edges (if filter
				(netz-filter-edges (car filter) (cadr filter) graph)
			      (netz-get-edges graph))))
		(ht-select-keys edges edge-keys))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; utilities
;;;;;;;;;;;;;;;;;;;;;;;;

(defun netz-node-ids-to-graph (ids old-graph new-name)
  "takes a list of `ids' and returns a new graph
containing the nodes and related edges."
  (let ((new-graph (netz-make-graph new-name))
	(old-graph (netz-get-graph old-graph)))
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

;; stolen from org-mode
(defun netz-merge-plists (&rest plists)
  "Create a single property list from all plists in PLISTS.
The process starts by copying the first list, and then setting properties
from the other lists.  Settings in the last list are the most significant
ones and overrule settings in the other lists."
  (let ((rtn (copy-sequence (pop plists)))
        p v ls)
    (while plists
      (setq ls (pop plists))
      (while ls
        (setq p (pop ls) v (pop ls))
        (setq rtn (plist-put rtn p v))))
    rtn))

(defun netz-node-neighbors (node &optional directed)
  (let ((edges (if directed
		   (-filter (lambda (edge)
			      (equal (car edge) (plist-get node :id)))
			    (plist-get node :edges))
		 (plist-get node :edges))))
    (delete-dups (remove (plist-get node :id) (-flatten edges)))))

(defun netz-filter-edges (k v graph)
  (with-graph graph
	      (ht-select (lambda (key value)
			   (equal (plist-get value k) v))
			 (netz-get-edges graph))))

(defun netz-filter-graph-edges (k v graph)
  (with-graph graph
	      (plist-put graph :edges (netz-filter-edges k v graph))
	      graph))

(defun netz-filter-graph-nodes (k v graph)
  (with-graph graph
	      (plist-put graph :nodes (netz-filter-nodes k v graph))
	      graph))

(defun netz-filter-nodes (k v graph)
  (with-graph graph
	      (ht-select (lambda (key value)
			   (equal (plist-get value k) v))
			 (netz-get-nodes graph))))

(defmacro with-graph (graph &rest body)
  `(let ((,graph (if (not (consp ,graph))
		     (netz-get-graph ,graph)
		   ,graph)))
     ,@body))

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
