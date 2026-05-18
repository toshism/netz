;;; netz.el --- Emacs generic graph store            -*- lexical-binding: t; -*-

;; Version: 0.1
;; Package-Requires: ((emacs "26.3") (ht "2.4") (dash "2.19"))
;; Keywords: tools, maint

;;; Commentary:
;; Generic directed property graph store for Emacs Lisp.

;;; Code:

(require 'cl-lib)
(require 'ht)
(require 'dash)

(defvar *netz-graph-store* (concat user-emacs-directory ".netz-graph"))
(defvar *netz-graphs* (ht-create 'equal))

(cl-defstruct netz-graph
  name
  path
  nodes
  edges
  out-index
  in-index)

(defun netz--keyword-name (name)
  (cond ((keywordp name) (substring (symbol-name name) 1))
        ((symbolp name) (symbol-name name))
        ((numberp name) (number-to-string name))
        ((stringp name) name)
        (t (error "Name must be a string, symbol, keyword, or number"))))

(defun netz-get-path (name &optional path)
  (or path (concat (file-name-as-directory *netz-graph-store*)
                   (netz--keyword-name name))))

(defun netz--coerce-graph (graph)
  (cond ((netz-graph-p graph) graph)
        ((or (keywordp graph) (symbolp graph) (stringp graph) (numberp graph))
         (or (ht-get *netz-graphs* graph)
             (ht-get *netz-graphs* (intern (format ":%s" (netz--keyword-name graph))))
             (ht-get *netz-graphs* (netz--keyword-name graph))
             (error "Graph not found: %S" graph)))
        (t (error "Invalid graph: %S" graph))))

(cl-defun netz-create-graph (name &key path save)
  "Create and register an empty graph named NAME."
  (let ((graph (make-netz-graph :name name
                                :path (netz-get-path name path)
                                :nodes (ht-create 'equal)
                                :edges (ht-create 'equal)
                                :out-index (ht-create 'equal)
                                :in-index (ht-create 'equal))))
    (ht-set! *netz-graphs* name graph)
    (when save (netz-save-graph graph))
    graph))

(defalias 'netz-make-graph #'netz-create-graph)

(defun netz-get-graph (name)
  "Return registered graph NAME."
  (ht-get *netz-graphs* name))

(defun netz-graph-node-count (graph)
  (ht-size (netz-graph-nodes (netz--coerce-graph graph))))

(defun netz-graph-edge-count (graph)
  (ht-size (netz-graph-edges (netz--coerce-graph graph))))

(defun netz--ht-values (table)
  (let (values)
    (maphash (lambda (_ value) (push value values)) table)
    (nreverse values)))

(defun netz-prop (entity prop)
  "Return PROP from plist ENTITY."
  (plist-get entity prop))

(defun netz-merge-plists (&rest plists)
  "Merge PLISTS with later values winning."
  (let ((result (copy-sequence (or (pop plists) nil))))
    (dolist (plist plists result)
      (while plist
        (setq result (plist-put result (pop plist) (pop plist)))))))

(defun netz-add-node (graph node)
  "Add NODE plist to GRAPH, merging if the id already exists."
  (setq graph (netz--coerce-graph graph))
  (let ((id (plist-get node :id)))
    (unless id (error "Node must include an :id parameter"))
    (ht-set! (netz-graph-nodes graph)
             id
             (netz-merge-plists (netz-get-node graph id) node))
    graph))

(defun netz-get-node (graph node-id)
  "Return node NODE-ID from GRAPH."
  (ht-get (netz-graph-nodes (netz--coerce-graph graph)) node-id))

(defun netz-get-nodes (graph)
  "Return all node plists from GRAPH."
  (netz--ht-values (netz-graph-nodes (netz--coerce-graph graph))))

(defun netz--index-add (index node-id edge-id)
  (ht-set! index node-id (delete-dups (cons edge-id (ht-get index node-id)))))

(defun netz--index-remove (index node-id edge-id)
  (let ((remaining (remove edge-id (ht-get index node-id))))
    (if remaining
        (ht-set! index node-id remaining)
      (ht-remove! index node-id))))

(defun netz--remove-edge-from-indexes (graph edge)
  (when edge
    (netz--index-remove (netz-graph-out-index graph) (plist-get edge :source) (plist-get edge :id))
    (netz--index-remove (netz-graph-in-index graph) (plist-get edge :target) (plist-get edge :id))))

(defun netz--add-edge-to-indexes (graph edge)
  (netz--index-add (netz-graph-out-index graph) (plist-get edge :source) (plist-get edge :id))
  (netz--index-add (netz-graph-in-index graph) (plist-get edge :target) (plist-get edge :id)))

(defun netz-add-edge (graph edge)
  "Add directed EDGE plist to GRAPH.
EDGE must include :id, :source, and :target."
  (setq graph (netz--coerce-graph graph))
  (let ((id (plist-get edge :id))
        (source (plist-get edge :source))
        (target (plist-get edge :target)))
    (unless id (error "Edge must include an :id parameter"))
    (unless source (error "Edge must include a :source parameter"))
    (unless target (error "Edge must include a :target parameter"))
    (unless (netz-get-node graph source) (netz-add-node graph `(:id ,source)))
    (unless (netz-get-node graph target) (netz-add-node graph `(:id ,target)))
    (netz--remove-edge-from-indexes graph (netz-get-edge graph id))
    (ht-set! (netz-graph-edges graph) id edge)
    (netz--add-edge-to-indexes graph edge)
    graph))

(defun netz-get-edge (graph edge-id)
  "Return edge EDGE-ID from GRAPH."
  (ht-get (netz-graph-edges (netz--coerce-graph graph)) edge-id))

(defun netz-get-edges (graph)
  "Return all edge plists from GRAPH."
  (netz--ht-values (netz-graph-edges (netz--coerce-graph graph))))

(defun netz--valid-direction-p (direction)
  (memq direction '(:out :in :any)))

(cl-defun netz-node-edge-ids (graph node-id &key (direction :any))
  "Return edge ids incident to NODE-ID in GRAPH for DIRECTION."
  (setq graph (netz--coerce-graph graph))
  (unless (netz--valid-direction-p direction)
    (error "Invalid edge direction: %S" direction))
  (pcase direction
    (:out (copy-sequence (ht-get (netz-graph-out-index graph) node-id)))
    (:in (copy-sequence (ht-get (netz-graph-in-index graph) node-id)))
    (:any (delete-dups (append (netz-node-edge-ids graph node-id :direction :out)
                               (netz-node-edge-ids graph node-id :direction :in))))))

(defun netz-delete-edge (graph edge-or-id)
  "Delete EDGE-OR-ID from GRAPH and update indexes."
  (setq graph (netz--coerce-graph graph))
  (let* ((edge-id (if (consp edge-or-id) (plist-get edge-or-id :id) edge-or-id))
         (edge (netz-get-edge graph edge-id)))
    (when edge
      (netz--remove-edge-from-indexes graph edge)
      (ht-remove! (netz-graph-edges graph) edge-id))
    graph))

(cl-defun netz-delete-node (graph node-or-id &key detach)
  "Delete NODE-OR-ID from GRAPH.  With DETACH, delete incident edges first."
  (setq graph (netz--coerce-graph graph))
  (let* ((node-id (if (consp node-or-id) (plist-get node-or-id :id) node-or-id))
         (incident (netz-node-edge-ids graph node-id :direction :any)))
    (when (and incident (not detach))
      (error "Cannot delete node with incident edges without :detach"))
    (dolist (edge-id incident)
      (netz-delete-edge graph edge-id))
    (ht-remove! (netz-graph-nodes graph) node-id)
    graph))

(defun netz--rebuild-indexes (graph)
  (setf (netz-graph-out-index graph) (ht-create 'equal)
        (netz-graph-in-index graph) (ht-create 'equal))
  (dolist (edge (netz-get-edges graph))
    (netz--add-edge-to-indexes graph edge))
  graph)

(defun netz-save-graph (graph)
  "Persist GRAPH to its path."
  (setq graph (netz--coerce-graph graph))
  (make-directory (file-name-directory (netz-graph-path graph)) t)
  (with-temp-file (netz-graph-path graph)
    (prin1 graph (current-buffer)))
  (ht-set! *netz-graphs* (netz-graph-name graph) graph)
  graph)

(defun netz-load-graph (path)
  "Load graph from PATH and register it."
  (unless (file-exists-p path)
    (error "File not found"))
  (let ((graph (with-temp-buffer
                 (insert-file-contents path)
                 (read (current-buffer)))))
    (unless (netz-graph-p graph)
      (error "File does not contain a netz graph"))
    (unless (netz-graph-out-index graph) (setf (netz-graph-out-index graph) (ht-create 'equal)))
    (unless (netz-graph-in-index graph) (setf (netz-graph-in-index graph) (ht-create 'equal)))
    (netz--rebuild-indexes graph)
    (ht-set! *netz-graphs* (netz-graph-name graph) graph)
    graph))

(defun netz-reload-graph (graph)
  (netz-load-graph (netz-graph-path (netz--coerce-graph graph))))

(defun netz-copy-graph (graph new-name &optional new-path)
  "Copy GRAPH into a newly registered graph NEW-NAME."
  (setq graph (netz--coerce-graph graph))
  (let ((copy (netz-create-graph new-name :path new-path)))
    (dolist (node (netz-get-nodes graph)) (netz-add-node copy (copy-sequence node)))
    (dolist (edge (netz-get-edges graph)) (netz-add-edge copy (copy-sequence edge)))
    copy))

(provide 'netz)

;;; netz.el ends here
