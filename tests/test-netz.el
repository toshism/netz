;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'netz)

(describe "Netz"
  (before-all (setq *netz-graph-store* "/tmp/netz-graph-test"))
  (after-each (delete-file "/tmp/netz-graph-test"))
  (after-each (setq *netz-graph* nil))

  (describe "cache"
    (it "initializes cache file"
      (netz-init-graph)
      (expect (file-exists-p *netz-graph-store*)))
    (it "save and reload of graph to cache file"
      (netz-init-graph)
      (setq *netz-graph* '(:id 1))
      (netz-save-graph)
      (setq *netz-graph* nil)
      (expect *netz-graph* :to-equal nil)
      (netz-load-graph)
      (expect *netz-graph* :to-equal '(:id 1))))

  (describe "node management"
    :var ((valid-node '(:id 1 :name "one" :edges ((1 2) (1 3))))
	  (invalid-node '(:name "one")))
    (before-each (netz-init-graph))

    (it "netz-add-node requires :id"
      (expect (netz-add-node invalid-node *netz-graph*) :to-throw))
    (it "basic netz-add-node with :id"
      (expect (netz-add-node valid-node *netz-graph*) :not :to-throw))
    (it "netz-add-node updates cache"
      (netz-add-node valid-node *netz-graph*)
      (setq *netz-graph* nil)
      (netz-load-graph)
      (expect (netz-get-node 1 *netz-graph*) :to-equal valid-node))
    (it "netz-add-node-no-save adds node in memory"
      (netz-add-node-no-save valid-node *netz-graph*)
      (expect (netz-get-node 1 *netz-graph*) :to-equal valid-node))
    (it "netz-add-node-no-save does not update cache"
      (netz-add-node-no-save valid-node *netz-graph*)
      (setq *netz-graph* nil)
      (netz-load-graph)
      (expect (netz-get-node 1 *netz-graph*) :to-equal nil))
    (it "netz-add-edge-to-node adds new edge"
      (netz-add-node-no-save '(:id 1 :name "one") *netz-graph*)
      (netz-add-edge-to-node (netz-get-node 1 *netz-graph*) '(1 2))
      (expect (netz-get-node 1 *netz-graph*) :to-equal '(:id 1 :name "one" :edges ((1 2)))))
    (it "netz-add-edge-to-node does not add duplicate edges"
      (netz-add-node-no-save '(:id 1 :name "one" :edges ((1 2) (1 3))) *netz-graph*)
      (netz-add-edge-to-node (netz-get-node 1 *netz-graph*) '(1 3))
      (expect (netz-get-node 1 *netz-graph*) :to-equal '(:id 1 :name "one" :edges ((1 2) (1 3))))
      (netz-add-edge-to-node (netz-get-node 1 *netz-graph*) '(1 2))
      (expect (netz-get-node 1 *netz-graph*) :to-equal '(:id 1 :name "one" :edges ((1 2) (1 3)))))
    (it "netz-add-edge-to-node order matters in edges"
      (netz-add-node-no-save '(:id 1 :name "one" :edges ((1 3))) *netz-graph*)
      (netz-add-edge-to-node (netz-get-node 1 *netz-graph*) '(3 1))
      (expect (netz-get-node 1 *netz-graph*) :to-equal '(:id 1 :name "one" :edges ((3 1) (1 3))))))

  (describe "edge management"
    :var ((invalid-edge '(:type "RELATED_TO"))
	  (valid-edge '(:id (1 2) :type "RELATED_TO")))
    (before-each (netz-init-graph))

    (it "netz-add-edge requires :id"
      (expect (netz-add-edge invalid-edge *netz-graph*) :to-throw))
    (it "netz-add-edge with :id"
      (expect (netz-add-edge valid-edge *netz-graph*) :not :to-throw))
    (it "netz-add-edge updates cache"
      (netz-add-edge valid-edge *netz-graph*)
      (setq *netz-graph* nil)
      (netz-load-graph)
      (expect (netz-get-edge '(1 2) *netz-graph*) :to-equal valid-edge))
    (it "netz-add-edge-no-save adds edge in memory"
      (netz-add-edge-no-save valid-edge *netz-graph*)
      (expect (netz-get-edge '(1 2) *netz-graph*) :to-equal valid-edge))
    (it "netz-add-edge-no-save does not update cache"
      (netz-add-edge-no-save valid-edge *netz-graph*)
      (setq *netz-graph* nil)
      (netz-load-graph)
      (expect (netz-get-edge '(1 2) *netz-graph*) :to-equal nil)))

  (describe "relationships"
    (before-each (netz-init-graph))

    (it "netz-connect-nodes doesn't throw"
      (expect (netz-connect-nodes '(:id 1) '(:id 2) '(:type "R") *netz-graph*) :not :to-throw))
    (it "netz-connect-nodes creates edge"
      (netz-connect-nodes '(:id 1) '(:id 2) '(:type "R") *netz-graph*)
      (expect (netz-get-edge '(1 2) *netz-graph*) :to-equal '(:type "R" :id (1 2))))
    (it "netz-connect-nodes updates node edges"
      (netz-connect-nodes '(:id 1) '(:id 2) '(:type "R") *netz-graph*)
      (expect (netz-get-node 1 *netz-graph*) :to-equal '(:id 1 :edges ((1 2))))
      (expect (netz-get-node 2 *netz-graph*) :to-equal '(:id 2 :edges ((1 2)))))
    (it "netz-connect-nodes handles adding additional edges"
      (netz-connect-nodes '(:id 3 :edges ((3 1))) '(:id 2) '(:type "R") *netz-graph*)
      (expect (netz-get-node 3 *netz-graph*) :to-equal '(:id 3 :edges ((3 2) (3 1)))))
    (it "netz-delete-node updates node relationships"
      (netz-add-node '(:id 1) *netz-graph*)
      (netz-add-node '(:id 2) *netz-graph*)
      (netz-add-node '(:id 3) *netz-graph*)
      (netz-add-node '(:id 4) *netz-graph*)

      (netz-connect-nodes
       (netz-get-node 1 *netz-graph*)
       (netz-get-node 2 *netz-graph*)
       '(:type "R")
       *netz-graph*)

      (netz-connect-nodes
       (netz-get-node 1 *netz-graph*)
       (netz-get-node 4 *netz-graph*)
       '(:type "R")
       *netz-graph*)

      (netz-connect-nodes
       (netz-get-node 2 *netz-graph*)
       (netz-get-node 3 *netz-graph*)
       '(:type "R")
       *netz-graph*)

      ;; sanity checks
      (expect (netz-get-node 1 *netz-graph*) :to-equal '(:id 1 :edges ((1 4) (1 2))))
      (expect (netz-get-node 2 *netz-graph*) :to-equal '(:id 2 :edges ((2 3) (1 2))))
      (expect (netz-get-node 4 *netz-graph*) :to-equal '(:id 4 :edges ((1 4))))

      (netz-delete-node (netz-get-node 1 *netz-graph*) *netz-graph*)

      (expect (netz-get-node 2 *netz-graph*) :to-equal '(:id 2 :edges ((2 3))))
      (expect (netz-get-node 4 *netz-graph*) :to-equal '(:id 4 :edges nil))))

  (describe "bulk tests"
    (before-each (netz-init-graph))

    (it "add 10,000 nodes"
      (dotimes (i 10000)
	(netz-add-node-no-save `(:id ,i) *netz-graph*))
      (expect (hash-table-count (car *netz-graph*)) :to-equal 10000))
    (it "add, save, and load 10,000 nodes"
      (dotimes (i 10000)
	(netz-add-node-no-save `(:id ,i) *netz-graph*))
      (netz-save-graph)
      (setq *netz-graph* nil)
      (netz-load-graph)
      (expect (netz-get-node 888 *netz-graph*) :to-equal '(:id 888)))
    (it "add 10,000 nodes and 9,999 edges"
      (dotimes (i 10000)
	(netz-add-node-no-save `(:id ,i) *netz-graph*))
      (dotimes (i 9999)
	(netz-connect-nodes-no-save
	 (netz-get-node i *netz-graph*)
	 (netz-get-node
	  (+ 1 i)
	  *netz-graph*)
	 '(:type "R")
	 *netz-graph*)))
    (it "add 10,000 nodes and 9,999 edges; save and load"
      (dotimes (i 10000)
	(netz-add-node-no-save `(:id ,i) *netz-graph*))
      (dotimes (i 9999)
	(netz-connect-nodes-no-save
	 (netz-get-node i *netz-graph*)
	 (netz-get-node
	  (+ 1 i)
	  *netz-graph*)
	 '(:type "R")
	 *netz-graph*))

      (netz-save-graph)
      (setq *netz-graph* nil)
      (netz-load-graph)

      (expect (hash-table-count (car *netz-graph*)) :to-equal 10000)
      (expect (hash-table-count (cadr *netz-graph*)) :to-equal 9999)
      (expect (netz-get-node 5555 *netz-graph*) :to-equal '(:id 5555 :edges ((5555 5556) (5554 5555)))))))
