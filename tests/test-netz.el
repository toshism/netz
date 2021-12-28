;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'netz)

(describe "Netz"
  (before-all (setq *netz-graph-store* "/tmp/netz-graph-test"))
  (after-all (delete-file "/tmp/netz-graph-test/test"))
  (after-each (setq *netz-graphs* (ht-create 'equal)))
  (after-each (setq test nil))

  (describe "graph management"
    (it "netz-make-graph creates graph"
      (expect (netz-make-graph :test) :not :to-throw))
    (it "supports names as string"
      (expect (netz-make-graph "test") :not :to-throw)
      (expect (plist-get (netz-get-graph "test") :name) :to-equal "test"))
    (it "supports name as string in variable"
      (setq test-name "test")
      (expect (netz-make-graph test-name) :not :to-throw)
      (expect (plist-get (netz-get-graph "test") :name) :to-equal "test"))
    (it "netz-make-graph initializes save file"
      (expect (file-exists-p (plist-get (netz-make-graph :test nil t) :path))))
    (it "netz-make-graph creates proper graph data structure"
      (setq test (netz-make-graph :test))
      (expect (plist-get test :name) :to-be :test)
      (expect (stringp (plist-get test :path)))
      (expect (hash-table-p (plist-get test :nodes)))
      (expect (hash-table-p (plist-get test :edges))))
    (it "netz-make-graph adds graph to cache hash"
      (setq test (netz-make-graph :test))
      (expect (ht-get *netz-graphs* :test) :to-equal test))
    (it "can save and load graph to file"
      (setq test (netz-make-graph :test))
      (netz-save-graph :test)
      (ht-clear *netz-graphs*)
      (netz-load-graph (plist-get test :path))
      (expect (plist-get (ht-get *netz-graphs* :test) :path) :to-equal (plist-get test :path))))

  (describe "node management"
    (before-each (netz-make-graph :test))

    (it "netz-add-node requires :id"
      (expect (netz-add-node '(:name "one") :test) :to-throw))
    (it "basic netz-add-node with :id"
      (expect (netz-add-node '(:id 1) :test) :not :to-throw))
    (it "can get node by id"
      (netz-add-node '(:id 1) :test)
      (expect (netz-get-node 1 :test) :to-equal '(:id 1)))
    (it "netz-add-edge-to-node adds new edge"
      (netz-add-node '(:id 1 :name "one") :test)
      (netz-add-edge-to-node (netz-get-node 1 :test) '(1 2))
      (expect (netz-get-node 1 :test) :to-equal '(:id 1 :name "one" :edges ((1 2)))))
    (it "netz-add-edge-to-node does not add duplicate edges"
      (netz-add-node '(:id 1 :name "one" :edges ((1 2) (1 3))) :test)
      (netz-add-edge-to-node (netz-get-node 1 :test) '(1 3))
      (expect (netz-get-node 1 :test) :to-equal '(:id 1 :name "one" :edges ((1 2) (1 3))))
      (netz-add-edge-to-node (netz-get-node 1 :test) '(1 2))
      (expect (netz-get-node 1 :test) :to-equal '(:id 1 :name "one" :edges ((1 2) (1 3)))))
    (it "netz-add-edge-to-node order matters in edges"
      (netz-add-node '(:id 1 :name "one" :edges ((1 3))) :test)
      (netz-add-edge-to-node (netz-get-node 1 :test) '(3 1))
      (expect (netz-get-node 1 :test) :to-equal '(:id 1 :name "one" :edges ((3 1) (1 3)))))
    (it "netz-add-node merges data from node and existing node"
      (netz-add-node '(:id 1 :name "one" :label "A") :test)
      (netz-add-node '(:id 1 :name "two") :test)
      (expect (netz-get-node 1 :test) :to-equal '(:id 1 :name "two" :label "A"))))

  (describe "edge management"
    :var ((invalid-edge '(:type "RELATED_TO"))
	  (valid-edge '(:id (1 2) :type "RELATED_TO")))
    (before-each (netz-make-graph :test))

    (it "netz-add-edge requires :id"
      (expect (netz-add-edge invalid-edge :test) :to-throw))
    (it "netz-add-edge with :id"
      (expect (netz-add-edge valid-edge :test) :not :to-throw))
    (it "netz-add-edge adds edge"
      (netz-add-edge valid-edge :test)
      (expect (netz-get-edge '(1 2) :test) :to-equal valid-edge))
    (it "netz-delete-edge deletes edges from nodes"
      (netz-connect-nodes '(:id 1) '(:id 2) '(:type "R") :test)
      (expect (netz-get-node 1 :test) :to-equal '(:id 1 :edges ((1 2))))
      (expect (netz-get-node 2 :test) :to-equal '(:id 2 :edges ((1 2))))
      (netz-delete-edge (netz-get-edge '(1 2) :test) :test)
      (expect (netz-get-edge '(1 2) :test) :to-equal nil)
      (expect (netz-get-node 1 :test) :to-equal '(:id 1 :edges nil))
      (expect (netz-get-node 2 :test) :to-equal '(:id 2 :edges nil))))

  (describe "relationships"
    (before-each (netz-make-graph :test))

    (it "netz-connect-nodes doesn't throw"
      (expect (netz-connect-nodes '(:id 1) '(:id 2) '(:type "R") :test) :not :to-throw))
    (it "netz-connect-nodes creates edge"
      (netz-connect-nodes '(:id 1) '(:id 2) '(:type "R") :test)
      (expect (netz-get-edge '(1 2) :test) :to-equal '(:type "R" :id (1 2))))
    (it "netz-connect-nodes existing nodes"
      (netz-add-node '(:id 1) :test)
      (netz-add-node '(:id 2) :test)
      (expect (netz-connect-nodes
	       (netz-get-node 1 :test)
	       (netz-get-node 2 :test)
	       '(:type "R") :test)
	      :not :to-throw))
    (it "netz-connect-nodes updates node edges"
      (netz-connect-nodes '(:id 1) '(:id 2) '(:type "R") :test)
      (expect (netz-get-node 1 :test) :to-equal '(:id 1 :edges ((1 2))))
      (expect (netz-get-node 2 :test) :to-equal '(:id 2 :edges ((1 2)))))
    (it "netz-connect-nodes handles existing edges"
      (netz-connect-nodes '(:id 1) '(:id 2) '(:type "R") :test)
      (expect (netz-connect-nodes '(:id 1) '(:id 2) '(:type "R") :test) :not :to-throw))
    (it "netz-connect-nodes handles adding additional edges"
      (netz-connect-nodes '(:id 3 :edges ((3 1))) '(:id 2) '(:type "R") :test)
      (expect (netz-get-node 3 :test) :to-equal '(:id 3 :edges ((3 2) (3 1)))))
    (it "netz-delete-node updates node relationships"
      (netz-add-node '(:id 1) :test)
      (netz-add-node '(:id 2) :test)
      (netz-add-node '(:id 3) :test)
      (netz-add-node '(:id 4) :test)

      (netz-connect-nodes
       (netz-get-node 1 :test)
       (netz-get-node 2 :test)
       '(:type "R")
       :test)

      (netz-connect-nodes
       (netz-get-node 1 :test)
       (netz-get-node 4 :test)
       '(:type "R")
       :test)

      (netz-connect-nodes
       (netz-get-node 2 :test)
       (netz-get-node 3 :test)
       '(:type "R")
       :test)

      ;; sanity checks
      (expect (netz-get-node 1 :test) :to-equal '(:id 1 :edges ((1 4) (1 2))))
      (expect (netz-get-node 2 :test) :to-equal '(:id 2 :edges ((2 3) (1 2))))
      (expect (netz-get-node 4 :test) :to-equal '(:id 4 :edges ((1 4))))

      (netz-delete-node (netz-get-node 1 :test) :test)

      (expect (netz-get-node 2 :test) :to-equal '(:id 2 :edges ((2 3))))
      (expect (netz-get-node 4 :test) :to-equal '(:id 4 :edges nil)))
    (it "gets related by undirected edge properties (mutate)"
      (netz-add-node '(:id 1) :test)
      (netz-add-node '(:id 2) :test)
      (netz-add-node '(:id 3) :test)
      (netz-add-node '(:id 4) :test)
      (netz-connect-nodes (netz-get-node 1 :test) (netz-get-node 2 :test) '(:type "A") :test)
      (netz-connect-nodes (netz-get-node 1 :test) (netz-get-node 4 :test) '(:type "A") :test)
      (netz-connect-nodes (netz-get-node 1 :test) (netz-get-node 3 :test) '(:type "B") :test)
      ;; (netz-get-related-by (netz-get-node 1 :test) :test :by '(:type "A"))
      (netz-get-related-by (netz-get-node 1 :test) :test :by (:match :type "A"))
      (expect (netz-get-node 3 :test) :to-be nil)
      (expect (netz-get-node 1 :test) :not :to-be nil)
      (expect (netz-get-node 2 :test) :not :to-be nil)
      (expect (netz-get-node 4 :test) :not :to-be nil)
      (expect (netz-get-edge '(1 3) :test) :to-be nil)
      (expect (netz-get-edge '(1 2) :test) :not :to-be nil)
      (expect (netz-get-edge '(1 4) :test) :not :to-be nil))
    (it "gets related by undirected edge properties (no mutate)"
      (netz-add-node '(:id 1) :test)
      (netz-add-node '(:id 2) :test)
      (netz-add-node '(:id 3) :test)
      (netz-add-node '(:id 4) :test)
      (netz-connect-nodes (netz-get-node 1 :test) (netz-get-node 2 :test) '(:type "A") :test)
      (netz-connect-nodes (netz-get-node 1 :test) (netz-get-node 4 :test) '(:type "A") :test)
      (netz-connect-nodes (netz-get-node 1 :test) (netz-get-node 3 :test) '(:type "B") :test)
      (netz-get-related-by (netz-get-node 1 :test) :test :by (:match :type "A") :new-name :test2)

      ;; original graph is not mutated
      (expect (netz-get-node 3 :test) :not :to-be nil)
      (expect (netz-get-edge '(1 3) :test) :not :to-be nil)

      ;; new graph
      (expect (netz-get-node 3 :test2) :to-be nil)
      (expect (netz-get-node 1 :test2) :not :to-be nil)
      (expect (netz-get-node 2 :test2) :not :to-be nil)
      (expect (netz-get-node 4 :test2) :not :to-be nil)
      (expect (netz-get-edge '(1 3) :test2) :to-be nil)
      (expect (netz-get-edge '(1 2) :test2) :not :to-be nil)
      (expect (netz-get-edge '(1 4) :test2) :not :to-be nil))
    (it "gets related by directed edge properties (mutate)"
      (netz-add-node '(:id 1) :test)
      (netz-add-node '(:id 2) :test)
      (netz-add-node '(:id 3) :test)
      (netz-add-node '(:id 4) :test)
      (netz-connect-nodes (netz-get-node 1 :test) (netz-get-node 2 :test) '(:type "A") :test)
      (netz-connect-nodes (netz-get-node 2 :test) (netz-get-node 1 :test) '(:type "A") :test)
      (netz-connect-nodes (netz-get-node 4 :test) (netz-get-node 1 :test) '(:type "A") :test)
      (netz-connect-nodes (netz-get-node 1 :test) (netz-get-node 3 :test) '(:type "B") :test)
      (netz-get-related-by (netz-get-node 1 :test) :test :by (:match :type "A") :directed t)
      (expect (netz-get-node 3 :test) :to-be nil)
      (expect (netz-get-node 1 :test) :not :to-be nil)
      (expect (netz-get-node 2 :test) :not :to-be nil)
      (expect (netz-get-node 4 :test) :to-be nil)
      (expect (netz-get-edge '(1 3) :test) :to-be nil)
      (expect (netz-get-edge '(1 2) :test) :not :to-be nil)
      (expect (netz-get-edge '(1 4) :test) :to-be nil)
      (expect (netz-get-edge '(2 1) :test) :to-be nil)))

  (describe "utilities"
    (before-each (netz-make-graph :test))

    (it "gets edge property"
      (netz-add-node '(:id 1) :test)
      (netz-add-node '(:id 2) :test)
      (netz-connect-nodes
       (netz-get-node 1 :test)
       (netz-get-node 2 :test)
       '(:type "A" :weight 2) :test)
      (expect (netz-get-edge-property '(1 2) :type :test) :to-equal "A")
      (expect (netz-get-edge-property '(1 2) :weight :test) :to-equal 2))
    (it "returns unique neighbor ids"
      (netz-add-node '(:id 1) :test)
      (netz-add-node '(:id 2) :test)
      (netz-add-node '(:id 3) :test)
      (netz-connect-nodes
       (netz-get-node 1 :test)
       (netz-get-node 2 :test)
       '(:type "A" :weight 2) :test)
      (netz-connect-nodes
       (netz-get-node 2 :test)
       (netz-get-node 1 :test)
       '(:type "A" :weight 2) :test)
      (netz-connect-nodes
       (netz-get-node 1 :test)
       (netz-get-node 3 :test)
       '(:type "A" :weight 2) :test)
      (expect (netz-node-neighbors (netz-get-node 1 :test)) :to-equal '(3 2))
      ;; directed
      (netz-connect-nodes
       (netz-get-node 3 :test)
       (netz-get-node 2 :test)
       '(:type "A" :weight 2) :test)
      (expect (netz-node-neighbors (netz-get-node 3 :test) t) :to-equal '(2))
      )
    (it "builds new graph for node ids"
      (netz-add-node '(:id 1) :test)
      (netz-add-node '(:id 2) :test)
      (netz-add-node '(:id 3) :test)
      (netz-add-node '(:id 4) :test)
      (netz-add-node '(:id 5) :test)
      (netz-connect-nodes
       (netz-get-node 1 :test)
       (netz-get-node 2 :test)
       '(:type "A" :weight 2) :test)
      (netz-connect-nodes
       (netz-get-node 2 :test)
       (netz-get-node 1 :test)
       '(:type "A" :weight 2) :test)
      (netz-connect-nodes
       (netz-get-node 1 :test)
       (netz-get-node 3 :test)
       '(:type "A" :weight 2) :test)
      (netz-connect-nodes
       (netz-get-node 2 :test)
       (netz-get-node 3 :test)
       '(:type "A" :weight 2) :test)
      (netz-connect-nodes
       (netz-get-node 1 :test)
       (netz-get-node 4 :test)
       '(:type "A" :weight 2) :test)
      (setq new-graph (netz-node-ids-to-graph '(1 2 4 5) :test :new-graph))
      (expect (ht-keys (plist-get new-graph :nodes)) :to-have-same-items-as '(1 2 4 5))
      (expect (ht-keys (plist-get new-graph :edges)) :to-have-same-items-as '((1 2) (2 1) (1 4)))))

  (describe "filtering nodes"
    (before-each (netz-make-graph :test))

    (it "supports single :match statement for nodes"
      (netz-add-node '(:id 1 :label "Note" :name "one") :test)
      (netz-add-node '(:id 2 :label "Note" :name "two") :test)
      (expect (ht-keys (netz-filter-nodes (:match :name "one") :test)) :to-equal '(1))
      (expect (ht-keys
	       (netz-filter-nodes (:match :label "Note") :test)) :to-have-same-items-as '(1 2)))
    (it "supports :and'ed :match statements for nodes"
      (netz-add-node '(:id 1 :label "Note" :name "one" :type "A") :test)
      (netz-add-node '(:id 2 :label "Note" :name "two" :type "A") :test)
      (netz-add-node '(:id 3 :label "Tag" :name "one" :type "A") :test)
      (netz-add-node '(:id 4 :label "Tag" :name "two") :test)
      (expect (ht-keys
	       (netz-filter-nodes (:and (:match :name "one")
					(:match :label "Tag")) :test))
	      :to-equal '(3))
      (expect (ht-keys (netz-filter-nodes (:and (:match :name "one")
						(:match :type "A")) :test))
	      :to-have-same-items-as '(1 3))
      (expect (ht-keys (netz-filter-nodes (:and (:match :name "one")
						(:match :type "A")
						(:match :label "Note")) :test))
	      :to-have-same-items-as '(1)))
    (it "supports :or'ed :match statements for nodes"
      (netz-add-node '(:id 1 :label "Note" :name "one" :type "A") :test)
      (netz-add-node '(:id 2 :label "Note" :name "two" :type "A") :test)
      (netz-add-node '(:id 3 :label "Tag" :name "one" :type "A") :test)
      (netz-add-node '(:id 4 :label "Tag" :name "two") :test)
      (expect (ht-keys
	       (netz-filter-nodes (:or (:match :name "one")
				       (:match :label "Tag")) :test))
	      :to-have-same-items-as '(1 3 4))
      (expect (ht-keys (netz-filter-nodes (:or (:match :name "one")
					       (:match :type "A")) :test))
	      :to-have-same-items-as '(1 2 3))))

  (describe "filtering edges"
    (before-each (netz-make-graph :test))

    (it "supports single :match statement for edges"
      (netz-add-node '(:id 1 :label "Note" :name "one" :type "A") :test)
      (netz-add-node '(:id 2 :label "Note" :name "two" :type "A") :test)
      (netz-add-node '(:id 3 :label "Tag" :name "one" :type "A") :test)
      (netz-add-node '(:id 4 :label "Tag" :name "two") :test)
      (netz-add-edge '(:id (1 2) :type "A") :test)
      (netz-add-edge '(:id (2 3) :type "B") :test)
      (netz-add-edge '(:id (4 3) :type "A") :test)
      (expect (ht-keys
	       (netz-filter-edges (:match :type "B") :test)) :to-equal '((2 3)))
      (expect (ht-keys
	       (netz-filter-edges (:match :type "A") :test)) :to-have-same-items-as '((1 2) (4 3))))
    (it "supports :and'ed :match statments for nodes"
      (netz-add-node '(:id 1 :label "Note" :name "one" :type "A") :test)
      (netz-add-node '(:id 2 :label "Note" :name "two" :type "A") :test)
      (netz-add-node '(:id 3 :label "Tag" :name "one" :type "A") :test)
      (netz-add-node '(:id 4 :label "Tag" :name "two") :test)
      (netz-add-edge '(:id (1 2) :type "A" :weight 4) :test)
      (netz-add-edge '(:id (2 3) :type "B" :weight 4) :test)
      (netz-add-edge '(:id (4 3) :type "A" :weight 2) :test)
      (expect (ht-keys
	       (netz-filter-edges (:and (:match :type "A")
					(:match :weight 4)) :test)) :to-equal '((1 2))))
    (it "supports :or'ed :match statments for nodes"
      (netz-add-node '(:id 1 :label "Note" :name "one" :type "A") :test)
      (netz-add-node '(:id 2 :label "Note" :name "two" :type "A") :test)
      (netz-add-node '(:id 3 :label "Tag" :name "one" :type "A") :test)
      (netz-add-node '(:id 4 :label "Tag" :name "two") :test)
      (netz-add-edge '(:id (1 2) :type "A" :weight 4) :test)
      (netz-add-edge '(:id (2 3) :type "B" :weight 4) :test)
      (netz-add-edge '(:id (4 3) :type "A" :weight 2) :test)
      (expect (ht-keys
	       (netz-filter-edges (:or (:match :type "A")
				       (:match :weight 4)) :test))
	      :to-have-same-items-as '((1 2) (2 3) (4 3)))))

  (describe "filtering graphs"
    (before-each (netz-make-graph :test))

    (it "get new graph filtered by edge properties"
      (netz-add-node '(:id 1 :label "Note" :name "one" :type "A") :test)
      (netz-add-node '(:id 2 :label "Note" :name "two" :type "A") :test)
      (netz-add-node '(:id 3 :label "Tag" :name "one" :type "A") :test)
      (netz-add-node '(:id 4 :label "Tag" :name "two") :test)
      (netz-add-node '(:id 5 :label "Tag" :name "five") :test)
      (netz-add-edge '(:id (1 2) :type "A" :weight 4) :test)
      (netz-add-edge '(:id (2 3) :type "B" :weight 4) :test)
      (netz-add-edge '(:id (4 3) :type "A" :weight 2) :test)
      (netz-add-edge '(:id (2 1) :type "C" :weight 8) :test)
      (netz-filtered-graph (:edges (:match :type "C")) :test :test2)
      (expect (ht-keys (netz-get-edges :test2)) :to-have-same-items-as '((2 1)))
      (expect (ht-keys (netz-get-nodes :test2)) :to-have-same-items-as '(1 2))

      (netz-filtered-graph (:edges (:or (:match :type "A")
					(:match :type "B"))) :test :test3)
      (expect (ht-keys (netz-get-edges :test3)) :to-have-same-items-as '((1 2) (2 3) (4 3)))
      (expect (ht-keys (netz-get-nodes :test3)) :to-have-same-items-as '(1 2 3 4)))
    (it "get new graph filtered by node properties"
      (netz-add-node '(:id 1 :label "Note" :name "one" :type "A") :test)
      (netz-add-node '(:id 2 :label "Note" :name "two" :type "A") :test)
      (netz-add-node '(:id 3 :label "Tag" :name "one" :type "A") :test)
      (netz-add-node '(:id 4 :label "Tag" :name "two") :test)
      (netz-add-node '(:id 5 :label "Tag" :name "five") :test)
      (netz-add-edge '(:id (1 2) :type "A" :weight 4) :test)
      (netz-add-edge '(:id (2 3) :type "B" :weight 4) :test)
      (netz-add-edge '(:id (4 3) :type "A" :weight 2) :test)
      (netz-add-edge '(:id (2 1) :type "C" :weight 8) :test)
      (netz-add-edge '(:id (5 4) :type "C" :weight 8) :test)
      (netz-filtered-graph (:nodes (:match :name "five")) :test :test2)
      (expect (ht-keys (netz-get-edges :test2)) :to-have-same-items-as '())
      (expect (ht-keys (netz-get-nodes :test2)) :to-have-same-items-as '(5))

      (netz-filtered-graph (:nodes (:or (:match :type "A")
					(:match :name "one"))) :test :test3)
      (expect (ht-keys (netz-get-edges :test3)) :to-have-same-items-as '((1 2) (2 1) (2 3)))
      (expect (ht-keys (netz-get-nodes :test3)) :to-have-same-items-as '(1 2 3))))

  (describe "bulk tests"
    (before-each (netz-make-graph :test))

    (it "add 10,000 nodes"
      (dotimes (i 10000)
	(netz-add-node `(:id ,i) :test))
      (expect (hash-table-count (netz-get-nodes :test)) :to-equal 10000))
    (it "add, save, and load 10,000 nodes"
      (dotimes (i 10000)
	(netz-add-node `(:id ,i) :test))
      (setq path (netz-get-path :test))
      (netz-save-graph :test)
      (ht-clear *netz-graphs*)
      (netz-load-graph path)
      (expect (netz-get-node 888 :test) :to-equal '(:id 888)))
    (it "add 10,000 nodes and 9,999 edges"
      (dotimes (i 10000)
	(netz-add-node `(:id ,i) :test))
      (dotimes (i 9999)
	(netz-connect-nodes
	 (netz-get-node i :test)
	 (netz-get-node (+ 1 i) :test)
	 '(:type "R")
	 :test)))
    (it "add 10,000 nodes and 9,999 edges; save and load"
      (dotimes (i 10000)
	(netz-add-node `(:id ,i) :test))
      (dotimes (i 9999)
	(netz-connect-nodes
	 (netz-get-node i :test)
	 (netz-get-node (+ 1 i) :test)
	 '(:type "R")
	 :test))

      (setq path (netz-get-path :test))
      (netz-save-graph :test)
      (ht-clear *netz-graphs*)
      (netz-load-graph path)

      (expect (hash-table-count (netz-get-nodes :test)) :to-equal 10000)
      (expect (hash-table-count (netz-get-edges :test)) :to-equal 9999)
      (expect (netz-get-node 5555 :test) :to-equal '(:id 5555 :edges ((5555 5556) (5554 5555))))))

  (describe "traversal and such"
    (before-each (netz-make-graph :test))

    (it "get node hood"
      (netz-add-node '(:id 1) :test)
      (netz-add-node '(:id 2) :test)
      (netz-add-node '(:id 3) :test)
      (netz-add-node '(:id 4) :test)
      (netz-connect-nodes (netz-get-node 1 :test) (netz-get-node 2 :test) '(:type "R") :test)
      (netz-connect-nodes (netz-get-node 3 :test) (netz-get-node 1 :test) '(:type "C") :test)
      (expect (netz-get-node 1 :test) :to-equal '(:id 1 :edges ((3 1) (1 2))))
      (expect (netz-get-node-hood 1 :test :new) :not :to-throw)
      (expect (ht-keys (netz-get-nodes
			(netz-get-node-hood 1 :test :new)))
	      :to-have-same-items-as '(1 2 3))
      (expect (ht-keys (netz-get-edges
			(netz-get-node-hood 1 :test :new)))
	      :to-have-same-items-as '((1 2) (3 1)))
      ;; hood filtering
      (expect (netz-get-node 3 (netz-get-node-hood 1 :test :new (:match :type "R")))
	      :to-be nil)
      (expect (ht-keys (netz-get-nodes (netz-get-node-hood 1 :test :new (:match :type "C"))))
	      :to-have-same-items-as '(1 3))
      (expect (netz-get-edge '(3 1) (netz-get-node-hood 1 :test :new (:match :type "C")))
	      :to-equal (netz-get-edge '(3 1) :test))
      (expect (netz-get-edge '(3 1) (netz-get-node-hood 1 :test :new (:match :type "R")))
	      :to-be nil)
      (expect (ht-keys (netz-get-edges (netz-get-node-hood 1 :test :new
							   (:or
							    (:match :type "R")
							    (:match :type "C")))))
	      :to-have-same-items-as '((1 2) (3 1)))
      (netz-connect-nodes (netz-get-node 1 :test) (netz-get-node 4 :test) '(:type "R") :test)
      (expect (ht-keys (netz-get-nodes
			(netz-get-node-hood 1 :test :new (:match :type "R"))))
	      :to-have-same-items-as '(1 2 4)))
    (it "finds shortest undirected unweighted path"
      (netz-add-node '(:id 1) :test)
      (netz-add-node '(:id 2) :test)
      (netz-add-node '(:id 3) :test)
      (netz-add-node '(:id 4) :test)
      (netz-add-node '(:id 5) :test)
      (netz-add-node '(:id 6) :test)
      (netz-connect-nodes (netz-get-node 1 :test) (netz-get-node 2 :test) '(:type "R") :test)
      (netz-connect-nodes (netz-get-node 2 :test) (netz-get-node 3 :test) '(:type "C") :test)
      ;; add a cycle
      (netz-connect-nodes (netz-get-node 2 :test) (netz-get-node 1 :test) '(:type "C") :test)
      (netz-connect-nodes (netz-get-node 3 :test) (netz-get-node 4 :test) '(:type "C") :test)
      (netz-connect-nodes (netz-get-node 2 :test) (netz-get-node 5 :test) '(:type "C") :test)

      (expect (netz-bfs-shortest-path
	       (netz-get-node 1 :test)
	       (netz-get-node 4 :test) :test) :to-equal '(1 2 3 4))
      (expect (netz-bfs-shortest-path
	       (netz-get-node 3 :test)
	       (netz-get-node 5 :test) :test) :to-equal '(3 2 5))
      (expect (netz-bfs-shortest-path
	       (netz-get-node 5 :test)
	       (netz-get-node 3 :test) :test) :to-equal '(5 2 3))
      (expect (netz-bfs-shortest-path
	       (netz-get-node 1 :test)
	       (netz-get-node 6 :test) :test) :to-equal nil))
    (it "finds shortest directed unweighted path"
      (netz-add-node '(:id 1) :test)
      (netz-add-node '(:id 2) :test)
      (netz-add-node '(:id 3) :test)
      (netz-add-node '(:id 4) :test)
      (netz-add-node '(:id 5) :test)
      (netz-add-node '(:id 6) :test)
      (netz-connect-nodes (netz-get-node 1 :test) (netz-get-node 2 :test) '(:type "R") :test)
      (netz-connect-nodes (netz-get-node 2 :test) (netz-get-node 3 :test) '(:type "C") :test)
      ;; add a cycle
      (netz-connect-nodes (netz-get-node 2 :test) (netz-get-node 1 :test) '(:type "C") :test)
      (netz-connect-nodes (netz-get-node 3 :test) (netz-get-node 4 :test) '(:type "C") :test)
      (netz-connect-nodes (netz-get-node 2 :test) (netz-get-node 5 :test) '(:type "C") :test)

      (expect (netz-bfs-shortest-path
	       (netz-get-node 5 :test)
	       (netz-get-node 1 :test) :test t) :to-equal nil)
      (expect (netz-bfs-shortest-path
	       (netz-get-node 3 :test)
	       (netz-get-node 5 :test) :test t) :to-equal nil)

      (netz-connect-nodes (netz-get-node 4 :test) (netz-get-node 2 :test) '(:type "C") :test)
      (netz-connect-nodes (netz-get-node 5 :test) (netz-get-node 4 :test) '(:type "C") :test)
      (expect (netz-bfs-shortest-path
	       (netz-get-node 3 :test)
	       (netz-get-node 5 :test) :test t) :to-equal '(3 4 2 5)))))
