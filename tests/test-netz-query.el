;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'netz)
(require 'netz-query)

(defun netz-query-test--node-ids (rows binding)
  "Return node ids for BINDING from ROWS.

ROWS are plist binding rows, e.g.
`((:person (:id "p1" ...)) (:person (:id "p2" ...)))'."
  (mapcar (lambda (row)
            (plist-get (plist-get row binding) :id))
          rows))

(defun netz-query-test--seed-people-graph ()
  "Return a graph with people, projects, and tags for query specs."
  (let ((graph (netz-create-graph :query-test)))
    (netz-add-node graph '(:id "person:tosh" :label "Person" :name "Tosh" :active t))
    (netz-add-node graph '(:id "person:ada" :label "Person" :name "Ada" :active t))
    (netz-add-node graph '(:id "person:grace" :label "Person" :name "Grace" :active nil))
    (netz-add-node graph '(:id "project:netz" :label "Project" :name "netz" :stars 12))
    (netz-add-node graph '(:id "project:pi" :label "Project" :name "pi" :stars 7))
    (netz-add-node graph '(:id "tag:emacs" :label "Tag" :name "emacs"))
    (netz-add-node graph '(:id "tag:graphs" :label "Tag" :name "graphs"))
    (netz-add-edge graph '(:id "works:tosh:netz" :source "person:tosh" :target "project:netz" :type "WORKS_ON"))
    (netz-add-edge graph '(:id "works:ada:netz" :source "person:ada" :target "project:netz" :type "WORKS_ON"))
    (netz-add-edge graph '(:id "works:grace:pi" :source "person:grace" :target "project:pi" :type "WORKS_ON"))
    (netz-add-edge graph '(:id "tagged:netz:emacs" :source "project:netz" :target "tag:emacs" :type "TAGGED"))
    (netz-add-edge graph '(:id "tagged:netz:graphs" :source "project:netz" :target "tag:graphs" :type "TAGGED"))
    graph))

(describe "Netz query DSL"
  (describe "MATCH node patterns"
    (it "returns plist binding rows for matching nodes"
      (let ((graph (netz-query-test--seed-people-graph)))
        (expect (netz-query graph
                  (:match
                   (:node person :label "Person"))
                  (:return person))
                :to-have-same-items-as
                `((:person ,(netz-get-node graph "person:tosh"))
                  (:person ,(netz-get-node graph "person:ada"))
                  (:person ,(netz-get-node graph "person:grace"))))))

    (it "matches nodes by id"
      (let ((graph (netz-query-test--seed-people-graph)))
        (expect (netz-query graph
                  (:match
                   (:node person :id "person:tosh"))
                  (:return person))
                :to-equal
                `((:person ,(netz-get-node graph "person:tosh"))))))

    (it "accepts registered graph names"
      (let ((graph (netz-query-test--seed-people-graph)))
        (expect (netz-query :query-test
                  (:match
                   (:node person :id "person:tosh"))
                  (:return person))
                :to-equal
                `((:person ,(netz-get-node graph "person:tosh"))))))

    (it "returns nil when no nodes match"
      (let ((graph (netz-query-test--seed-people-graph)))
        (expect (netz-query graph
                  (:match
                   (:node person :label "Person" :name "Nobody"))
                  (:return person))
                :to-equal nil)))

    (it "supports anonymous node patterns"
      (let ((graph (netz-query-test--seed-people-graph)))
        (expect (netz-query graph
                  (:match
                   (:node _ :label "Tag"))
                  (:return :count))
                :to-equal 2)))

    (it "supports anonymous edge endpoints"
      (let ((graph (netz-query-test--seed-people-graph)))
        (expect (netz-query-test--node-ids
                 (netz-query graph
                   (:match
                    (:node project :label "Project")
                    (:edge project _ :type "TAGGED" :direction :out))
                   (:return project))
                 :project)
                :to-have-same-items-as
                '("project:netz" "project:netz")))))

  (describe "MATCH edge patterns"
    (it "matches related nodes with any direction by default"
      (let ((graph (netz-query-test--seed-people-graph)))
        (expect (netz-query-test--node-ids
                 (netz-query graph
                   (:match
                    (:node project :id "project:netz")
                    (:edge project person :type "WORKS_ON")
                    (:node person :label "Person"))
                   (:return person))
                 :person)
                :to-have-same-items-as
                '("person:tosh" "person:ada"))))

    (it "matches outgoing directed edges"
      (let ((graph (netz-query-test--seed-people-graph)))
        (expect (netz-query-test--node-ids
                 (netz-query graph
                   (:match
                    (:node person :label "Person")
                    (:edge person project :type "WORKS_ON" :direction :out)
                    (:node project :id "project:netz"))
                   (:return person))
                 :person)
                :to-have-same-items-as
                '("person:tosh" "person:ada"))))

    (it "matches incoming directed edges"
      (let ((graph (netz-query-test--seed-people-graph)))
        (expect (netz-query-test--node-ids
                 (netz-query graph
                   (:match
                    (:node project :id "project:netz")
                    (:edge project person :type "WORKS_ON" :direction :in)
                    (:node person :label "Person"))
                   (:return person))
                 :person)
                :to-have-same-items-as
                '("person:tosh" "person:ada"))))

    (it "keeps repeated bindings consistent across patterns"
      (let ((graph (netz-query-test--seed-people-graph)))
        (expect (netz-query graph
                  (:match
                   (:node project :label "Project")
                   (:edge project tag :type "TAGGED" :direction :out)
                   (:node tag :name "emacs")
                   (:node project :name "netz"))
                  (:return project tag))
                :to-equal
                `((:project ,(netz-get-node graph "project:netz")
                   :tag ,(netz-get-node graph "tag:emacs"))))))

    (it "can bind matched edges with :as"
      (let ((graph (netz-query-test--seed-people-graph)))
        (expect (netz-query graph
                  (:match
                   (:node person :id "person:tosh")
                   (:edge person project :as rel :type "WORKS_ON" :direction :out))
                  (:return rel))
                :to-equal
                `((:rel ,(netz-get-edge graph "works:tosh:netz")))))))

  (describe "WHERE and RETURN clauses"
    (it "allows ordinary elisp predicates over bound variables"
      (let ((graph (netz-query-test--seed-people-graph)))
        (expect (netz-query graph
                  (:match
                   (:node person :label "Person")
                   (:edge person project :type "WORKS_ON" :direction :out)
                   (:node project :label "Project"))
                  (:where
                   (> (plist-get project :stars) 10))
                  (:return person project))
                :to-have-same-items-as
                `((:person ,(netz-get-node graph "person:tosh")
                   :project ,(netz-get-node graph "project:netz"))
                  (:person ,(netz-get-node graph "person:ada")
                   :project ,(netz-get-node graph "project:netz"))))))

    (it "supports netz-prop in where predicates"
      (let ((graph (netz-query-test--seed-people-graph)))
        (expect (netz-query graph
                  (:match
                   (:node project :label "Project"))
                  (:where
                   (> (netz-prop project :stars) 10))
                  (:return project))
                :to-equal
                `((:project ,(netz-get-node graph "project:netz"))))))

    (it "supports count returns"
      (let ((graph (netz-query-test--seed-people-graph)))
        (expect (netz-query graph
                  (:match
                   (:node person :label "Person"))
                  (:return :count))
                :to-equal 3)))

    (it "supports id returns"
      (let ((graph (netz-query-test--seed-people-graph)))
        (expect (netz-query graph
                  (:match
                   (:node person :label "Person"))
                  (:return :ids person))
                :to-have-same-items-as
                '("person:tosh" "person:ada" "person:grace"))))

    (it "supports pluck returns"
      (let ((graph (netz-query-test--seed-people-graph)))
        (expect (netz-query graph
                  (:match
                   (:node person :label "Person"))
                  (:return :pluck person :name))
                :to-have-same-items-as
                '("Tosh" "Ada" "Grace"))))

    (it "supports graph returns"
      (let ((graph (netz-query-test--seed-people-graph)))
        (let ((result (netz-query graph
                        (:match
                         (:node person :label "Person")
                         (:edge person project :type "WORKS_ON" :direction :out)
                         (:node project :id "project:netz"))
                        (:return :graph :netz-workers))))
          (expect (netz-graph-p result) :to-be-truthy)
          (expect (netz-graph-name result) :to-equal :netz-workers)
          (expect (mapcar (lambda (node) (plist-get node :id))
                          (netz-get-nodes result))
                  :to-have-same-items-as '("person:tosh" "person:ada" "project:netz"))))))

  (describe "path patterns"
    (it "matches depth-limited directed paths"
      (let ((graph (netz-query-test--seed-people-graph)))
        (expect (netz-query-test--node-ids
                 (netz-query graph
                   (:match
                    (:node start :id "person:tosh")
                    (:path start target
                     :direction :out
                     :depth (2 2))
                    (:node target :label "Tag"))
                   (:return target))
                 :target)
                :to-have-same-items-as
                '("tag:emacs" "tag:graphs"))))

    (it "honors path edge property constraints"
      (let ((graph (netz-query-test--seed-people-graph)))
        (expect (netz-query-test--node-ids
                 (netz-query graph
                   (:match
                    (:node start :id "person:tosh")
                    (:path start target
                     :edge (:type "WORKS_ON")
                     :direction :out
                     :depth (1 2)))
                   (:return target))
                 :target)
                :to-have-same-items-as
                '("project:netz"))))

    (it "supports zero-depth paths"
      (let ((graph (netz-query-test--seed-people-graph)))
        (expect (netz-query graph
                  (:match
                   (:node start :id "person:tosh")
                   (:path start target
                    :direction :out
                    :depth (0 0)))
                  (:return target))
                :to-equal
                `((:target ,(netz-get-node graph "person:tosh"))))))

    (it "handles cycles without looping"
      (let ((graph (netz-query-test--seed-people-graph)))
        (netz-add-edge graph '(:id "cycle:netz:tosh" :source "project:netz" :target "person:tosh" :type "REFERENCES"))
        (expect (netz-query graph
                  (:match
                   (:node start :id "person:tosh")
                   (:path start target
                    :direction :out
                    :depth (1 4))
                   (:node target :id "person:tosh"))
                  (:return :count))
                :to-equal 1)))

    (it "returns nil when no path matches"
      (let ((graph (netz-query-test--seed-people-graph)))
        (expect (netz-query graph
                  (:match
                   (:node start :id "person:tosh")
                   (:path start target
                    :edge (:type "DOES_NOT_EXIST")
                    :direction :out
                    :depth (1 3)))
                  (:return target))
                :to-equal nil))))

  (describe "mutation clauses"
    (it "creates nodes and edges using the query pattern language"
      (let ((graph (netz-create-graph :test)))
        (expect (netz-query graph
                  (:create
                   (:node person :id "person:tosh" :label "Person" :name "Tosh")
                   (:node project :id "project:netz" :label "Project" :name "netz")
                   (:edge person project :id "works:tosh:netz" :type "WORKS_ON"))
                  (:return person project))
                :to-equal
                `((:person ,(netz-get-node graph "person:tosh")
                   :project ,(netz-get-node graph "project:netz"))))
        (expect (netz-get-edge graph "works:tosh:netz") :not :to-be nil)
        (expect (netz-node-edge-ids graph "person:tosh" :direction :out)
                :to-have-same-items-as '("works:tosh:netz"))
        (expect (netz-node-edge-ids graph "project:netz" :direction :in)
                :to-have-same-items-as '("works:tosh:netz"))))

    (it "merges nodes with find-or-create semantics"
      (let ((graph (netz-create-graph :test)))
        (netz-add-node graph '(:id "person:tosh" :label "Person"))
        (netz-query graph
          (:merge
           (:node person :id "person:tosh" :label "Person"))
          (:set person :name "Tosh" :active t)
          (:return person))
        (expect (plist-get (netz-get-node graph "person:tosh") :name)
                :to-equal "Tosh")
        (expect (netz-graph-node-count graph) :to-equal 1)))

    (it "merges edges with find-or-create semantics"
      (let ((graph (netz-create-graph :test)))
        (netz-add-node graph '(:id "person:tosh" :label "Person"))
        (netz-add-node graph '(:id "project:netz" :label "Project"))
        (netz-query graph
          (:match
           (:node person :id "person:tosh")
           (:node project :id "project:netz"))
          (:merge
           (:edge person project :id "works:tosh:netz" :type "WORKS_ON"))
          (:merge
           (:edge person project :id "works:tosh:netz" :type "WORKS_ON"))
          (:return person project))
        (expect (netz-graph-edge-count graph) :to-equal 1)))

    (it "sets edge properties"
      (let ((graph (netz-create-graph :test)))
        (netz-add-node graph '(:id "a"))
        (netz-add-node graph '(:id "b"))
        (netz-add-edge graph '(:id "e1" :source "a" :target "b" :type "KNOWS"))
        (netz-query graph
          (:match
           (:node a :id "a")
           (:edge a b :as rel :id "e1" :direction :out))
          (:set b :name "B")
          (:set rel :since 2026)
          (:return a b))
        (expect (plist-get (netz-get-node graph "b") :name) :to-equal "B")
        (expect (plist-get (netz-get-edge graph "e1") :since) :to-equal 2026)))

    (it "deletes matched nodes without incident edges"
      (let ((graph (netz-create-graph :test)))
        (netz-add-node graph '(:id "tmp:1" :label "Temp"))
        (netz-query graph
          (:match
           (:node node :label "Temp"))
          (:delete node))
        (expect (netz-get-node graph "tmp:1") :to-be nil)))

    (it "deletes matched edges"
      (let ((graph (netz-create-graph :test)))
        (netz-add-node graph '(:id "a"))
        (netz-add-node graph '(:id "b"))
        (netz-add-edge graph '(:id "e1" :source "a" :target "b" :type "TEMP"))
        (netz-query graph
          (:match
           (:node a :id "a")
           (:edge a b :as rel :id "e1" :direction :out))
          (:delete rel))
        (expect (netz-get-edge graph "e1") :to-be nil)
        (expect (netz-node-edge-ids graph "a" :direction :out) :to-equal nil)
        (expect (netz-node-edge-ids graph "b" :direction :in) :to-equal nil)))

    (it "detach-deletes matched nodes and incident edges"
      (let ((graph (netz-create-graph :test)))
        (netz-add-node graph '(:id "a"))
        (netz-add-node graph '(:id "b"))
        (netz-add-edge graph '(:id "e1" :source "a" :target "b" :type "TEMP"))
        (netz-query graph
          (:match
           (:node a :id "a"))
          (:detach-delete a))
        (expect (netz-get-node graph "a") :to-be nil)
        (expect (netz-get-edge graph "e1") :to-be nil)
        (expect (netz-node-edge-ids graph "b" :direction :in) :to-equal nil)))))
