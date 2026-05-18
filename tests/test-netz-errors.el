;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'netz)
(require 'netz-query)

(describe "Netz error handling"
  (describe "core errors"
    (it "rejects invalid edge directions"
      (let ((graph (netz-create-graph :test)))
        (expect (netz-node-edge-ids graph "a" :direction :sideways) :to-throw)))

    (it "throws when loading a missing graph file"
      (expect (netz-load-graph "/tmp/netz-missing-file-does-not-exist") :to-throw)))

  (describe "query errors"
    (it "rejects unknown clauses"
      (let ((graph (netz-create-graph :test)))
        (expect (netz-query graph
                  (:bogus))
                :to-throw)))

    (it "rejects malformed node patterns"
      (let ((graph (netz-create-graph :test)))
        (expect (netz-query graph
                  (:match
                   (:node))
                  (:return :count))
                :to-throw)))

    (it "rejects malformed edge patterns"
      (let ((graph (netz-create-graph :test)))
        (expect (netz-query graph
                  (:match
                   (:edge a))
                  (:return :count))
                :to-throw)))

    (it "rejects malformed path patterns"
      (let ((graph (netz-create-graph :test)))
        (expect (netz-query graph
                  (:match
                   (:path a))
                  (:return :count))
                :to-throw)))

    (it "rejects invalid edge directions in queries"
      (let ((graph (netz-create-graph :test)))
        (expect (netz-query graph
                  (:match
                   (:node a)
                   (:edge a b :direction :sideways))
                  (:return b))
                :to-throw)))

    (it "rejects invalid path directions in queries"
      (let ((graph (netz-create-graph :test)))
        (expect (netz-query graph
                  (:match
                   (:node a)
                   (:path a b :direction :sideways :depth (1 2)))
                  (:return b))
                :to-throw)))

    (it "rejects returns of unbound variables"
      (let ((graph (netz-create-graph :test)))
        (expect (netz-query graph
                  (:match
                   (:node a))
                  (:return b))
                :to-throw)))

    (it "rejects mutation of unbound variables"
      (let ((graph (netz-create-graph :test)))
        (expect (netz-query graph
                  (:set missing :name "Missing"))
                :to-throw)))

    (it "rejects duplicate ids on create"
      (let ((graph (netz-create-graph :test)))
        (netz-add-node graph '(:id "a"))
        (expect (netz-query graph
                  (:create
                   (:node a :id "a")))
                :to-throw)))

    (it "rejects node merge without an id"
      (let ((graph (netz-create-graph :test)))
        (expect (netz-query graph
                  (:merge
                   (:node person :label "Person")))
                :to-throw)))

    (it "rejects edge merge without an id"
      (let ((graph (netz-create-graph :test)))
        (netz-add-node graph '(:id "a"))
        (netz-add-node graph '(:id "b"))
        (expect (netz-query graph
                  (:match
                   (:node a :id "a")
                   (:node b :id "b"))
                  (:merge
                   (:edge a b :type "KNOWS")))
                :to-throw)))))
