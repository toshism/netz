;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'netz)

(describe "Netz core graph model"
  (describe "graph construction"
    (it "creates an empty graph"
      (let ((graph (netz-create-graph :test)))
        (expect (netz-graph-p graph) :to-be-truthy)
        (expect (netz-graph-name graph) :to-equal :test)
        (expect (netz-graph-node-count graph) :to-equal 0)
        (expect (netz-graph-edge-count graph) :to-equal 0))))

  (describe "nodes"
    (it "adds and retrieves nodes by id"
      (let ((graph (netz-create-graph :test)))
        (netz-add-node graph '(:id "person:tosh" :label "Person" :name "Tosh"))
        (expect (netz-get-node graph "person:tosh")
                :to-equal '(:id "person:tosh" :label "Person" :name "Tosh"))))

    (it "requires node ids"
      (let ((graph (netz-create-graph :test)))
        (expect (netz-add-node graph '(:label "Person")) :to-throw)))

    (it "merges node properties on repeated add with later properties winning"
      (let ((graph (netz-create-graph :test)))
        (netz-add-node graph '(:id "person:tosh" :label "Person" :active nil))
        (netz-add-node graph '(:id "person:tosh" :name "Tosh" :active t))
        (expect (netz-get-node graph "person:tosh")
                :to-equal '(:id "person:tosh" :label "Person" :active t :name "Tosh"))))

    (it "returns all nodes"
      (let ((graph (netz-create-graph :test)))
        (netz-add-node graph '(:id "a"))
        (netz-add-node graph '(:id "b"))
        (expect (mapcar (lambda (node) (plist-get node :id))
                        (netz-get-nodes graph))
                :to-have-same-items-as '("a" "b")))))

  (describe "edges"
    (it "adds and retrieves directed edges by edge id"
      (let ((graph (netz-create-graph :test)))
        (netz-add-node graph '(:id "a" :label "Person"))
        (netz-add-node graph '(:id "b" :label "Person"))
        (netz-add-edge graph '(:id "e1" :source "a" :target "b" :type "KNOWS"))
        (expect (netz-get-edge graph "e1")
                :to-equal '(:id "e1" :source "a" :target "b" :type "KNOWS"))))

    (it "requires edge id, source, and target"
      (let ((graph (netz-create-graph :test)))
        (expect (netz-add-edge graph '(:source "a" :target "b" :type "KNOWS")) :to-throw)
        (expect (netz-add-edge graph '(:id "e1" :target "b" :type "KNOWS")) :to-throw)
        (expect (netz-add-edge graph '(:id "e1" :source "a" :type "KNOWS")) :to-throw)))

    (it "supports multiple distinct edges between the same nodes"
      (let ((graph (netz-create-graph :test)))
        (netz-add-node graph '(:id "a"))
        (netz-add-node graph '(:id "b"))
        (netz-add-edge graph '(:id "e1" :source "a" :target "b" :type "LIKES"))
        (netz-add-edge graph '(:id "e2" :source "a" :target "b" :type "KNOWS"))
        (expect (netz-graph-edge-count graph) :to-equal 2)
        (expect (netz-get-edge graph "e1")
                :to-equal '(:id "e1" :source "a" :target "b" :type "LIKES"))
        (expect (netz-get-edge graph "e2")
                :to-equal '(:id "e2" :source "a" :target "b" :type "KNOWS"))))

    (it "maintains outgoing and incoming edge indexes"
      (let ((graph (netz-create-graph :test)))
        (netz-add-node graph '(:id "a"))
        (netz-add-node graph '(:id "b"))
        (netz-add-node graph '(:id "c"))
        (netz-add-edge graph '(:id "e1" :source "a" :target "b" :type "KNOWS"))
        (netz-add-edge graph '(:id "e2" :source "c" :target "b" :type "KNOWS"))
        (expect (netz-node-edge-ids graph "a" :direction :out) :to-have-same-items-as '("e1"))
        (expect (netz-node-edge-ids graph "b" :direction :in) :to-have-same-items-as '("e1" "e2"))
        (expect (netz-node-edge-ids graph "b" :direction :any) :to-have-same-items-as '("e1" "e2"))))

    (it "returns all edges"
      (let ((graph (netz-create-graph :test)))
        (netz-add-node graph '(:id "a"))
        (netz-add-node graph '(:id "b"))
        (netz-add-edge graph '(:id "e1" :source "a" :target "b" :type "KNOWS"))
        (expect (mapcar (lambda (edge) (plist-get edge :id))
                        (netz-get-edges graph))
                :to-have-same-items-as '("e1"))))

    (it "updates indexes when replacing an edge"
      (let ((graph (netz-create-graph :test)))
        (netz-add-node graph '(:id "a"))
        (netz-add-node graph '(:id "b"))
        (netz-add-node graph '(:id "c"))
        (netz-add-edge graph '(:id "e1" :source "a" :target "b" :type "KNOWS"))
        (netz-add-edge graph '(:id "e1" :source "a" :target "c" :type "KNOWS"))
        (expect (netz-node-edge-ids graph "b" :direction :in) :to-equal nil)
        (expect (netz-node-edge-ids graph "c" :direction :in) :to-have-same-items-as '("e1")))))

  (describe "deletion"
    (it "deletes edges and updates indexes"
      (let ((graph (netz-create-graph :test)))
        (netz-add-node graph '(:id "a"))
        (netz-add-node graph '(:id "b"))
        (netz-add-edge graph '(:id "e1" :source "a" :target "b" :type "KNOWS"))
        (netz-delete-edge graph "e1")
        (expect (netz-get-edge graph "e1") :to-be nil)
        (expect (netz-node-edge-ids graph "a" :direction :out) :to-equal nil)
        (expect (netz-node-edge-ids graph "b" :direction :in) :to-equal nil)))

    (it "detach-deletes nodes and their incident edges"
      (let ((graph (netz-create-graph :test)))
        (netz-add-node graph '(:id "a"))
        (netz-add-node graph '(:id "b"))
        (netz-add-node graph '(:id "c"))
        (netz-add-edge graph '(:id "e1" :source "a" :target "b" :type "KNOWS"))
        (netz-add-edge graph '(:id "e2" :source "b" :target "c" :type "KNOWS"))
        (netz-delete-node graph "b" :detach t)
        (expect (netz-get-node graph "b") :to-be nil)
        (expect (netz-get-edge graph "e1") :to-be nil)
        (expect (netz-get-edge graph "e2") :to-be nil)
        (expect (netz-graph-edge-count graph) :to-equal 0)))

    (it "refuses to delete a node with incident edges unless detach is requested"
      (let ((graph (netz-create-graph :test)))
        (netz-add-node graph '(:id "a"))
        (netz-add-node graph '(:id "b"))
        (netz-add-edge graph '(:id "e1" :source "a" :target "b" :type "KNOWS"))
        (expect (netz-delete-node graph "a") :to-throw)
        (expect (netz-get-node graph "a") :not :to-be nil)
        (expect (netz-get-edge graph "e1") :not :to-be nil)))))
