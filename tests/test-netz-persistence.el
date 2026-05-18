;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'netz)

(defvar netz-test-persistence-path "/tmp/netz-query-design-persistence.el")

(describe "Netz persistence"
  (after-each
    (when (file-exists-p netz-test-persistence-path)
      (delete-file netz-test-persistence-path)))

  (it "saves and loads graph data"
    (let ((graph (netz-create-graph :persist :path netz-test-persistence-path)))
      (netz-add-node graph '(:id "a" :label "Note" :title "A"))
      (netz-add-node graph '(:id "b" :label "Note" :title "B"))
      (netz-add-edge graph '(:id "e1" :source "a" :target "b" :type "LINKS_TO"))
      (netz-save-graph graph)
      (let ((loaded (netz-load-graph netz-test-persistence-path)))
        (expect (netz-graph-p loaded) :to-be-truthy)
        (expect (netz-graph-name loaded) :to-equal :persist)
        (expect (netz-get-node loaded "a") :to-equal '(:id "a" :label "Note" :title "A"))
        (expect (netz-get-edge loaded "e1") :to-equal '(:id "e1" :source "a" :target "b" :type "LINKS_TO")))))

  (it "preserves or rebuilds indexes after load"
    (let ((graph (netz-create-graph :persist :path netz-test-persistence-path)))
      (netz-add-node graph '(:id "a"))
      (netz-add-node graph '(:id "b"))
      (netz-add-node graph '(:id "c"))
      (netz-add-edge graph '(:id "e1" :source "a" :target "b" :type "LINKS_TO"))
      (netz-add-edge graph '(:id "e2" :source "c" :target "b" :type "LINKS_TO"))
      (netz-save-graph graph)
      (let ((loaded (netz-load-graph netz-test-persistence-path)))
        (expect (netz-node-edge-ids loaded "a" :direction :out) :to-have-same-items-as '("e1"))
        (expect (netz-node-edge-ids loaded "b" :direction :in) :to-have-same-items-as '("e1" "e2"))
        (expect (netz-node-edge-ids loaded "b" :direction :any) :to-have-same-items-as '("e1" "e2"))))))
