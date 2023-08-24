(in-package #:ai-project-tools/core-tests)

(def-suite ai-project-tools/core-tests-suite :in ai-project-tools/tests:ai-project-tools/tests-suite)

(in-suite ai-project-tools/core-tests-suite)

(test ai-project-tools/core-tests-suite-exists
  (is-true t))

;; memory-store/scoped-memory-store tests
(test project-session-scoped-store-create-1
  (multiple-value-bind (config root-store project session
                        projects-store project-store
                        sessions-store session-store) (make-scoped-project-sessions)
    (is (not (null config)))
    (is (not (null root-store)))
    (is (not (null project)))
    (is (not (null session)))
    (is (not (null projects-store)))
    (is (not (null project-store)))
    (is (not (null sessions-store)))
    (is (not (null session-store)))
    ;; Test parent-child relationships
    (is (eq (parent projects-store) root-store))
    (is (eq (parent project-store) projects-store))
    (is (eq (parent sessions-store) project-store))
    (is (eq (parent session-store) sessions-store))
    ;; Test scope-paths
    (is (string= (scope-path root-store)
                 "/root"))
    (is (string= (scope-path projects-store)
                 "/root/projects"))
    (is (string= (scope-path project-store)
                 "/root/projects/ephemeral-chat-project"))
    (is (string= (scope-path sessions-store)
                 "/root/projects/ephemeral-chat-project/sessions"))
    (is (string= (scope-path session-store)
                 "/root/projects/ephemeral-chat-project/sessions/ephemeral-chat-session"))))
;; (run! 'project-session-scoped-store-create-1)

(test project-session-scoped-store-lookup-1
  (multiple-value-bind (config root-store project session
                        projects-store project-store
                        sessions-store session-store) (make-scoped-project-sessions)
    (let* ((expected-session-key-prefix
            "/root/projects/ephemeral-chat-project/sessions/ephemeral-chat-session/")
          (expected-session-foo-key (concatenate 'string expected-session-key-prefix "foo")))
      (setf (lookup session-store "foo") "bar")
      (is (string= expected-session-foo-key
                   (scoped-key-name session-store "foo")))
      (is (string= "bar" (lookup session-store "foo")))
      ;; Check raw hash-table lookup on root-store
      (is (string= "bar"
                   (gethash expected-session-foo-key (store root-store))))
      ;; Check has-metadata-store protocol delegation on session object
      (is (string= "bar" (lookup session "foo")))
      ;; Check that the hashtable count are the same
      (is (= 1
             (hash-table-count (store root-store))
             (hash-table-count (store session-store))))
      ;; Check that the backing store is the same object for root/session stores
      (is (eq (store root-store) (store session-store)))
    )))
;; (run! 'project-session-scoped-store-lookup-1)

(test scoped-memory-store-scoping-rules-1
  (let* ((scoped-ht (setup-scoped-hash-table))
         (root-store (make-instance 'core::memory-scoped-metadata-store
                                    :name "a"
                                    :parent nil
                                    :schema (list :fake :schema)
                                    :store scoped-ht))
         (b-store (make-instance 'core::memory-scoped-metadata-store
                                 :name "b"
                                 :parent root-store
                                 :schema (list :fake :schema)))
         (c-store (make-instance 'core::memory-scoped-metadata-store
                                 :name "c"
                                 :parent root-store
                                 :schema (list :fake :schema))))
    (is (= 11 (hash-table-count scoped-ht)))
    (is (eq (core::parent b-store) root-store))
    (is (eq (core::parent c-store) root-store))
    (core::clear c-store)
    (log:info "After clearing c-store: ~a" (alexandria:hash-table-alist scoped-ht))
    (is (= 6 (hash-table-count scoped-ht)))
    (core::clear b-store)
    (is (= 1 (hash-table-count scoped-ht)))))
;; (run! 'scoped-memory-store-scoping-rules-1)

;; (ql:quickload '(:ai-project-tools :ai-project-tools/core-tests))
;; (run! 'ai-project-tools/core-tests-suite-exists)
;; (run! 'ai-project-tools/core-tests-suite)

;; Predicates: is is-every is-false is-true signals finishes
