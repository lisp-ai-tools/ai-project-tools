(in-package #:ai-project-tools/core-tests)

(def-suite ai-project-tools/core-tests-suite :in ai-project-tools/tests-suite)

(in-suite ai-project-tools/core-tests-suite)

(test ai-project-tools/core-tests-suite-exists
  (is-true t))

(test simple-creation-of-top-level-components-1
  (let ((app (make-in-mem-app)))
    (is-true (typep app 'application))
    (is-true (typep (project app) 'project))
    (is-true (typep (system-configuration app) 'system-configuration))
    (is-true (typep (metadata-store app) 'scoped-metadata-store))))

(test simple-creation-of-top-level-components-2
  (let ((app (make-in-mem-app)))
    (is (eql (get-current-system-configuration) (system-configuration app)))
    (is (eql (get-default-system-configuration) (system-configuration app)))))

;; (ql:quickload '(:ai-project-tools :ai-project-tools/core-tests))
;; (run! 'ai-project-tools/core-tests-suite-exists)
;; (run! 'ai-project-tools/core-tests-suite)

;; Predicates: is is-every is-false is-true signals finishes
