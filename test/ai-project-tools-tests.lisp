(in-package :ai-project-tools/tests)

(def-suite ai-project-tools/tests-suite)

(in-suite ai-project-tools/tests-suite)

(test ai-project-tools/tests-suite-exists
  (is-true t))

;; (ql:quickload '(:ai-project-tools/tests))
;; (run! 'ai-project-tools/tests-suite-exists)
;; (run! 'ai-project-tools/tests-suite)

(defun run-ai-project-tools-tests ()
  ;; (ql:quickload '(:ai-project-tools/core-tests
  ;;                 :ai-project-tools/app-tests))
  ;; (run! 'ai-project-tools/core-tests:ai-project-tools/core-tests-suite)
  ;; (run! 'ai-project-tools/app-tests:ai-project-tools/app-tests-suite)
  (run! 'ai-project-tools/tests-suite))
