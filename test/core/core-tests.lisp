(in-package :ai-project-tools/core-tests)

(def-suite ai-project-tools/core-tests-suite :in ai-project-tools/tests-suite)

(in-suite ai-project-tools/core-tests-suite)


(test ai-project-tools/core-tests-suite-exists
  (is-true t))


;; (ql:quickload '(:ai-project-tools/llm-tests))
;; (run! 'ai-project-tools/core-tests-suite-exists)
;; (run! 'ai-project-tools/core-tests-suite)
