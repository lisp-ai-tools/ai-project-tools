(defpackage :ai-project-tools/tests
  (:use #:cl #:fiveam)
  #+nil(:import-from #:ai-project-tools/prompts
                #:make-prompt-template
                #:format-prompt)
  #+nil(:import-from #:fiveam
                #:run-tests)
  (:export #:ai-project-tools/tests-suite))
