(defpackage :ai-project-tools/prompts-tests
  (:use #:cl #:fiveam)
  #+(or)(:local-nicknames
   (:short :long-pkg-name))
  (:import-from #:ai-project-tools/core
                ;; conditions

                ;; classes
                ;; accessors
                ;; functions
                ;; )
  (:import-from #:ai-project-tools/tests
                #:ai-project-tools/tests-suite)
  (:import-from #:fiveam
                #:run-tests)
  (:export #:ai-project-tools/core-tests-suite
           #:ai-project-tools/core-tests-suite-exists))
