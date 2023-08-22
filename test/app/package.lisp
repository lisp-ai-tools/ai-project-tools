(defpackage :ai-project-tools/app-tests
  (:use #:cl #:fiveam)
  (:local-nicknames
   (:core :ai-project-tools/core)
   (:app :ai-project-tools/app)
   (:lp :lparallel))
  (:import-from #:fiveam
                #:run-tests)
  (:import-from #:ai-project-tools/core
                ;; conditions

                ;; classes
                #:application
                #:system-configuration
                #:metadata-store
                #:project
                #:session
                ;; accessors
                #:name

                ;; functions
                #:lookup-system-configuration
                #:lookup
                "(setf lookup)")
  (:import-from #:ai-project-tools/app
                ;; conditions
                ;; classes
                #:run-loop-application
                #:base-lparallel-application
                ;; functions
                #:destroy
                #:start
                #:stop
                #:run-loop-step
                #:run-task)
  (:import-from #:ai-project-tools/tests
                #:ai-project-tools/tests-suite)
  (:export #:ai-project-tools/app-tests-suite
           #:ai-project-tools/app-tests-suite-exists
           #:make-in-mem-app))
