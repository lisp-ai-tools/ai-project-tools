(defpackage :ai-project-tools/app
  (:use #:cl)
  (:local-nicknames
   (:core :ai-project-tools/core)
   (:alx :alexandria)
   (:bt :bordeaux-threads)
   (:lp :lparallel)
   (:lpq :lparallel.queue))
  (:import-from #:ai-project-tools/core
                ;; conditions
                #:immediate-task-stop

                ;; classes
                #:application
                #:system-configuration
                #:simple-in-memory-system-configuration
                #:metadata-store
                #:project
                #:session
                #:runnable
                ;; accessors
                #:name
                ;; functions
                #:run
                #:lookup
                "(setf lookup)"
                ;; variables
                *current-application*
                *current-system-configuration*
                *current-metadata-store*
                *root-metadata-store*
                *current-project*
                *current-session*)
  (:export
   ;; conditions
   ;; classes
   #:run-loop-application
   #:lparallel-application
   ;; accessors
   #:app-lock
   #:app-thread
   #:app-started-latch
   #:max-iterations
   #:iteration-count
   #:running-p
   #:kernel
   #:task-queue
   #:task-queue-capacity
   #:task-queue-timeout
   ;; macros
   #:with-app-lock-held

   ;; functions
   ;; #:init ;; just use initialize-instance
   #:destroy
   #:start
   #:stop
   #:wait-for-app-start
   #:wait-for-app-stop
   #:should-continue-running-p
   #:run-loop-step
   #:run-task
   #:process-task-queue))
