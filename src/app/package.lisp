(defpackage :ai-project-tools/app
  (:use #:cl)
  (:local-nicknames
   (:core :ai-project-tools/core)
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
                *current-project*
                *current-session*)
  (:export
   ;; conditions
   ;; classes
   #:runnable-application
   #:lparallel-application
   ;; functions
   ;; #:init ;; just use initialize-instance
   #:destroy
   #:start
   #:stop
   #:run-loop-step
   #:run-task))
