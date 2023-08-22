(defpackage :ai-project-tools/core-tests
  (:use #:cl #:fiveam)
  (:local-nicknames
   (:core :ai-project-tools/core)
   (:lp :lparallel))
  (:import-from #:fiveam
                #:run-tests)
  (:import-from #:ai-project-tools/core
                ;; conditions
                #:immediate-task-stop

                ;; classes
                #:application
                #:system-configuration
                #:simple-in-memory-system-configuration
                #:metadata-store
                #:simple-memory-metadata-store
                #:scoped-metadata-store
                #:memory-scoped-metadata-store
                #:project
                #:session
                ;; accessors
                #:root-metadata-store
                #:designator
                #:configuration
                #:sessions
                #:scope-delimeter
                ;; #:metadata-store
                #:execution-events
                #:store
                #:scope-delimeter

                #:input-keys
                #:output-keys
                #:inputs
                #:outputs
                #:execution-node

                #:parent

                #:start-time
                #:end-time
                #:duration

                #:session
                #:project

                #:role
                #:name
                #:description
                #:state
                #:notes
                #:data
                #:metadata

                ;; functions
                #:get-system-configuration
                #:get-system-configuration-registry
                #:register-system-configuration
                #:register-system-components
                #:remove-system-configuration
                #:clear-system-configuration-registry
                #:get-default-system-configuration
                #:get-current-system-configuration
                #:get-current-project
                #:get-current-session
                #:lookup-system-configuration
                #:lookup
                "(setf lookup)"
                #:lookup-project
                #:lookup-session
                #:scoped-path
                #:scoped-key-name)
  (:import-from #:ai-project-tools/tests
                #:ai-project-tools/tests-suite)
  (:export #:ai-project-tools/core-tests-suite
           #:ai-project-tools/core-tests-suite-exists))
