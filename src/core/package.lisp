(defpackage #:ai-project-tools/core
  (:use #:cl #:alexandria)
  (:import-from #:serapeum
                #:with-thunk)
  (:export
   ;; variables
   *ai-project-tools-version*
   *current-application*
   *current-system-configuration*
   *current-metadata-store*
   *root-metadata-store*
   *current-project*
   *current-session*

   ;;;;   base types
   ;; base mixins
   #:has-data
   #:has-metadata
   #:has-metadata-store
   #:has-notes
   #:has-state
   #:has-name
   #:has-description
   #:has-role
   #:belongs-to-project
   #:belongs-to-session
   #:timed

   ;; Hierarchy based on functional-trees
   #:node
   #:simple-leaf-node
   #:tree-node

   ;; Core system types
   #:artifact
   #:runnable
   #:realtime-timed-runnable
   #:execution-event
   #:execution-node
   #:execution-tree-node

   ;; Core system runtime container types
   #:metadata-store
   #:scoped-metadata-store
   #:metadata-schema
   #:run-context
   #:session
   #:project
   #:system-configuration

   ;; conditions
   #:immediate-task-stop

   ;; accessors
   #:data
   #:metadata
   #:metadata-store
   #:notes
   #:state
   #:name
   #:description
   #:project
   #:session
   #:current-project
   #:current-session
   #:start-time
   #:end-time
   #:duration
   #:time-units
   #:children
   #:input-keys
   #:output-keys
   #:inputs
   #:outputs
   #:execution-node
   #:scope-delimeter
   #:runs

   ;; functions
   #:run
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
   #:lookup
   "(setf lookup)"
   #:lookup-system-configuration
   #:lookup-project
   #:lookup-session
   #:scoped-path
   #:scoped-key-name

   ;; utilities
   #:vector-to-hex-string
   #:file-md5-sum
   #:collect-keyword-args
   #:extract-args
   #:clos-class-initargs
   #:clos-class-initargs-all))
