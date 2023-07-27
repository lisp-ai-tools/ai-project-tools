(in-package #:ai-project-tools/core)

(defvar *test-app* nil)

(progn
  (setf *test-app* nil)
  (let* ((config (make-instance 'simple-in-memory-system-configuration
                                :designator :ephemeral-chat-config
                                :name "Simple in-memory system configuration."
                                :description "A simple in-memory system configuration that does not persist."))
         (store (make-instance 'memory-scoped-metadata-store
                               :name "/root/"
                               :parent nil
                               :schema (list :fake :schema)
                               :store (make-hash-table :test #'equal)))
         (project (make-instance 'project
                                 :name "ephemeral-chat-project"
                                 :description "Project for a quick-n-dirty chat app for testing in the REPL."
                                 :metadata-store (make-instance 'memory-scoped-metadata-store
                                                                :name "/ephemeral-chat-project/"
                                                                :parent store
                                                                :schema (list :fake :schema))))
         (app (make-instance 'application
                             :name "ephemeral-chat-app"
                             :description "Just a quick-n-dirty chat app for testing in the REPL."
                             :system-configuration config
                             :project project
                             :root-metadata-store store)))
    (setf *test-app* app))
  )
