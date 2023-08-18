(in-package #:ai-project-tools/app-tests)

(defparameter *in-mem-app* nil)
(defparameter *in-mem-app-proj* nil)
(defparameter *in-mem-app-session* nil)
;;(describe *in-mem-app*)
;;(scoped-path (metadata-store (project *in-mem-app*)))

(defclass simple-chat-app (base-lparallel-application) ())

(defun make-in-mem-app ()
  (let* ((config (make-instance 'core::simple-in-memory-system-configuration
                                :designator :ephemeral-chat-config
                                :name "Simple in-memory system configuration."
                                :description "A simple in-memory system configuration that does not persist."))
         (store (make-instance 'core::memory-scoped-metadata-store
                               :name "root"
                               :parent nil
                               :schema (list :fake :schema)
                               :store (make-hash-table :test #'equal)))
         (projects-store (make-instance 'core::memory-scoped-metadata-store
                                        :name "projects"
                                        :parent store
                                        :schema (list :fake :schema)))
         (project-store (make-instance 'core::memory-scoped-metadata-store
                                       :name "ephemeral-chat-project"
                                       :parent projects-store
                                       :schema (list :fake :schema)))
         (project (make-instance 'core:project
                                 :name "ephemeral-chat-project"
                                 :description "Project for a quick-n-dirty chat app for testing in the REPL."
                                 :metadata-store project-store))
         (sessions-store (make-instance 'core::memory-scoped-metadata-store
                                        :name "sessions"
                                        :parent project-store
                                        :schema (list :fake :schema)))
         (session-store (make-instance 'core::memory-scoped-metadata-store
                                       :name "ephemeral-chat-session"
                                       :parent sessions-store
                                       :schema (list :fake :schema)))
         (session (make-instance 'core:session
                                 :name "ephemeral-chat-session"
                                 :description "Session for a quick-n-dirty chat app for testing in the REPL."
                                 :project project
                                 :metadata-store session-store))
         (app (make-instance 'simple-chat-app
                             :name "ephemeral-chat-app"
                             :description "Just a quick-n-dirty chat app for testing in the REPL."
                             :system-configuration config
                             :project project
                             :metadata-store store)))
    (core:register-system-configuration :default (system-configuration app))
    (setf *in-mem-app-proj* project
          *in-mem-app-session* session
          *in-mem-app* app)
    (log:info "Created in-mem app ~a" app)
    (ai-project-tools/core:run app project session)
    app))

;; (make-in-mem-app)

;; (run-task *in-mem-app* (lambda () (log:info "Running this task in app ~a" core:*current-application*)))
;; (run-task *in-mem-app* (lambda () (error "Error to induce quit.")))
;; (stop *in-mem-app*)
;; (setf (running-p *in-mem-app*) nil)
;; (running-p *in-mem-app*)
;; (%shutdown-app *in-mem-app*)
#+(or)(setf *iterations* 0
            *queue-timeouts* 0)
;; (setf (lookup *in-mem-app-session* "some-session-key") "BLAH")
;; (lookup *in-mem-app-session* "some-session-key")
