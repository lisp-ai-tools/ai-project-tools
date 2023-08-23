(in-package #:ai-project-tools/core-tests)

(defvar *test-config* nil)
(defvar *test-root-store* nil)
(defvar *test-project* nil)
(defvar *test-projects-store* nil)
(defvar *test-project-store* nil)
(defvar *test-session* nil)
(defvar *test-sessions-store* nil)
(defvar *test-session-store* nil)

(defun clear-vars ()
  (setf *test-config* nil
        *test-root-store* nil
        *test-project* nil
        *test-session* nil
        *test-projects-store* nil
        *test-project-store* nil
        *test-sessions-store* nil
        *test-session-store* nil))

;; (hash-table-count (store *test-root-store*))

(defun make-scoped-project-sessions (&key (clear-vars t))
  (when clear-vars (clear-vars))

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
                                 :metadata-store session-store)))
    (setf *test-config* config
          *test-root-store* store
          *test-project* project
          *test-session* session
          *test-projects-store* projects-store
          *test-project-store* project-store
          *test-sessions-store* sessions-store
          *test-session-store* session-store)
    (values config store project session
            projects-store project-store
            sessions-store session-store)))
