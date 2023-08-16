(in-package #:ai-project-tools/core-tests)

(def-suite ai-project-tools/core-tests-suite :in ai-project-tools/tests-suite)

(in-suite ai-project-tools/core-tests-suite)

(defclass has-memory ()
  ((%metadata-store :initarg :metadata-store :accessor metadata-store :type metadata-store)))

(defclass has-scoped-memory (has-memory)
  ((%metadata-store :initarg :metadata-store :accessor metadata-store :type scoped-metadata-store)))

(defclass expects-inputs (has-memory)
  ((%inputs :initarg :inputs :accessor inputs :type list)))

(defclass provides-outputs (has-memory)
  ((%outputs :initarg :outputs :accessor outputs :type list)))

(defclass execution-tree-node-with-scoped-memory (execution-tree-node has-scoped-memory)
  ()
  (:documentation
   "An execution node in an execution tree that has a scoped metadata store.
 This node orchestrates the setting of inputs/outputs and execution of its children."))

(defclass simple-chat-app (application ai-project-tools/core:runnable) ())

(defmethod ai-project-tools/core:run ((app simple-chat-app) &rest args)
  (log:info "Running ~a with args ~a" app args))

(defparameter *in-mem-app* nil)
(defparameter *in-mem-app-proj* nil)
(defparameter *in-mem-app-session* nil)
;;(describe *in-mem-app*)
;;(scoped-path (metadata-store (project *in-mem-app*)))
(defun make-in-mem-app ()
  (let* ((config (make-instance 'simple-in-memory-system-configuration
                                :designator :ephemeral-chat-config
                                :name "Simple in-memory system configuration."
                                :description "A simple in-memory system configuration that does not persist."))
         (store (make-instance 'memory-scoped-metadata-store
                               :name "root"
                               :parent nil
                               :schema (list :fake :schema)
                               :store (make-hash-table :test #'equal)))
         (projects-store (make-instance 'memory-scoped-metadata-store
                                        :name "projects"
                                        :parent store
                                        :schema (list :fake :schema)))
         (project-store (make-instance 'memory-scoped-metadata-store
                                       :name "ephemeral-chat-project"
                                       :parent projects-store
                                       :schema (list :fake :schema)))
         (project (make-instance 'project
                                 :name "ephemeral-chat-project"
                                 :description "Project for a quick-n-dirty chat app for testing in the REPL."
                                 :metadata-store project-store))
         (sessions-store (make-instance 'memory-scoped-metadata-store
                                        :name "sessions"
                                        :parent project-store
                                        :schema (list :fake :schema)))
         (session-store (make-instance 'memory-scoped-metadata-store
                                       :name "ephemeral-chat-session"
                                       :parent sessions-store
                                       :schema (list :fake :schema)))
         (session (make-instance 'session
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
    (register-system-configuration :default (system-configuration app))
    (setf *in-mem-app-proj* project
          *in-mem-app-session* session
          *in-mem-app* app)))
;; (make-in-mem-app)
;; (ai-project-tools/core:run *in-mem-app*)
;; (setf (lookup *in-mem-app-session* "some-session-key") "BLAH")
;; (lookup *in-mem-app-session* "some-session-key")

(test ai-project-tools/core-tests-suite-exists
  (is-true t))

(test simple-creation-of-top-level-components-1
  (let ((app (make-in-mem-app)))
    (is-true (typep app 'application))
    (is-true (typep (project app) 'project))
    (is-true (typep (system-configuration app) 'system-configuration))
    (is-true (typep (metadata-store app) 'scoped-metadata-store))))

(test simple-creation-of-top-level-components-2
  (let ((app (make-in-mem-app)))
    (is (eql (get-current-system-configuration) (system-configuration app)))
    (is (eql (get-default-system-configuration) (system-configuration app)))))



;; (ql:quickload '(:ai-project-tools :ai-project-tools/core-tests))
;; (run! 'ai-project-tools/core-tests-suite-exists)
;; (run! 'ai-project-tools/core-tests-suite)

;; Predicates: is is-every is-false is-true signals finishes
