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

(defclass simple-chat-app (application ai-project-tools/core:runnable)
  ((%kernel
    :initarg :kernel
    :initform nil
    :accessor kernel
    ;; :type lp:kernel
    :documentation "The lparallel kernel for the application.")
   (%task-queue
    :initform nil
    :accessor task-queue
    ;; :type lp:queue
    :documentation "The lparallel task queue for the application.")
   (%task-queue-capacity
    :initform 8
    :accessor task-queue-capacity
    :type integer
    :documentation "The capacity of the task queue.")
   (%running-p
    :initform nil
    :accessor running-p
    :type boolean
    :documentation "Whether the application is running or not.")))

(defgeneric run-task (app task)
  (:documentation "Run a task in the application."))

(defmethod run-task ((app simple-chat-app) task)
  (unless (running-p app)
    (error "Cannot run task ~a in application ~a because it is not running." task app))
  (let ((result-channel (lp:make-channel)))
    (lp:submit-task result-channel
                    (lambda ()
                      (let ((result (funcall task)))
                        result)))
    (lp:receive-result result-channel)))

(defgeneric stop (app))

(defmethod stop ((app simple-chat-app))
  (unless (running-p app)
    (error "Cannot stop application ~a because it is not running." app))
  (run-task app :stop))

(defun %shutdown-app (app)
  (lp:end-kernel :wait t)
  (log:info "Kernel shut down. App ~a is no longer running." app)
  (log:info "lp:*kernel* = ~a" lp:*kernel*)
  (when (> 0 (lparallel.queue:queue-count (task-queue app)))
    (log:warn "App task-queue count: ~a. NOTE: These tasks will NOT be run. Discarding..."
              (lparallel.queue:queue-count (task-queue app)))
  (setf (kernel app) nil
        (task-queue app) nil
        (running-p app) nil)))

(defmethod ai-project-tools/core:run ((app simple-chat-app)
                                      &rest args
                                      &key
                                        (project (project app) project-supplied-p)
                                        (session nil session-supplied-p))
  (unwind-protect
       (let* ((core::*current-application* app)
              (core::*current-system-configuration* (system-configuration app))
              (core::*root-metadata-store* (metadata-store app))
              (core::*current-metadata-store* (metadata-store app))
              (core::*current-project* project)
              (core::*current-session* session))
         (setf (kernel app) (lp:make-kernel 8 :name (format nil "~a-kernel" (name app))
                                              :bindings `((core::*current-application* . ,core::*current-application*)
                                                          (core::*current-system-configuration* . ,core::*current-system-configuration*)
                                                          (core::*current-metadata-store* . ,core::*current-metadata-store*)
                                                          (core::*root-metadata-store* . ,core::*root-metadata-store*)
                                                          (core::*current-project* . ,core::*current-project*)
                                                          (core::*current-session* . ,core::*current-session*)))
               lp:*kernel* (kernel app)
               (task-queue app) (lparallel.queue:make-queue :fixed-capacity (task-queue-capacity app))
               (running-p app) t)
         (let ((iterations 0)
               (queue-timeouts 0)
               (check-app-kernel-running-p (run-task app (lambda ()
                                                           (log:info "Running ~a with args ~a" app args)
                                                           t))))
           (log:info "Application ran first task. Result: ~a" check-app-kernel-running-p)
         (loop :while (running-p app)
               :do (multiple-value-bind (new-task no-timeout-p) (lparallel.queue:try-pop-queue (task-queue app) :timeout 5)
                     (incf iterations)
                     (if (not no-timeout-p)
                         (progn
                           (incf queue-timeouts)
                           (log:info "App task queue timeout. Iterations: ~a. Queue timeouts: ~a" iterations queue-timeouts))
                         (if (eq new-task :stop)
                             (setf (running-p app) nil)
                             (run-task app new-task)))))))
  (progn (%shutdown-app app)
         (log:info "App ~a shut down.DONE." app))))

;; (make-in-mem-app)
;; (run-task *in-mem-app* (lambda () (log:info "Running this task in app ~a" core:*current-application*)))
;; (stop *in-mem-app*)
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
          *in-mem-app* app)
    (log:info "Created in-mem app ~a" app)
    (ai-project-tools/core:run app project session)))
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
