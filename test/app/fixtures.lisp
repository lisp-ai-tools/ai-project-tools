(in-package #:ai-project-tools/app-tests)


(defparameter *in-mem-app* nil)
(defparameter *in-mem-app-proj* nil)
(defparameter *in-mem-app-session* nil)
;;(describe *in-mem-app*)
;;(scoped-path (metadata-store (project *in-mem-app*)))

(defclass simple-chat-app (base-lparallel-application) ())

(defclass mock-chat-app (simple-chat-app)
  ((%prompts
    :initarg :prompts
    :initform ()
    :accessor prompts
    :type list
    :documentation "A list of prompts to be used by the mock client.")
   (%prompt-responses
    :initarg :prompt-responses
    :initform ()
    :accessor prompt-responses
    :type list
    :documentation "A list of responses to be used by the mock client.")))

(defclass mock-remote-llm-client (core:execution-node) ())
(defclass mock-remote-llm-interaction (core:execution-event)
  ((%client
    :initarg :client
    :initform (error "No client provided.")
    :accessor client)))

(defun make-mock-remote-llm-interaction (client)
  (make-instance 'mock-remote-llm-interaction
                 :client client
                 :input-keys (list :message-history :prompt
                                   :raw-chat-completion-request-message :raw-http-request)
                 :output-keys (list :message-history :prompt-response
                                    :raw-chat-completion-response-message :raw-http-response)))
#+(or) (defparameter *llm-interaction*
         (make-mock-remote-llm-interaction (make-instance 'mock-remote-llm-client)))

(defun %make-raw-chat-completion-request-message (prompt
                                                  &key (message-history nil))
       (list :prompt prompt :message-id "1234" :message-history message-history))

(defun %make-raw-chat-completion-response-message (prompt-response
                                                   &key (message-history nil))
         (list :prompt-response prompt-response :message-id "1234"
               :message-history message-history))

(defun make-raw-http-request (message-payload)
  (format nil "POST /chat HTTP/1.1~%Content-Type: application/json~%~%~a"
          ;; (json:encode-json-to-string message-payload)
          "{
            \"prompt\": \"Hello, world!\",
            \"message-id\": \"1234\",
            \"message-history\": []
          }"
          ))

(defun make-raw-http-response (message-payload)
  (format nil "HTTP/1.1 200 OK~%Content-Type: application/json~%~%~a"
          ;; (json:encode-json-to-string message-payload)
          "{
            \"prompt-response\": \"Hello, world!\",
            \"message-id\": \"1234\",
            \"message-history\": []
          }"
          ))


(defun %run-one-mock-llm-interaction (app prompt prompt-response)
  (let*  ((llm-client (make-instance 'mock-remote-llm-client))
          (llm-interaction (make-mock-remote-llm-interaction llm-client))
          (interaction-id-key (format nil "interaction-~a" (app:iteration-count app)))
          (message-history (list))
          (raw-chat-completion-request-message
            (%make-raw-chat-completion-request-message prompt))
          (raw-chat-completion-response-message
            (%make-raw-chat-completion-response-message prompt-response))
          (raw-http-request (make-raw-http-request raw-chat-completion-request-message))
          (raw-http-response (make-raw-http-response raw-chat-completion-response-message)))
    (setf (core:inputs llm-interaction) (list :message-history message-history
                                              :prompt prompt
                                              :raw-chat-completion-request-message
                                              raw-chat-completion-request-message
                                              :raw-http-request
                                              raw-http-request)
          (core:outputs llm-interaction) (list :message-history message-history
                                               :prompt-response
                                               :raw-chat-completion-response-message
                                               raw-chat-completion-response-message
                                               :raw-http-response
                                               raw-http-response)
          (core:lookup *in-mem-app-session* interaction-id-key) llm-interaction)))

(defmethod app:should-continue-running-p ((app mock-chat-app))
  (call-next-method)
  (let ((it-count (app:iteration-count app))
        (prompts-len (length (prompts app)))
        (prompt-responses-len (length (prompt-responses app))))
  (log:info "(< it-count ~a (1- (min prompts-len prompt-responses-len))) => ~a"
            it-count (< it-count (1- (min prompts-len prompt-responses-len))))
  (< it-count (1- (min prompts-len prompt-responses-len)))))

(defmethod run-loop-step ((app mock-chat-app) &rest args)
  (declare (ignore args))
  (let ((prompt (nth (app:iteration-count app) (prompts app)))
        (prompt-response (nth (app:iteration-count app) (prompt-responses app))))
    (log:info "Prompt: ~a" prompt)
    (log:info "Prompt response: ~a" prompt-response)
    (app:run-task app
                  (lambda () (%run-one-mock-llm-interaction app prompt prompt-response)))
    (app:process-task-queue app)))

(defmethod core:run :around ((app mock-chat-app) &rest args)
  (declare (ignore args))

  (call-next-method))

(defun make-basic-in-mem-app-context ()
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
    (values config store
            projects-store project-store project
            sessions-store session-store session)))

(defun make-in-mem-app (&key
                          (app-class 'simple-chat-app)
                          (start t start-provided-p)
                          (max-iterations 10)
                          (queue-timeout 0.1))
  (multiple-value-bind (config store
                        projects-store project-store project
                        sessions-store session-store session)
      (make-basic-in-mem-app-context)
    (let* ((app (make-instance app-class
                               :name "ephemeral-chat-app"
                               :description "Just a quick-n-dirty chat app for testing in the REPL."
                               :system-configuration config
                               :project project
                               :metadata-store store
                               :max-iterations max-iterations
                               :task-queue-timeout queue-timeout)))
      (core:register-system-configuration :default (system-configuration app))
      (setf *in-mem-app-proj* project
            *in-mem-app-session* session
            *in-mem-app* app)
      (log:info "Created in-mem app ~a" app)
      (when start (ai-project-tools/app:start app :project project :session session))
      app)))

(defun make-in-mem-chat-app (&key (start nil) prompts prompt-responses)
  (let ((app (make-in-mem-app :app-class 'mock-chat-app :start nil)))
    (setf (prompts app) prompts
          (prompt-responses app) prompt-responses)
    (when start
      (ai-project-tools/app:start app
                                  :project *in-mem-app-proj*
                                  :session *in-mem-app-session*))
    app))

(defparameter *chat-prompts-1* '("What is your name?"
                                 "What is your favorite color?"
                                 "What is your quest?"))
(defparameter *chat-prompt-responses-1* '("Sir Lancelot of Camelot"
                                          "Blue"
                                          "To seek the Holy Grail!"))
;; (make-in-mem-app)
#+(or)(make-in-mem-chat-app :prompts *chat-prompts-1*
                            :prompt-responses *chat-prompt-responses-1*)
#+(or)(ai-project-tools/app:start *in-mem-app*
                                  :project *in-mem-app-proj*
                                  :session *in-mem-app-session*)
;; (run-task *in-mem-app* (lambda () (log:info "Running this task in app ~a" core:*current-application*)))
;; (run-task *in-mem-app* (lambda () (error "Error to induce quit.")))
;; (stop *in-mem-app*)
;; (setf (running-p *in-mem-app*) nil)
;; (running-p *in-mem-app*)
;; (%shutdown-app *in-mem-app*)
(defparameter *app-specials* (list 'core::*current-application*
                                   'core::*current-system-configuration*
                                   'core::*current-metadata-store*
                                   'core::*root-metadata-store*
                                   'core::*current-project*
                                   'core::*current-session*))

(defun symbol-bound-and-not-null (sym)
  (and (boundp sym) (not (null (symbol-value sym)))))

(defun do-app-specials (fn)
  (dolist (x *app-specials*)
    (funcall fn x)))

(defun map-app-specials (fn)
  (mapcar fn *app-specials*))

(defun check-app-specials ()
  (every #'symbol-bound-and-not-null *app-specials*))

(defun log-app-specials ()
  (loop for x in *app-specials*
        do (log:info "Symbol ~a bound to ~a" x (symbol-value x))))

(defun log-app-specials-to-string ()
  (with-output-to-string (s)
    (loop for x in *app-specials*
          do (format s "Symbol ~a bound to ~a ~%" x (symbol-value x)))
    s))

#+(or)(setf *iterations* 0
            *queue-timeouts* 0)
;; (setf (lookup *in-mem-app-session* "some-session-key") "BLAH")
;; (lookup *in-mem-app-session* "some-session-key")
