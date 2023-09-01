(in-package #:ai-project-tools/app-tests)


(defparameter *in-mem-app* nil)
(defparameter *in-mem-app-proj* nil)
(defparameter *in-mem-app-session* nil)
;;(describe *in-mem-app*)
;;(scoped-path (metadata-store (project *in-mem-app*)))

(defun %make-raw-chat-completion-request-message (prompt
                                                  &key (message-history nil))
       (list :prompt prompt :message-id "1234" :message-history message-history))

(defun %make-raw-chat-completion-response-message (prompt-response
                                                   &key (message-history nil))
         (list :prompt-response prompt-response :message-id "1234"
               :message-history message-history))

(defun make-raw-http-request (message-payload)
  (format nil "POST /chat HTTP/1.1~%Content-Type: application/json~%~%~a"
          "{
            \"prompt\": \"Hello, world!\",
            \"message-id\": \"1234\",
            \"message-history\": []
          }"
          ))

(defun make-raw-http-response (message-payload)
  (format nil "HTTP/1.1 200 OK~%Content-Type: application/json~%~%~a"
          "{
            \"prompt-response\": \"Hello, world!\",
            \"message-id\": \"1234\",
            \"message-history\": []
          }"
          ))

(defclass llm-configuration (core:configuration-set) ())
(defclass remote-llm-client (core:configuration-set) ())
(defclass remote-llm-configuration (llm-configuration)
  ((%client
    :initarg :client
    :initform (error "No client provided.")
    :accessor client
    :type remote-llm-client)))

(defun make-mock-llm-configuration (&key (client (make-instance 'remote-llm-client)))
  (make-instance 'remote-llm-configuration :client client))
;; (make-mock-llm-configuration)
(defclass remote-llm-interaction (core:execution-node) ())

(defun make-mock-remote-llm-interaction-event (&key (session core:*current-session*))
  (make-instance 'core:execution-event
                 :session session
                 :input-keys (list
                              :llm-configuration
                              :message-history
                              :prompt
                              :raw-chat-completion-request-message
                              :raw-http-request)
                 :output-keys (list
                               :start-time
                               :end-time
                               :duration
                               :message-history
                               :prompt-response
                               :raw-chat-completion-response-message
                               :raw-http-response)))

(defgeneric build-completion-request-payload (llm prompt format-designator &rest args)
  (:documentation
   "Build a completion request payload for the given LLM in the given format (JSON,
XML, YAML etc.)."))

(defclass simple-chat-app (base-lparallel-application)
  ((%llm-configuration
    :initarg :llm-configuration
    :initform nil
    :accessor llm-configuration
    :documentation "The llm-configuration to be used by the app."))
  (:documentation
   "A simple chat app. This is an app that uses a single remote LLM endoint with a
default configuration. It also contains a remote client that can speak to the
various LLM endpoints."))

(defclass mock-chat-app (simple-chat-app)
  ())

(defun %get-prompts ()
  (alexandria:when-let ((session (when (boundp 'core:*current-session*)
                                   core:*current-session*)))
    (core:lookup session "prompts")))
;;(%get-prompts)
(defun %get-prompt-responses ()
  (alexandria:when-let ((session (when (boundp 'core:*current-session*)
                                   core:*current-session*)))
    (core:lookup session "prompt-responses")))

(defun %get-prompt-at-index (index)
  (alexandria:when-let ((prompts (%get-prompts)))
    (nth index prompts)))
;;(%get-prompt-at-index 0)
(defun %get-prompt-response-at-index (index)
  (alexandria:when-let ((prompt-responses (%get-prompt-responses)))
    (nth index prompt-responses)))

(defun %get-prompts-length ()
  (let ((prompts (%get-prompts))
        (prompt-responses (%get-prompt-responses)))
    (if (and prompts prompt-responses)
        (min (length prompts) (length prompt-responses))
        0)))

;;(%get-prompts-length)
(defun %get-history (current-iteration)
  (let ((prompts (%get-prompts))
        (prompt-responses (%get-prompt-responses)))
    (when (and prompts prompt-responses)
      (loop :for i :from 0 :below (min (length prompts) (length prompt-responses) current-iteration)
            :if (and (nth i prompts) (nth i prompt-responses))
            :collect (list (nth i prompts) (nth i prompt-responses))))))
;;(%get-history 0)

(defgeneric %chat (app prompt &rest args)
  (:documentation "A generic function for chatting with the LLM."))

(defmethod %chat ((app simple-chat-app) prompt &rest args))

(defmethod %chat ((app mock-chat-app) prompt &rest args)
  ;; Note -- the args could be used to alter the configuration of the LLM.
  (let* ((llm-interaction-event (make-mock-remote-llm-interaction-event))
         (iteration-count (app:iteration-count app))
         (history (%get-history iteration-count))
         (prompt-response (%get-prompt-response-at-index iteration-count)))
    (log:info "Iteration count: ~a, history: ~a, prompt: ~a, prompt-response: ~a"
              iteration-count history prompt prompt-response)
    llm-interaction-event))

(defmethod app:should-continue-running-p ((app mock-chat-app))
  (call-next-method)
  (let ((it-count (app:iteration-count app))
        (prompts-len (%get-prompts-length)))
  (log:info "it-count ~a prompts-len: => ~a, SHOULD?: ~a"
            it-count prompts-len (< it-count (1- prompts-len)))
    (< it-count (1- prompts-len))))

(defmethod run-loop-step ((app mock-chat-app) &rest args)
  (declare (ignore args))
  (let ((prompt (%get-prompt-at-index (app:iteration-count app)))
        (result-channel (lp:make-channel)))
    (app:run-task app (lambda () (%chat app prompt)) :result-channel result-channel)
    (app:process-task-queue app)
    (lp:receive-result result-channel)))

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

(defun make-mock-in-mem-chat-app (&key (start nil) prompts prompt-responses)
  (let ((app (make-in-mem-app :app-class 'mock-chat-app :start nil))
        (llm-configuration (make-mock-llm-configuration))
        (session *in-mem-app-session*))
    (setf (core:lookup session "prompts") prompts
          (core:lookup session "prompt-responses") prompt-responses
          (llm-configuration app) llm-configuration)
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
#+(or)(make-mock-in-mem-chat-app :prompts *chat-prompts-1*
                                 :prompt-responses *chat-prompt-responses-1*
                                 :start t)
;; (make-mock-llm-configuration)
#+(or)(ai-project-tools/app:start *in-mem-app*
                                  :project *in-mem-app-proj*
                                  :session *in-mem-app-session*)
;; (run-task *in-mem-app* (lambda () (log:info "Running this task in app ~a" core:*current-application*)))
;; (stop *in-mem-app*)
;; (setf (running-p *in-mem-app*) nil)
;; (app:running-p *in-mem-app*)
;; (%shutdown-app *in-mem-app*)
