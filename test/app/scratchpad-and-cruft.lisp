
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

(defgeneric %push-execution-node (app execution-node)
  (:documentation "Pushes an execution node onto the execution node stack."))
(defgeneric %pop-execution-node (app)
  (:documentation "Pops an execution node off the execution node stack."))
(defmacro with-execution-node (app execution-node &body body)
  "Pushes an execution node onto the execution node stack, executes the body,
and then pops the execution node off the execution node stack."
  `(progn
     (%push-execution-node ,app ,execution-node)
     (unwind-protect
          (progn ,@body)
       (%pop-execution-node ,app))))



#+(or) (defparameter *llm-interaction*
         (make-mock-remote-llm-interaction (make-instance 'mock-remote-llm-client)))

#+(or)(defun %run-one-mock-llm-interaction (app prompt prompt-response)
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
