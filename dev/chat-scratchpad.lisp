(in-package #:ai-project-tools/core)

;; (ql:quickload '(:schemata))


(defclass llm () ())

(defclass persistent-execution-node-mixin (execution-node) ())

(defclass llm-interaction-node (execution-node has-name)
  ((%llm
    :initarg :llm
    :reader llm)))

(defclass one-shot-llm-chat-completion-node (llm-interaction-node) ())

(defclass looping-llm-chat-node (llm-interaction-node
                                 execution-tree-node
                                 stepped-runnable
                                 has-name)
  ((%chat-queue
    :accessor %chat-queue)
   (%chat-queue-capacity
    :initarg :chat-queue-capacity
    :reader chat-queue-capacity
    :initform 5)
   (%chat-queue-timeout
    :initarg :chat-queue-timeout
    :reader chat-queue-timeout
    :initform 5)))

(define-condition llm-chat-quit-condition (condition) ())

(defmethod run ((node llm-interaction-node) &rest args))

(defgeneric chat (chat-node prompt &rest args))

(defmethod chat ((node looping-llm-chat-node) (msg string) &rest args)
  (lparallel.queue:push-queue msg (%chat-queue node)))

(defmethod chat ((node looping-llm-chat-node) (msg (eql :quit)) &rest args)
  (lparallel.queue:push-queue msg (%chat-queue node)))

(defmethod run ((node looping-llm-chat-node) &rest args)
  "A looping-llm-chat-node run method sets up a queue where prompts are placed on a
queue from within the chat method. The chat prompt is then processed in the
run-step method. If we get an interrupt or :quit message on the queue, we end
the listening loop."
  (let ((chat-queue (lparallel.queue:make-queue
                     :fixed-capacity (chat-queue-capacity node))))
    (setf (%chat-queue node) chat-queue)
    (loop :with llm = (llm node)
          :with timeout = (chat-queue-timeout node)
          :with chat-message :and chat-message-provided-p
          :do (setf (values chat-message chat-message-provided-p)
                    (lparallel.queue:try-pop-queue chat-queue :timeout timeout))
          :when chat-message-provided-p
            :do (run-step node :prompt chat-message)
          )))

(defmethod run-step ((node looping-llm-chat-node) &rest args &key prompt)
  (log:info "Running step for ~a, with prompt: ~a" node prompt)
  (when (eql prompt :quit)
    (log:info "Quitting ~a" node)
    (signal 'llm-chat-quit-condition :report "User requested quit")))

#+(or)(defparameter *chat-node* (make-instance 'looping-llm-chat-node
                                               :name "test-chat"
                                               :llm (make-instance 'llm)))
(defun test-chat (&optional (chat-node *chat-node*))
  (flet ((run-chat-node () (run chat-node)))
    (let ((chat-thread (bt:make-thread #'run-chat-node :name "chat-thread"))
          (messages '("Hello" "Goodbye" :quit)))
      (loop :for msg :in messages
            :do (sleep 1)
            :do (chat chat-node msg))
      (bt:join-thread chat-thread)
      (chat chat-node "This should not be printed"))))
;; (test-chat)
