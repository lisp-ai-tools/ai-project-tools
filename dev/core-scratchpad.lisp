(in-package #:ai-project-tools/core)

(defvar *test-app* nil)

(defclass test-parent-task-node (execution-tree-node has-name) ())
(defclass test-child-task-node (execution-node has-name)
  ((transform-fn :initarg :transform-fn :accessor transform-fn)))

(defun %mock-prompt-transform-fn (prompt output-key)
  (list output-key (format nil "##~A" prompt)))
;;(%mock-prompt-transform-fn "foo" :llm-prepared-prompt)

(defun %make-prompt-artifact (prompt-text
                              &key
                                (name (format nil "prompt-~A" prompt-text) name-provided-p)
                                (description (format nil "A simple prompt: prompt-~A" prompt-text) description-provided-p)
                                (metadata nil metadata-provided-p))

  (make-instance 'artifact :data prompt-text :name name :description description :metadata metadata))
;;(defparameter *pa* (%make-prompt-artifact "foo"))

(defun %make-test-child-task-node (transform-fn input-keys output-keys)
  (let ((execution-event (make-instance 'execution-event :input-keys input-keys :output-keys output-keys)))
    (make-instance 'test-child-task-node
                   :execution-event execution-event
                   :transform-fn transform-fn)))

(defmethod run ((node test-child-task-node) &rest args)
  (let ((execution-event (execution-event node)))
    (apply (transform-fn node) (input-args execution-event))))


#+(or)(let ((node (%make-test-child-task-node
                  '%mock-prompt-transform-fn
                  '(:llm-raw-prompt)
                  '(:llm-prepared-prompt))))
        (run node))
;;;;
;;;; We need a mapping between input keys and output keys.
;;;; A single input key can map to multiple output keys.
;;;;
;;;;


#+(or) (progn (journal:jtrace run))
#+(or) (journal:with-journaling (:record t)
         (journal:journaled (node-journal)
           (let ((parent-node (%make-test-parent-task-node)))
             (run parent-node))
           (journal:list-events)))

#+(or)(let ((bundle (journal:make-file-bundle "/tmp/user-example/"))
            (parent-node (%make-test-parent-task-node)))
        (format t "Recording")
        (journal:with-bundle (bundle)
          (run parent-node))
        (format t "~%Replaying")
        (journal:with-bundle (bundle)
          (run parent-node)))
