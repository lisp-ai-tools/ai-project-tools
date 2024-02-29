(in-package #:ai-project-tools/core)

(defvar *test-app* nil)
(defvar *last-artifact* nil)

(defclass test-parent-task-node (execution-tree-node has-name) ())
(defclass test-child-task-node (execution-node has-name)
  ((transform-fn :initarg :transform-fn :accessor transform-fn)))

(defun %mock-prompt-transform-fn (&key llm-raw-prompt)
  (format nil "###~A" llm-raw-prompt))
;;(%mock-prompt-transform-fn "foo" :llm-prepared-prompt)

(defun %make-prompt-artifact (prompt-text
                              &key
                                (name nil name-provided-p)
                                (description (format nil "A simple prompt: prompt-~A" prompt-text) description-provided-p)
                                (metadata nil metadata-provided-p))
  (make-instance 'artifact :data prompt-text :name name
                           :description description :metadata metadata))
;;(defparameter *pa* (%make-prompt-artifact "foo"))

(defun %make-prepared-prompt-artifact (raw-prompt
                                       &key
                                         (name nil name-provided-p)
                                         (description (format nil "A prepared prompt: -~A" (data raw-prompt)) description-provided-p)
                                         (metadata nil metadata-provided-p))
  (let ((prompt-text (format nil "###~A" (data raw-prompt))))
    (make-instance 'artifact :data prompt-text :name name
                             :description description :metadata metadata)))
;;(defparameter *ppa* (%make-prepared-prompt-artifact *pa*))

(defun %make-test-child-task-node (transform-fn input-keys output-keys)
  (let ((execution-event (make-instance 'execution-event :input-keys input-keys
                                                         :output-keys output-keys)))
    (make-instance 'test-child-task-node
                   :execution-event execution-event
                   :transform-fn transform-fn)))

(defun %get-execution-args (execution-event)
  "Returns a list of (name input) pairs for each input in the execution event."
  (let ((args-plist))
    (loop :for input :in (inputs execution-event)
          :do (push (name input) args-plist)
              (push input args-plist))
    (nreverse args-plist)))

;;(alexandria:alist-plist '((:a 1) (:b 2)))

(defun %add-input (execution-event input)
  (let ((inputs (inputs execution-event)))
    (setf (inputs execution-event) (append inputs (list input)))))

(defmethod run ((node test-child-task-node) &rest args)
  (let* ((prompt (or (getf args :prompt) (error "No prompt provided")))
         (prompt-artifact (%make-prompt-artifact prompt :name :llm-raw-prompt))
         (execution-event (execution-event node)))
    (%add-input execution-event prompt-artifact)
    (list :llm-prepared-prompt
          (%make-prepared-prompt-artifact prompt-artifact :name :llm-prepared-prompt))))

(defun %run-mock-prompt-task-node ()
  (journal:jtrace run)
  (journal:with-journaling (:record t)
    (journal:journaled (node-journal)
      (let ((node (%make-test-child-task-node
                   '%mock-prompt-transform-fn
                   '(:llm-raw-prompt)
                   '(:llm-prepared-prompt))))
        (setf *last-artifact* (run node :prompt "Hi there")))
      (journal:list-events))))
;;(%run-mock-prompt-task-node)


(defun %run-file-journaled-prompt-task-node ()
  (let ((bundle (journal:make-file-bundle "/tmp/prompt-node-example/"))
        (node (%make-test-child-task-node
                   '%mock-prompt-transform-fn
                   '(:llm-raw-prompt)
                   '(:llm-prepared-prompt))))
    (log:info "Recording")
    (journal:with-bundle (bundle)
      (run node :prompt "Hi from journaled node!"))
    (log:info "~%Replaying")
    (journal:with-bundle (bundle)
      (run node :prompt "Hi from journaled node!"))))
;; (%run-file-journaled-prompt-task-node)

#+(or)(let ((node (%make-test-child-task-node
                   '%mock-prompt-transform-fn
                   '(:llm-raw-prompt)
                   '(:llm-prepared-prompt))))
        (setf *last-artifact* (run node :prompt "Hi there")))

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
