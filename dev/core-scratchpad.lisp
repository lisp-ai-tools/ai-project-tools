(in-package #:ai-project-tools/core)

(defvar *test-app* nil)

(defclass test-parent-task-node (execution-tree-node has-name) ())
(defclass test-child-task-node (execution-node has-name) ())

(defun %make-test-parent-task-node ()
  (make-instance 'test-parent-task-node
                 :name "parent"
                 :children (list (make-instance 'test-child-task-node
                                                :name "child-a"
                                                :input-keys '("KEY-child-a")
                                                :output-keys '("VAL-child-a"))
                                 (make-instance 'test-child-task-node
                                                :name "child-b"
                                                :input-keys '("KEY-child-b")
                                                :output-keys '("VAL-child-b"))
                                 (make-instance 'test-child-task-node
                                                :name "child-c"
                                                :input-keys '("KEY-child-c")
                                                :output-keys '("VAL-child-c")))))

#+(or) (defparameter *test-parent-task-node* (%make-test-parent-task-node))
#+(or) (defparameter *test-root-store* nil)

(defmethod run ((node test-child-task-node) &rest args)
  (let ((key (format nil "KEY-~a" (name node)))
        (val (format nil "Hi, my name is: ~a~%" (name node))))
    (log:info "node: ~a" node)
  (setf (lookup *current-metadata-store* key) val)))

(defun %run-child-task-nodes (parent-node)
  (flet ((%run-child-task-node (child-node)
           (let* ((store (make-instance 'memory-scoped-metadata-store
                                        :name (name child-node)
                                        :parent *test-root-store*
                                        :schema (list :fake :schema)))
                  (*current-metadata-store* store))
             (format t "Running ~a, with store: ~a~%" (name child-node) store)
             (run child-node))))
    (map nil #'%run-child-task-node (children parent-node))))

(defmethod run ((node test-parent-task-node) &rest args)
  (setf *test-root-store* nil)
  (let* ((store (make-instance 'memory-scoped-metadata-store
                               :name "root"
                               :parent nil
                               :schema (list :fake :schema)))
         (*root-metadata-store* store)
         (*current-metadata-store* store))
    (setf *test-root-store* store)
    (format t "Running ~a, with store: ~a~%" (name node) store)
    (%run-child-task-nodes node)))

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
