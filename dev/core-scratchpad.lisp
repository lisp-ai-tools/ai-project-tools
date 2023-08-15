(in-package #:ai-project-tools/core)

(defvar *test-app* nil)

(defun make-nested-stores ()
  (let* ((store (make-instance 'memory-scoped-metadata-store
                               :name "/root/"
                               :parent nil
                               :schema (list :fake :schema)
                               :store (make-hash-table :test #'equal)))
         (child-store (make-instance 'memory-scoped-metadata-store
                                     :name "child/"
                                     :parent store
                                     :schema (list :fake :schema))))
    (values store child-store)))
