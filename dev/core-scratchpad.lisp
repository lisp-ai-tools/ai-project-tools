(in-package #:ai-project-tools/core)

(defvar *test-app* nil)

(progn
  (setf *test-app* nil)
  (let* ((config (make-instance 'simple-in-memory-system-configuration
                                :designator :ephemeral-chat-config
                                :name "Simple in-memory system configuration."
                                :description "A simple in-memory system configuration that does not persist."))
         (store (make-instance 'memory-scoped-metadata-store
                               :name "/root/"
                               :parent nil
                               :schema (list :fake :schema)
                               :store (make-hash-table :test #'equal)))
         (project-store (make-instance 'memory-scoped-metadata-store
                                       :name "/ephemeral-chat-project/"
                                       :parent store
                                       :schema (list :fake :schema)))
         (project (make-instance 'project
                                 :name "ephemeral-chat-project"
                                 :description "Project for a quick-n-dirty chat app for testing in the REPL."
                                 :metadata-store project-store))
         (app (make-instance 'application
                             :name "ephemeral-chat-app"
                             :description "Just a quick-n-dirty chat app for testing in the REPL."
                             :system-configuration config
                             :project project
                             :root-metadata-store store)))
    (setf *test-app* app))
  )

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

#+(or) (defparameter *root-store* nil)
#+(or) (defparameter *child-store* nil)

#+(or) (multiple-value-bind (store child) (make-nested-stores)
                             (setf *root-store* store
                                   *child-store* child)
         (setf (lookup *root-store* :foo) "foo"
               (lookup *child-store* :bar) "bar"))
;; (ft:children *root-store*)
;; (children *root-store*)
;; (parent *child-store*)
;; (ft:child-slot-specifiers *root-store*)
;; (setf (children *root-store*) (pushnew *child-store* (children *root-store*)))
;; (format nil "~a" *root-store*)
;; (describe *root-store*)
;; (ft:children-alist *root-store*)
;; (ft:parent *root-store* *child-store*)
;; (ft:parent *child-store* *root-store*)
;; (ft:path-of-node *root-store* *child-store*)
;; (ft::lookup *root-store* '(:children))
;; (ft::lookup *root-store* '(:children . 0))
;; (ft::lookup *root-store* '(:children 0 . :store))
;; (ft::lookup *root-store* '(:children 0 :store :bar
