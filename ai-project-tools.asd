(in-package :cl-user)

(defpackage ai-project-tools-asd
  (:use :cl :asdf))
(in-package :ai-project-tools-asd)

(asdf:defsystem #:ai-project-tools
  :description "AI Project Tools - Scaffolding and tools for AI/ML/LLM workflows, projects, experiments and applications."
  :version "0.1.0"
  :author "Joel Boehland <jboehland@gmail.com>"
  :license "Apache 2.0"
  :serial t
  :depends-on (:ai-project-tools/core))

(asdf:defsystem #:ai-project-tools/core
  :description "AI Project Tools - Core concept classes, protocols, and utilities"
  :version "0.1.0"
  :author "Joel Boehland <jboehland@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (:log4cl
               :alexandria
               :serapeum
               :iterate
               :closer-mop
               :fset
               :functional-trees
               :local-time)
  :components ((:module "src/core"
                :components
                ((:file "package")
                 (:file "variables")
                 (:file "utilities")
                 (:file "datatype-definitions")
                 (:file "protocols")
                 (:file "implementation"))))
  :in-order-to ((test-op (load-op "ai-project-tools/core-tests")))
  :perform (test-op (op c)
                    (unless
                        (uiop:symbol-call
                         :fiveam :run!
                         (uiop:find-symbol* :ai-project-tools/tests-suite
                                            :ai-project-tools/tests))
                      (error "test failure"))))

(defmethod perform :after ((op load-op) (sys (eql (find-system "ai-project-tools/core"))))
  (uiop:symbol-call
   :ai-project-tools/core :register-system-components :ai-project-tools/core))

(asdf:defsystem #:ai-project-tools/core-tests
  :depends-on (#:ai-project-tools/core :fiveam)
  :serial t
  :components ((:module "test"
                :components
                ((:file "package")
                 (:file "ai-project-tools-tests")))
               (:module "test/core"
                :components
                ((:file "package")
                 (:file "core-tests"))))
  :perform (test-op (op c)
                    (uiop:symbol-call
                     :fiveam :run!
                     (uiop:find-symbol* :ai-project-tools/tests-suite
                                        :ai-project-tools/tests))))
