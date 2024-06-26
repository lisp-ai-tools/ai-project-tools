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
  :depends-on (:ai-project-tools/core :ai-project-tools/app)
  :in-order-to ((test-op (load-op "ai-project-tools/tests")))
  :perform (test-op (op c)
                    (unless
                        (uiop:symbol-call
                         :fiveam :run!
                         (uiop:find-symbol* :ai-project-tools/tests-suite
                                            :ai-project-tools/tests))
                      (error "test failure"))))

(asdf:defsystem #:ai-project-tools/tests
  :depends-on (#:fiveam #:ai-project-tools/core-tests #:ai-project-tools/app-tests)
  :serial t
  :components ((:module "test"
                :components
                ((:file "package")
                 (:file "ai-project-tools-tests"))))
  :perform (test-op (op c)
                    (uiop:symbol-call
                     :fiveam :run!
                     (uiop:find-symbol* :ai-project-tools/tests-suite
                                        :ai-project-tools/tests))))

(asdf:defsystem #:ai-project-tools/ml-metadata
  :description "AI Project Tools - Google ml-metadata integration"
  :version "0.1.0"
  :author "Joel Boehland <jboehland@gmail.com>"
  :license "MIT"
  :serial t
  :defsystem-depends-on (:cl-protobufs.asdf)
  :depends-on (#:log4cl
               #:alexandria
               #:serapeum
               #:split-sequence
               #:cl-protobufs
               )
  :components ((:module "ml-metadata-protobufs"
                :serial t
                :pathname "src/ml-metadata/"
                :components
                (
                 (:protobuf-source-file "metadata-source"
                  :proto-pathname "../../data/proto/ml_metadata/proto/metadata_source.proto"
                  ;; :proto-search-path ("../../data/proto/")
                  )
                 ;; :proto-search-path ("data/proto/" "data/proto/google/protobuf/"))
                 (:protobuf-source-file "metadata-store"
                  :proto-pathname "../../data/proto/ml_metadata/proto/metadata_store.proto"
                  ;; :proto-search-path ("../../data/proto/")
                  )
                 ;; :proto-search-path ("data/proto/" "data/proto/google/protobuf/"))
                 (:protobuf-source-file "metadata-store-service"
                  :proto-pathname "../../data/proto/ml_metadata/proto/metadata_store_service.proto"
                  :proto-search-path ("../../data/proto/")
                  ;; :proto-search-path ("data/proto/" "data/proto/google/protobuf/")
                  )
                ))))



(asdf:defsystem #:ai-project-tools/core
  :description "AI Project Tools - Core concept classes, protocols, and utilities"
  :version "0.1.0"
  :author "Joel Boehland <jboehland@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (#:log4cl
               #:alexandria
               #:serapeum
               #:split-sequence
               #:iterate
               #:closer-mop
               #:local-time
               #:fset
               #:ironclad
               #:lparallel
               #:journal)
  :components ((:module "src/core"
                :components
                ((:file "package")
                 (:file "variables")
                 (:file "utilities")
                 (:file "datatype-definitions")
                 (:file "protocols")
                 (:file "in-memory-metadata-store")
                 (:file "implementation"))))
  :in-order-to ((test-op (load-op "ai-project-tools/core-tests")))
  :perform (test-op (op c)
                    (unless
                        (uiop:symbol-call
                         :fiveam :run!
                         (uiop:find-symbol* :ai-project-tools/core-tests-suite
                                            :ai-project-tools/core-tests))
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
                 (:file "fixtures")
                 (:file "core-tests"))))
  :perform (test-op (op c)
                    (uiop:symbol-call
                     :fiveam :run!
                     (uiop:find-symbol* :ai-project-tools/core-tests-suite
                                        :ai-project-tools/core-tests))))


(asdf:defsystem #:ai-project-tools/app
  :description "AI Project Tools - Application scaffolding. Lifecycles, configuration and more."
  :version "0.1.0"
  :author "Joel Boehland <jboehland@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (:log4cl
               :alexandria
               :serapeum
               :iterate
               :closer-mop
               :local-time
               :ironclad
               :lparallel
               :ai-project-tools/core)
  :components ((:module "src/app"
                :components
                ((:file "package")
                 (:file "latch")
                 (:file "variables")
                 (:file "datatype-definitions")
                 (:file "protocols")
                 (:file "implementation"))))
  :in-order-to ((test-op (load-op "ai-project-tools/app-tests")))
  :perform (test-op (op c)
                    (unless
                        (uiop:symbol-call
                         :fiveam :run!
                         (uiop:find-symbol* :ai-project-tools/app-tests-suite
                                            :ai-project-tools/app-tests))
                      (error "test failure"))))

(asdf:defsystem #:ai-project-tools/app-tests
  :depends-on (#:ai-project-tools/app :fiveam)
  :serial t
  :components ((:module "test"
                :components
                ((:file "package")
                 (:file "ai-project-tools-tests")))
               (:module "test/app"
                :components
                ((:file "package")
                 (:file "fixtures")
                 (:file "app-tests"))))
  :perform (test-op (op c)
                    (uiop:symbol-call
                     :fiveam :run!
                     (uiop:find-symbol* :ai-project-tools/app-tests-suite
                                        :ai-project-tools/app-tests))))

#+(or) (ql:quickload :ai-project-tools)
