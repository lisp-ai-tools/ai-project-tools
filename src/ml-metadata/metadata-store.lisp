;;; metadata_store.proto.lisp
;;;
;;; Generated by the protocol buffer compiler. DO NOT EDIT!

(cl:in-package #:common-lisp-user)

#+sbcl
(cl:progn
 (cl:eval-when (:compile-toplevel) (sb-ext:restrict-compiler-policy 'cl:debug 0 1))
 (cl:declaim (cl:optimize (sb-c:store-coverage-data 0))))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:unless (cl:find-package "CL-PROTOBUFS.ML-METADATA.PROTO")
    (cl:defpackage "CL-PROTOBUFS.ML-METADATA.PROTO" (:use)
                   (:local-nicknames (#:pi #:cl-protobufs.implementation)))))

(cl:in-package "CL-PROTOBUFS.ML-METADATA.PROTO")

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
(pi:define-schema 'metadata_store
    :package "ml_metadata.proto"
     :import '("google/protobuf/any.proto"
              "google/protobuf/struct.proto"
              "google/protobuf/descriptor.proto"))
)


;;; Top-Level enums

(pi:define-enum property-type
    ()
  (:unknown :index 0)
  (:int :index 1)
  (:double :index 2)
  (:string :index 3)
  (:struct :index 4)
  (:proto :index 5)
  (:boolean :index 6))

;;; Top-Level messages

(pi:define-message system-type-extension
    ()
  ;; Fields
  (type-name
   :index 1 :type cl:string :kind :scalar :label (:optional) :json-name "typeName"))

(pi:define-message value
    ()
  ;; Fields
  (pi:define-oneof value ()
    (int-value
     :index 1 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "intValue")
    (double-value
     :index 2 :type cl:double-float :kind :scalar :label (:optional) :json-name "doubleValue")
    (string-value
     :index 3 :type cl:string :kind :scalar :label (:optional) :json-name "stringValue")
    (struct-value
     :index 4 :type cl-protobufs.google.protobuf::struct :kind :message :label (:optional) :json-name "structValue")
    (proto-value
     :index 5 :type cl-protobufs.google.protobuf::any :kind :message :label (:optional) :json-name "protoValue")
    (bool-value
     :index 6 :type cl:boolean :kind :scalar :label (:optional) :json-name "boolValue")))

(pi:define-message artifact
    ()
  ;; Nested enums

  (pi:define-enum artifact.state
      ()
    (:unknown :index 0)
    (:pending :index 1)
    (:live :index 2)
    (:marked-for-deletion :index 3)
    (:deleted :index 4)
    (:abandoned :index 5)
    (:reference :index 6))
  ;; Nested messages
  ;; Fields
  (id
   :index 1 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "id")
  (name
   :index 7 :type cl:string :kind :scalar :label (:optional) :json-name "name")
  (type-id
   :index 2 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "typeId")
  (type
   :index 8 :type cl:string :kind :scalar :label (:optional) :json-name "type")
  (uri
   :index 3 :type cl:string :kind :scalar :label (:optional) :json-name "uri")
  (external-id
   :index 11 :type cl:string :kind :scalar :label (:optional) :json-name "externalId")
  (pi:define-map properties
     :key-type cl:string
     :value-type value
     :json-name "properties"
     :value-kind :message
     :index 4)
  (pi:define-map custom-properties
     :key-type cl:string
     :value-type value
     :json-name "customProperties"
     :value-kind :message
     :index 5)
  (state
   :index 6 :type artifact.state :kind :enum :label (:optional) :json-name "state" :default :unknown)
  (create-time-since-epoch
   :index 9 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "createTimeSinceEpoch")
  (last-update-time-since-epoch
   :index 10 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "lastUpdateTimeSinceEpoch")
  (system-metadata
   :index 12 :type cl-protobufs.google.protobuf::any :kind :message :label (:optional) :json-name "systemMetadata"))

(pi:define-message artifact-type
    ()
  ;; Nested enums

  (pi:define-enum artifact-type.system-defined-base-type
      ()
    (:unset :index 0)
    (:dataset :index 1)
    (:model :index 2)
    (:metrics :index 3)
    (:statistics :index 4))
  ;; Nested messages
  ;; Fields
  (id
   :index 1 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "id")
  (name
   :index 2 :type cl:string :kind :scalar :label (:optional) :json-name "name")
  (version
   :index 4 :type cl:string :kind :scalar :label (:optional) :json-name "version")
  (description
   :index 5 :type cl:string :kind :scalar :label (:optional) :json-name "description")
  (external-id
   :index 7 :type cl:string :kind :scalar :label (:optional) :json-name "externalId")
  (pi:define-map properties
     :key-type cl:string
     :value-type property-type
     :json-name "properties"
     :value-kind :enum
     :index 3
     :val-default :unknown)
  (base-type
   :index 6 :type artifact-type.system-defined-base-type :kind :enum :label (:optional) :json-name "baseType" :default :unset))

(pi:define-message event
    ()
  ;; Nested enums

  (pi:define-enum event.type
      ()
    (:unknown :index 0)
    (:declared-output :index 1)
    (:declared-input :index 2)
    (:input :index 3)
    (:output :index 4)
    (:internal-input :index 5)
    (:internal-output :index 6)
    (:pending-output :index 7))
  ;; Nested messages

  (pi:define-message event.path
      ()
    ;; Nested messages

    (pi:define-message event.path.step
        ()
      ;; Fields
      (pi:define-oneof value ()
        (index
         :index 1 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "index")
        (key
         :index 2 :type cl:string :kind :scalar :label (:optional) :json-name "key")))
    ;; Fields
    (steps
     :index 1 :type event.path.step :kind :message :label (:repeated :list) :json-name "steps"))
  ;; Fields
  (artifact-id
   :index 1 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "artifactId")
  (execution-id
   :index 2 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "executionId")
  (path
   :index 3 :type event.path :kind :message :label (:optional) :json-name "path")
  (type
   :index 4 :type event.type :kind :enum :label (:optional) :json-name "type" :default :unknown)
  (milliseconds-since-epoch
   :index 5 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "millisecondsSinceEpoch")
  (system-metadata
   :index 6 :type cl-protobufs.google.protobuf::any :kind :message :label (:optional) :json-name "systemMetadata"))

(pi:define-message execution
    ()
  ;; Nested enums

  (pi:define-enum execution.state
      ()
    (:unknown :index 0)
    (:new :index 1)
    (:running :index 2)
    (:complete :index 3)
    (:failed :index 4)
    (:cached :index 5)
    (:canceled :index 6))
  ;; Nested messages
  ;; Fields
  (id
   :index 1 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "id")
  (name
   :index 6 :type cl:string :kind :scalar :label (:optional) :json-name "name")
  (type-id
   :index 2 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "typeId")
  (type
   :index 7 :type cl:string :kind :scalar :label (:optional) :json-name "type")
  (external-id
   :index 10 :type cl:string :kind :scalar :label (:optional) :json-name "externalId")
  (last-known-state
   :index 3 :type execution.state :kind :enum :label (:optional) :json-name "lastKnownState" :default :unknown)
  (pi:define-map properties
     :key-type cl:string
     :value-type value
     :json-name "properties"
     :value-kind :message
     :index 4)
  (pi:define-map custom-properties
     :key-type cl:string
     :value-type value
     :json-name "customProperties"
     :value-kind :message
     :index 5)
  (create-time-since-epoch
   :index 8 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "createTimeSinceEpoch")
  (last-update-time-since-epoch
   :index 9 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "lastUpdateTimeSinceEpoch")
  (system-metadata
   :index 11 :type cl-protobufs.google.protobuf::any :kind :message :label (:optional) :json-name "systemMetadata"))

(pi:define-message execution-type
    ()
  ;; Nested enums

  (pi:define-enum execution-type.system-defined-base-type
      ()
    (:unset :index 0)
    (:train :index 1)
    (:transform :index 2)
    (:process :index 3)
    (:evaluate :index 4)
    (:deploy :index 5))
  ;; Nested messages
  ;; Fields
  (id
   :index 1 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "id")
  (name
   :index 2 :type cl:string :kind :scalar :label (:optional) :json-name "name")
  (version
   :index 6 :type cl:string :kind :scalar :label (:optional) :json-name "version")
  (description
   :index 7 :type cl:string :kind :scalar :label (:optional) :json-name "description")
  (external-id
   :index 9 :type cl:string :kind :scalar :label (:optional) :json-name "externalId")
  (pi:define-map properties
     :key-type cl:string
     :value-type property-type
     :json-name "properties"
     :value-kind :enum
     :index 3
     :val-default :unknown)
  (input-type
   :index 4 :type artifact-struct-type :kind :message :label (:optional) :json-name "inputType")
  (output-type
   :index 5 :type artifact-struct-type :kind :message :label (:optional) :json-name "outputType")
  (base-type
   :index 8 :type execution-type.system-defined-base-type :kind :enum :label (:optional) :json-name "baseType" :default :unset))

(pi:define-message context-type
    ()
  ;; Nested enums

  (pi:define-enum context-type.system-defined-base-type
      ()
    (:unset :index 0))
  ;; Nested messages
  ;; Fields
  (id
   :index 1 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "id")
  (name
   :index 2 :type cl:string :kind :scalar :label (:optional) :json-name "name")
  (version
   :index 4 :type cl:string :kind :scalar :label (:optional) :json-name "version")
  (description
   :index 5 :type cl:string :kind :scalar :label (:optional) :json-name "description")
  (external-id
   :index 7 :type cl:string :kind :scalar :label (:optional) :json-name "externalId")
  (pi:define-map properties
     :key-type cl:string
     :value-type property-type
     :json-name "properties"
     :value-kind :enum
     :index 3
     :val-default :unknown)
  (base-type
   :index 6 :type context-type.system-defined-base-type :kind :enum :label (:optional) :json-name "baseType" :default :unset))

(pi:define-message context
    ()
  ;; Nested messages
  ;; Fields
  (id
   :index 1 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "id")
  (name
   :index 3 :type cl:string :kind :scalar :label (:optional) :json-name "name")
  (type-id
   :index 2 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "typeId")
  (type
   :index 6 :type cl:string :kind :scalar :label (:optional) :json-name "type")
  (external-id
   :index 9 :type cl:string :kind :scalar :label (:optional) :json-name "externalId")
  (pi:define-map properties
     :key-type cl:string
     :value-type value
     :json-name "properties"
     :value-kind :message
     :index 4)
  (pi:define-map custom-properties
     :key-type cl:string
     :value-type value
     :json-name "customProperties"
     :value-kind :message
     :index 5)
  (create-time-since-epoch
   :index 7 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "createTimeSinceEpoch")
  (last-update-time-since-epoch
   :index 8 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "lastUpdateTimeSinceEpoch")
  (system-metadata
   :index 10 :type cl-protobufs.google.protobuf::any :kind :message :label (:optional) :json-name "systemMetadata"))

(pi:define-message attribution
    ()
  ;; Fields
  (artifact-id
   :index 1 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "artifactId")
  (context-id
   :index 2 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "contextId")
  (system-metadata
   :index 3 :type cl-protobufs.google.protobuf::any :kind :message :label (:optional) :json-name "systemMetadata"))

(pi:define-message association
    ()
  ;; Fields
  (execution-id
   :index 1 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "executionId")
  (context-id
   :index 2 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "contextId")
  (system-metadata
   :index 3 :type cl-protobufs.google.protobuf::any :kind :message :label (:optional) :json-name "systemMetadata"))

(pi:define-message parent-context
    ()
  ;; Fields
  (child-id
   :index 1 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "childId")
  (parent-id
   :index 2 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "parentId")
  (system-metadata
   :index 3 :type cl-protobufs.google.protobuf::any :kind :message :label (:optional) :json-name "systemMetadata"))

(pi:define-message lineage-graph
    ()
  ;; Fields
  (artifact-types
   :index 1 :type artifact-type :kind :message :label (:repeated :list) :json-name "artifactTypes")
  (execution-types
   :index 2 :type execution-type :kind :message :label (:repeated :list) :json-name "executionTypes")
  (context-types
   :index 3 :type context-type :kind :message :label (:repeated :list) :json-name "contextTypes")
  (artifacts
   :index 4 :type artifact :kind :message :label (:repeated :list) :json-name "artifacts")
  (executions
   :index 5 :type execution :kind :message :label (:repeated :list) :json-name "executions")
  (contexts
   :index 6 :type context :kind :message :label (:repeated :list) :json-name "contexts")
  (events
   :index 7 :type event :kind :message :label (:repeated :list) :json-name "events")
  (attributions
   :index 8 :type attribution :kind :message :label (:repeated :list) :json-name "attributions")
  (associations
   :index 9 :type association :kind :message :label (:repeated :list) :json-name "associations")
  (parent-contexts
   :index 10 :type parent-context :kind :message :label (:repeated :list) :json-name "parentContexts"))

(pi:define-message artifact-struct-type
    ()
  ;; Fields
  (pi:define-oneof kind ()
    (simple
     :index 1 :type artifact-type :kind :message :label (:optional) :json-name "simple")
    (union-type
     :index 2 :type union-artifact-struct-type :kind :message :label (:optional) :json-name "unionType")
    (intersection
     :index 3 :type intersection-artifact-struct-type :kind :message :label (:optional) :json-name "intersection")
    (list
     :index 4 :type list-artifact-struct-type :kind :message :label (:optional) :json-name "list")
    (none
     :index 5 :type none-artifact-struct-type :kind :message :label (:optional) :json-name "none")
    (any
     :index 6 :type any-artifact-struct-type :kind :message :label (:optional) :json-name "any")
    (tuple
     :index 7 :type tuple-artifact-struct-type :kind :message :label (:optional) :json-name "tuple")
    (dict
     :index 8 :type dict-artifact-struct-type :kind :message :label (:optional) :json-name "dict")))

(pi:define-message union-artifact-struct-type
    ()
  ;; Fields
  (candidates
   :index 1 :type artifact-struct-type :kind :message :label (:repeated :list) :json-name "candidates"))

(pi:define-message intersection-artifact-struct-type
    ()
  ;; Fields
  (constraints
   :index 1 :type artifact-struct-type :kind :message :label (:repeated :list) :json-name "constraints"))

(pi:define-message list-artifact-struct-type
    ()
  ;; Fields
  (element
   :index 1 :type artifact-struct-type :kind :message :label (:optional) :json-name "element"))

(pi:define-message none-artifact-struct-type
    ())

(pi:define-message any-artifact-struct-type
    ())

(pi:define-message tuple-artifact-struct-type
    ()
  ;; Fields
  (elements
   :index 1 :type artifact-struct-type :kind :message :label (:repeated :list) :json-name "elements"))

(pi:define-message dict-artifact-struct-type
    ()
  ;; Nested messages
  ;; Fields
  (pi:define-map properties
     :key-type cl:string
     :value-type artifact-struct-type
     :json-name "properties"
     :value-kind :message
     :index 1)
  (none-type-not-required
   :index 2 :type cl:boolean :kind :scalar :label (:optional) :json-name "noneTypeNotRequired")
  (extra-properties-type
   :index 3 :type artifact-struct-type :kind :message :label (:optional) :json-name "extraPropertiesType"))

(pi:define-message fake-database-config
    ())

(pi:define-message my-sql-database-config
    (
     :name "MySQLDatabaseConfig")
  ;; Nested messages

  (pi:define-message my-sql-database-config.ssl-options
      (
       :name "SSLOptions")
    ;; Fields
    (key
     :index 1 :type cl:string :kind :scalar :label (:optional) :json-name "key")
    (cert
     :index 2 :type cl:string :kind :scalar :label (:optional) :json-name "cert")
    (ca
     :index 3 :type cl:string :kind :scalar :label (:optional) :json-name "ca")
    (capath
     :index 4 :type cl:string :kind :scalar :label (:optional) :json-name "capath")
    (cipher
     :index 5 :type cl:string :kind :scalar :label (:optional) :json-name "cipher")
    (verify-server-cert
     :index 6 :type cl:boolean :kind :scalar :label (:optional) :json-name "verifyServerCert"))
  ;; Fields
  (host
   :index 1 :type cl:string :kind :scalar :label (:optional) :json-name "host")
  (port
   :index 2 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "port")
  (database
   :index 3 :type cl:string :kind :scalar :label (:optional) :json-name "database")
  (user
   :index 4 :type cl:string :kind :scalar :label (:optional) :json-name "user")
  (password
   :index 5 :type cl:string :kind :scalar :label (:optional) :json-name "password")
  (socket
   :index 6 :type cl:string :kind :scalar :label (:optional) :json-name "socket")
  (ssl-options
   :index 7 :type my-sql-database-config.ssl-options :kind :message :label (:optional) :json-name "sslOptions")
  (skip-db-creation
   :index 8 :type cl:boolean :kind :scalar :label (:optional) :json-name "skipDbCreation"))

(pi:define-message sqlite-metadata-source-config
    ()
  ;; Nested enums

  (pi:define-enum sqlite-metadata-source-config.connection-mode
      ()
    (:unknown :index 0)
    (:readonly :index 1)
    (:readwrite :index 2)
    (:readwrite-opencreate :index 3))
  ;; Fields
  (filename-uri
   :index 1 :type cl:string :kind :scalar :label (:optional) :json-name "filenameUri")
  (connection-mode
   :index 2 :type sqlite-metadata-source-config.connection-mode :kind :enum :label (:optional) :json-name "connectionMode" :default :unknown))

(pi:define-message postgre-sql-database-config
    (
     :name "PostgreSQLDatabaseConfig")
  ;; Nested messages

  (pi:define-message postgre-sql-database-config.ssl-options
      (
       :name "SSLOptions")
    ;; Fields
    (sslmode
     :index 1 :type cl:string :kind :scalar :label (:optional) :json-name "sslmode")
    (sslcert
     :index 2 :type cl:string :kind :scalar :label (:optional) :json-name "sslcert")
    (sslkey
     :index 3 :type cl:string :kind :scalar :label (:optional) :json-name "sslkey")
    (sslpassword
     :index 4 :type cl:string :kind :scalar :label (:optional) :json-name "sslpassword")
    (sslrootcert
     :index 5 :type cl:string :kind :scalar :label (:optional) :json-name "sslrootcert"))
  ;; Fields
  (host
   :index 1 :type cl:string :kind :scalar :label (:optional) :json-name "host")
  (hostaddr
   :index 2 :type cl:string :kind :scalar :label (:optional) :json-name "hostaddr")
  (port
   :index 3 :type cl:string :kind :scalar :label (:optional) :json-name "port")
  (user
   :index 4 :type cl:string :kind :scalar :label (:optional) :json-name "user")
  (password
   :index 5 :type cl:string :kind :scalar :label (:optional) :json-name "password")
  (passfile
   :index 6 :type cl:string :kind :scalar :label (:optional) :json-name "passfile")
  (dbname
   :index 7 :type cl:string :kind :scalar :label (:optional) :json-name "dbname")
  (skip-db-creation
   :index 8 :type cl:boolean :kind :scalar :label (:optional) :json-name "skipDbCreation")
  (ssloption
   :index 9 :type postgre-sql-database-config.ssl-options :kind :message :label (:optional) :json-name "ssloption"))

(pi:define-message migration-options
    ()
  ;; Fields
  (enable-upgrade-migration
   :index 3 :type cl:boolean :kind :scalar :label (:optional) :json-name "enableUpgradeMigration")
  (downgrade-to-schema-version
   :index 2 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "downgradeToSchemaVersion" :default -1))

(pi:define-message retry-options
    ()
  ;; Fields
  (max-num-retries
   :index 1 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "maxNumRetries"))

(pi:define-message connection-config
    ()
  ;; Fields
  (pi:define-oneof config ()
    (fake-database
     :index 1 :type fake-database-config :kind :message :label (:optional) :json-name "fakeDatabase")
    (mysql
     :index 2 :type my-sql-database-config :kind :message :label (:optional) :json-name "mysql")
    (sqlite
     :index 3 :type sqlite-metadata-source-config :kind :message :label (:optional) :json-name "sqlite")
    (postgresql
     :index 5 :type postgre-sql-database-config :kind :message :label (:optional) :json-name "postgresql"))
  (retry-options
   :index 4 :type retry-options :kind :message :label (:optional) :json-name "retryOptions"))

(pi:define-message grpc-channel-arguments
    ()
  ;; Fields
  (max-receive-message-length
   :index 1 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "maxReceiveMessageLength")
  (http2-max-ping-strikes
   :index 2 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "http2MaxPingStrikes"))

(pi:define-message metadata-store-client-config
    ()
  ;; Nested messages

  (pi:define-message metadata-store-client-config.ssl-config
      (
       :name "SSLConfig")
    ;; Fields
    (client-key
     :index 1 :type cl:string :kind :scalar :label (:optional) :json-name "clientKey")
    (server-cert
     :index 2 :type cl:string :kind :scalar :label (:optional) :json-name "serverCert")
    (custom-ca
     :index 3 :type cl:string :kind :scalar :label (:optional) :json-name "customCa"))
  ;; Fields
  (host
   :index 1 :type cl:string :kind :scalar :label (:optional) :json-name "host")
  (port
   :index 2 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "port")
  (ssl-config
   :index 3 :type metadata-store-client-config.ssl-config :kind :message :label (:optional) :json-name "sslConfig")
  (channel-arguments
   :index 4 :type grpc-channel-arguments :kind :message :label (:optional) :json-name "channelArguments")
  (client-timeout-sec
   :index 5 :type cl:double-float :kind :scalar :label (:optional) :json-name "clientTimeoutSec"))

(pi:define-message metadata-store-server-config
    ()
  ;; Nested messages

  (pi:define-message metadata-store-server-config.ssl-config
      (
       :name "SSLConfig")
    ;; Fields
    (server-key
     :index 1 :type cl:string :kind :scalar :label (:optional) :json-name "serverKey")
    (server-cert
     :index 2 :type cl:string :kind :scalar :label (:optional) :json-name "serverCert")
    (custom-ca
     :index 3 :type cl:string :kind :scalar :label (:optional) :json-name "customCa")
    (client-verify
     :index 4 :type cl:boolean :kind :scalar :label (:optional) :json-name "clientVerify"))
  ;; Fields
  (connection-config
   :index 1 :type connection-config :kind :message :label (:optional) :json-name "connectionConfig")
  (migration-options
   :index 3 :type migration-options :kind :message :label (:optional) :json-name "migrationOptions")
  (ssl-config
   :index 2 :type metadata-store-server-config.ssl-config :kind :message :label (:optional) :json-name "sslConfig"))

(pi:define-message list-operation-options
    ()
  ;; Nested messages

  (pi:define-message list-operation-options.order-by-field
      ()
    ;; Nested enums

    (pi:define-enum list-operation-options.order-by-field.field
        ()
      (:field-unspecified :index 0)
      (:create-time :index 1)
      (:last-update-time :index 2)
      (:id :index 3))
    ;; Fields
    (field
     :index 1 :type list-operation-options.order-by-field.field :kind :enum :label (:optional) :json-name "field" :default :id)
    (is-asc
     :index 2 :type cl:boolean :kind :scalar :label (:optional) :json-name "isAsc" :default cl:t))
  ;; Fields
  (max-result-size
   :index 1 :type cl-protobufs:int32 :kind :scalar :label (:optional) :json-name "maxResultSize" :default 20)
  (order-by-field
   :index 2 :type list-operation-options.order-by-field :kind :message :label (:optional) :json-name "orderByField")
  (next-page-token
   :index 3 :type cl:string :kind :scalar :label (:optional) :json-name "nextPageToken")
  (filter-query
   :index 4 :type cl:string :kind :scalar :label (:optional) :json-name "filterQuery"))

(pi:define-message list-operation-next-page-token
    ()
  ;; Fields
  (id-offset
   :index 1 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "idOffset")
  (field-offset
   :index 2 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "fieldOffset")
  (set-options
   :index 3 :type list-operation-options :kind :message :label (:optional) :json-name "setOptions")
  (listed-ids
   :index 4 :type cl-protobufs:int64 :kind :scalar :label (:repeated :list) :json-name "listedIds"))

(pi:define-message transaction-options
    ()
  ;; Fields
  (tag
   :index 1 :type cl:string :kind :scalar :label (:optional) :json-name "tag")
  ;; Extension ranges
  (pi:define-extension 1000 536870911))

(pi:define-message lineage-graph-query-options
    ()
  ;; Nested messages

  (pi:define-message lineage-graph-query-options.boundary-constraint
      ()
    ;; Fields
    (max-num-hops
     :index 1 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "maxNumHops")
    (boundary-artifacts
     :index 2 :type cl:string :kind :scalar :label (:optional) :json-name "boundaryArtifacts")
    (boundary-executions
     :index 3 :type cl:string :kind :scalar :label (:optional) :json-name "boundaryExecutions"))
  ;; Fields
  (pi:define-oneof query-nodes ()
    (artifacts-options
     :index 1 :type list-operation-options :kind :message :label (:optional) :json-name "artifactsOptions"))
  (stop-conditions
   :index 2 :type lineage-graph-query-options.boundary-constraint :kind :message :label (:optional) :json-name "stopConditions")
  (max-node-size
   :index 3 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "maxNodeSize" :default 20))

(pi:define-message lineage-subgraph-query-options
    ()
  ;; Nested enums

  (pi:define-enum lineage-subgraph-query-options.direction
      ()
    (:direction-unspecified :index 0)
    (:upstream :index 1)
    (:downstream :index 2)
    (:bidirectional :index 3))
  ;; Nested messages

  (pi:define-message lineage-subgraph-query-options.starting-nodes
      ()
    ;; Fields
    (filter-query
     :index 1 :type cl:string :kind :scalar :label (:optional) :json-name "filterQuery"))

  (pi:define-message lineage-subgraph-query-options.ending-nodes
      ()
    ;; Fields
    (filter-query
     :index 1 :type cl:string :kind :scalar :label (:optional) :json-name "filterQuery")
    (include-ending-nodes
     :index 2 :type cl:boolean :kind :scalar :label (:optional) :json-name "includeEndingNodes" :default cl:nil))
  ;; Fields
  (pi:define-oneof starting-nodes ()
    (starting-artifacts
     :index 1 :type lineage-subgraph-query-options.starting-nodes :kind :message :label (:optional) :json-name "startingArtifacts")
    (starting-executions
     :index 2 :type lineage-subgraph-query-options.starting-nodes :kind :message :label (:optional) :json-name "startingExecutions"))
  (max-num-hops
   :index 3 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "maxNumHops")
  (direction
   :index 4 :type lineage-subgraph-query-options.direction :kind :enum :label (:optional) :json-name "direction" :default :direction-unspecified)
  (ending-artifacts
   :index 5 :type lineage-subgraph-query-options.ending-nodes :kind :message :label (:optional) :json-name "endingArtifacts")
  (ending-executions
   :index 6 :type lineage-subgraph-query-options.ending-nodes :kind :message :label (:optional) :json-name "endingExecutions"))

;;; Top-Level extensions
(pi:define-extend cl-protobufs.google.protobuf::enum-value-options ()
  (system-type-extension
   :index 384560917 :type system-type-extension :kind :message :label (:optional) :json-name "systemTypeExtension"))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
(pi:add-file-descriptor #P"metadata_store.proto" 'metadata_store)
)

(cl:export '(any
             any-artifact-struct-type
             artifact
             artifact-id
             artifact-struct-type
             artifact-type
             artifact-type.properties-entry
             artifact-type.system-defined-base-type
             artifact-type.system-defined-base-type-int-to-keyword
             artifact-type.system-defined-base-type-keyword-to-int
             artifact-types
             artifact.custom-properties-entry
             artifact.properties-entry
             artifact.state
             artifact.state-int-to-keyword
             artifact.state-keyword-to-int
             artifacts
             artifacts-options
             association
             associations
             attribution
             attributions
             base-type
             bool-value
             boundary-artifacts
             boundary-executions
             ca
             candidates
             capath
             cert
             channel-arguments
             child-id
             cipher
             client-key
             client-timeout-sec
             client-verify
             connection-config
             connection-mode
             constraints
             context
             context-id
             context-type
             context-type.properties-entry
             context-type.system-defined-base-type
             context-type.system-defined-base-type-int-to-keyword
             context-type.system-defined-base-type-keyword-to-int
             context-types
             context.custom-properties-entry
             context.properties-entry
             contexts
             create-time-since-epoch
             custom-ca
             custom-properties
             database
             dbname
             description
             dict
             dict-artifact-struct-type
             dict-artifact-struct-type.properties-entry
             direction
             double-value
             downgrade-to-schema-version
             element
             elements
             enable-upgrade-migration
             ending-artifacts
             ending-executions
             event
             event.path
             event.path.step
             event.type
             event.type-int-to-keyword
             event.type-keyword-to-int
             events
             execution
             execution-id
             execution-type
             execution-type.properties-entry
             execution-type.system-defined-base-type
             execution-type.system-defined-base-type-int-to-keyword
             execution-type.system-defined-base-type-keyword-to-int
             execution-types
             execution.custom-properties-entry
             execution.properties-entry
             execution.state
             execution.state-int-to-keyword
             execution.state-keyword-to-int
             executions
             external-id
             extra-properties-type
             fake-database
             fake-database-config
             field
             field-offset
             filename-uri
             filter-query
             grpc-channel-arguments
             host
             hostaddr
             http2-max-ping-strikes
             id
             id-offset
             include-ending-nodes
             index
             input-type
             int-value
             intersection
             intersection-artifact-struct-type
             is-asc
             key
             last-known-state
             last-update-time-since-epoch
             lineage-graph
             lineage-graph-query-options
             lineage-graph-query-options.boundary-constraint
             lineage-subgraph-query-options
             lineage-subgraph-query-options.direction
             lineage-subgraph-query-options.direction-int-to-keyword
             lineage-subgraph-query-options.direction-keyword-to-int
             lineage-subgraph-query-options.ending-nodes
             lineage-subgraph-query-options.starting-nodes
             list
             list-artifact-struct-type
             list-operation-next-page-token
             list-operation-options
             list-operation-options.order-by-field
             list-operation-options.order-by-field.field
             list-operation-options.order-by-field.field-int-to-keyword
             list-operation-options.order-by-field.field-keyword-to-int
             listed-ids
             max-node-size
             max-num-hops
             max-num-retries
             max-receive-message-length
             max-result-size
             metadata-store-client-config
             metadata-store-client-config.ssl-config
             metadata-store-server-config
             metadata-store-server-config.ssl-config
             metadata_store
             migration-options
             milliseconds-since-epoch
             my-sql-database-config
             my-sql-database-config.ssl-options
             mysql
             name
             next-page-token
             none
             none-artifact-struct-type
             none-type-not-required
             order-by-field
             output-type
             parent-context
             parent-contexts
             parent-id
             passfile
             password
             path
             port
             postgre-sql-database-config
             postgre-sql-database-config.ssl-options
             postgresql
             properties
             property-type
             property-type-int-to-keyword
             property-type-keyword-to-int
             proto-value
             retry-options
             server-cert
             server-key
             set-options
             simple
             skip-db-creation
             socket
             sqlite
             sqlite-metadata-source-config
             sqlite-metadata-source-config.connection-mode
             sqlite-metadata-source-config.connection-mode-int-to-keyword
             sqlite-metadata-source-config.connection-mode-keyword-to-int
             ssl-config
             ssl-options
             sslcert
             sslkey
             sslmode
             ssloption
             sslpassword
             sslrootcert
             starting-artifacts
             starting-executions
             state
             steps
             stop-conditions
             string-value
             struct-value
             system-metadata
             system-type-extension
             tag
             transaction-options
             tuple
             tuple-artifact-struct-type
             type
             type-id
             type-name
             union-artifact-struct-type
             union-type
             uri
             user
             value
             verify-server-cert
             version))
