;;; metadata_source.proto.lisp
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
(pi:define-schema 'metadata_source
    :syntax :proto3

     :package "ml_metadata.proto"
     :import '("google/protobuf/any.proto"))
)


;;; Top-Level enums

(pi:define-enum metadata-source-type
    ()
  (:unknown-metadata-source :index 0)
  (:fake-metadata-source :index 1)
  (:mysql-metadata-source :index 2)
  (:sqlite-metadata-source :index 3)
  (:postgresql-metadata-source :index 6))

;;; Top-Level messages

(pi:define-message record-set
    ()
  ;; Nested messages

  (pi:define-message record-set.record
      ()
    ;; Fields
    (values
     :index 1 :type cl:string :kind :scalar :label (:repeated :list) :json-name "values"))
  ;; Fields
  (column-names
   :index 1 :type cl:string :kind :scalar :label (:repeated :list) :json-name "columnNames")
  (records
   :index 2 :type record-set.record :kind :message :label (:repeated :list) :json-name "records"))

(pi:define-message metadata-source-query-config
    ()
  ;; Nested messages

  (pi:define-message metadata-source-query-config.template-query
      ()
    ;; Fields
    (query
     :index 1 :type cl:string :kind :scalar :label (:optional) :json-name "query")
    (parameter-num
     :index 2 :type cl-protobufs:int32 :kind :scalar :label (:optional) :json-name "parameterNum"))

  (pi:define-message metadata-source-query-config.db-verification
      ()
    ;; Fields
    (total-num-tables
     :index 1 :type cl-protobufs:int32 :kind :scalar :label (:optional) :json-name "totalNumTables")
    (total-num-indexes
     :index 2 :type cl-protobufs:int32 :kind :scalar :label (:optional) :json-name "totalNumIndexes"))

  (pi:define-message metadata-source-query-config.migration-scheme
      ()
    ;; Nested messages

    (pi:define-message metadata-source-query-config.migration-scheme.verification-scheme
        ()
      ;; Fields
      (previous-version-setup-queries
       :index 1 :type metadata-source-query-config.template-query :kind :message :label (:repeated :list) :json-name "previousVersionSetupQueries")
      (post-migration-verification-queries
       :index 2 :type metadata-source-query-config.template-query :kind :message :label (:repeated :list) :json-name "postMigrationVerificationQueries"))
    ;; Fields
    (upgrade-queries
     :index 1 :type metadata-source-query-config.template-query :kind :message :label (:repeated :list) :json-name "upgradeQueries")
    (downgrade-queries
     :index 3 :type metadata-source-query-config.template-query :kind :message :label (:repeated :list) :json-name "downgradeQueries")
    (upgrade-verification
     :index 2 :type metadata-source-query-config.migration-scheme.verification-scheme :kind :message :label (:optional) :json-name "upgradeVerification")
    (downgrade-verification
     :index 4 :type metadata-source-query-config.migration-scheme.verification-scheme :kind :message :label (:optional) :json-name "downgradeVerification")
    (db-verification
     :index 5 :type metadata-source-query-config.db-verification :kind :message :label (:optional) :json-name "dbVerification"))
  ;; Fields
  (metadata-source-type
   :index 1 :type metadata-source-type :kind :enum :label (:optional) :json-name "metadataSourceType" :default :unknown-metadata-source)
  (drop-type-table
   :index 3 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "dropTypeTable")
  (create-type-table
   :index 4 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "createTypeTable")
  (check-type-table
   :index 44 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "checkTypeTable")
  (insert-artifact-type
   :index 54 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "insertArtifactType")
  (insert-execution-type
   :index 55 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "insertExecutionType")
  (insert-context-type
   :index 58 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "insertContextType")
  (select-types-by-id
   :index 128 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectTypesById")
  (select-types-by-external-ids
   :index 134 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectTypesByExternalIds")
  (select-type-by-id
   :index 6 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectTypeById")
  (select-type-by-name
   :index 20 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectTypeByName")
  (select-type-by-name-and-version
   :index 111 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectTypeByNameAndVersion")
  (select-types-by-names
   :index 136 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectTypesByNames")
  (select-types-by-names-and-versions
   :index 137 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectTypesByNamesAndVersions")
  (select-all-types
   :index 57 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectAllTypes")
  (update-type
   :index 138 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "updateType")
  (drop-parent-type-table
   :index 99 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "dropParentTypeTable")
  (create-parent-type-table
   :index 100 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "createParentTypeTable")
  (check-parent-type-table
   :index 101 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "checkParentTypeTable")
  (insert-parent-type
   :index 109 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "insertParentType")
  (select-parent-type-by-type-id
   :index 110 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectParentTypeByTypeId")
  (drop-type-property-table
   :index 7 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "dropTypePropertyTable")
  (create-type-property-table
   :index 8 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "createTypePropertyTable")
  (check-type-property-table
   :index 45 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "checkTypePropertyTable")
  (insert-type-property
   :index 9 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "insertTypeProperty")
  (select-property-by-type-id
   :index 10 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectPropertyByTypeId")
  (select-last-insert-id
   :index 11 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectLastInsertId")
  (drop-artifact-table
   :index 12 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "dropArtifactTable")
  (create-artifact-table
   :index 13 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "createArtifactTable")
  (check-artifact-table
   :index 46 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "checkArtifactTable")
  (insert-artifact
   :index 14 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "insertArtifact")
  (select-artifact-by-id
   :index 15 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectArtifactById")
  (select-artifact-by-type-id-and-name
   :index 94 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectArtifactByTypeIdAndName")
  (select-artifacts-by-type-id
   :index 52 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectArtifactsByTypeId")
  (select-artifacts-by-uri
   :index 56 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectArtifactsByUri")
  (select-artifacts-by-external-ids
   :index 130 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectArtifactsByExternalIds")
  (update-artifact
   :index 21 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "updateArtifact")
  (drop-artifact-property-table
   :index 16 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "dropArtifactPropertyTable")
  (create-artifact-property-table
   :index 17 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "createArtifactPropertyTable")
  (check-artifact-property-table
   :index 47 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "checkArtifactPropertyTable")
  (insert-artifact-property
   :index 18 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "insertArtifactProperty")
  (select-artifact-property-by-artifact-id
   :index 19 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectArtifactPropertyByArtifactId")
  (update-artifact-property
   :index 22 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "updateArtifactProperty")
  (delete-artifact-property
   :index 23 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "deleteArtifactProperty")
  (drop-execution-table
   :index 24 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "dropExecutionTable")
  (create-execution-table
   :index 25 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "createExecutionTable")
  (check-execution-table
   :index 48 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "checkExecutionTable")
  (insert-execution
   :index 28 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "insertExecution")
  (select-execution-by-id
   :index 29 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectExecutionById")
  (select-execution-by-type-id-and-name
   :index 95 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectExecutionByTypeIdAndName")
  (select-executions-by-type-id
   :index 53 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectExecutionsByTypeId")
  (select-executions-by-external-ids
   :index 132 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectExecutionsByExternalIds")
  (update-execution
   :index 34 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "updateExecution")
  (drop-execution-property-table
   :index 26 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "dropExecutionPropertyTable")
  (create-execution-property-table
   :index 27 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "createExecutionPropertyTable")
  (check-execution-property-table
   :index 49 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "checkExecutionPropertyTable")
  (insert-execution-property
   :index 30 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "insertExecutionProperty")
  (select-execution-property-by-execution-id
   :index 31 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectExecutionPropertyByExecutionId")
  (update-execution-property
   :index 32 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "updateExecutionProperty")
  (delete-execution-property
   :index 33 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "deleteExecutionProperty")
  (drop-context-table
   :index 67 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "dropContextTable")
  (create-context-table
   :index 68 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "createContextTable")
  (check-context-table
   :index 69 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "checkContextTable")
  (insert-context
   :index 70 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "insertContext")
  (select-context-by-id
   :index 71 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectContextById")
  (select-contexts-by-type-id
   :index 72 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectContextsByTypeId")
  (select-context-by-type-id-and-name
   :index 93 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectContextByTypeIdAndName")
  (select-contexts-by-external-ids
   :index 133 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectContextsByExternalIds")
  (update-context
   :index 73 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "updateContext")
  (drop-context-property-table
   :index 74 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "dropContextPropertyTable")
  (create-context-property-table
   :index 75 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "createContextPropertyTable")
  (check-context-property-table
   :index 76 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "checkContextPropertyTable")
  (insert-context-property
   :index 77 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "insertContextProperty")
  (select-context-property-by-context-id
   :index 78 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectContextPropertyByContextId")
  (update-context-property
   :index 79 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "updateContextProperty")
  (delete-context-property
   :index 80 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "deleteContextProperty")
  (drop-parent-context-table
   :index 102 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "dropParentContextTable")
  (create-parent-context-table
   :index 103 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "createParentContextTable")
  (check-parent-context-table
   :index 104 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "checkParentContextTable")
  (insert-parent-context
   :index 106 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "insertParentContext")
  (select-parent-context-by-context-id
   :index 107 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectParentContextByContextId")
  (select-parent-context-by-parent-context-id
   :index 108 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectParentContextByParentContextId")
  (select-parent-contexts-by-context-ids
   :index 139 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectParentContextsByContextIds")
  (select-parent-contexts-by-parent-context-ids
   :index 140 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectParentContextsByParentContextIds")
  (drop-event-table
   :index 35 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "dropEventTable")
  (create-event-table
   :index 36 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "createEventTable")
  (check-event-table
   :index 50 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "checkEventTable")
  (insert-event
   :index 37 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "insertEvent")
  (select-event-by-artifact-ids
   :index 96 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectEventByArtifactIds")
  (select-event-by-execution-ids
   :index 97 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectEventByExecutionIds")
  (drop-event-path-table
   :index 40 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "dropEventPathTable")
  (create-event-path-table
   :index 41 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "createEventPathTable")
  (check-event-path-table
   :index 51 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "checkEventPathTable")
  (insert-event-path
   :index 42 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "insertEventPath")
  (select-event-path-by-event-ids
   :index 98 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectEventPathByEventIds")
  (drop-association-table
   :index 81 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "dropAssociationTable")
  (create-association-table
   :index 82 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "createAssociationTable")
  (check-association-table
   :index 83 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "checkAssociationTable")
  (insert-association
   :index 84 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "insertAssociation")
  (select-association-by-context-id
   :index 85 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectAssociationByContextId")
  (select-associations-by-execution-ids
   :index 142 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectAssociationsByExecutionIds")
  (drop-attribution-table
   :index 87 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "dropAttributionTable")
  (create-attribution-table
   :index 88 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "createAttributionTable")
  (check-attribution-table
   :index 89 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "checkAttributionTable")
  (insert-attribution
   :index 90 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "insertAttribution")
  (select-attribution-by-context-id
   :index 91 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectAttributionByContextId")
  (select-attributions-by-artifact-ids
   :index 143 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectAttributionsByArtifactIds")
  (drop-mlmd-env-table
   :index 60 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "dropMlmdEnvTable")
  (create-mlmd-env-table
   :index 61 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "createMlmdEnvTable")
  (schema-version
   :index 59 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "schemaVersion")
  (check-mlmd-env-table
   :index 63 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "checkMlmdEnvTable")
  (check-mlmd-env-table-existence
   :index 141 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "checkMlmdEnvTableExistence")
  (insert-schema-version
   :index 66 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "insertSchemaVersion")
  (update-schema-version
   :index 64 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "updateSchemaVersion")
  (check-tables-in-v0-13-2
   :index 65 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "checkTablesInV0132")
  (secondary-indices
   :index 105 :type metadata-source-query-config.template-query :kind :message :label (:repeated :list) :json-name "secondaryIndices")
  (pi:define-map migration-schemes
     :key-type cl-protobufs:int64
     :value-type metadata-source-query-config.migration-scheme
     :json-name "migrationSchemes"
     :value-kind :message
     :index 62)
  (delete-contexts-by-id
   :index 112 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "deleteContextsById")
  (delete-contexts-properties-by-contexts-id
   :index 113 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "deleteContextsPropertiesByContextsId")
  (delete-parent-contexts-by-parent-ids
   :index 114 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "deleteParentContextsByParentIds")
  (delete-parent-contexts-by-child-ids
   :index 115 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "deleteParentContextsByChildIds")
  (delete-parent-contexts-by-parent-id-and-child-ids
   :index 131 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "deleteParentContextsByParentIdAndChildIds")
  (delete-artifacts-by-id
   :index 116 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "deleteArtifactsById")
  (delete-artifacts-properties-by-artifacts-id
   :index 117 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "deleteArtifactsPropertiesByArtifactsId")
  (delete-executions-by-id
   :index 118 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "deleteExecutionsById")
  (delete-executions-properties-by-executions-id
   :index 119 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "deleteExecutionsPropertiesByExecutionsId")
  (delete-events-by-artifacts-id
   :index 120 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "deleteEventsByArtifactsId")
  (delete-events-by-executions-id
   :index 121 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "deleteEventsByExecutionsId")
  (delete-associations-by-contexts-id
   :index 122 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "deleteAssociationsByContextsId")
  (delete-associations-by-executions-id
   :index 123 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "deleteAssociationsByExecutionsId")
  (delete-attributions-by-contexts-id
   :index 124 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "deleteAttributionsByContextsId")
  (delete-attributions-by-artifacts-id
   :index 125 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "deleteAttributionsByArtifactsId")
  (delete-event-paths
   :index 126 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "deleteEventPaths")
  (delete-parent-type
   :index 127 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "deleteParentType")
  (select-properties-by-type-id
   :index 129 :type metadata-source-query-config.template-query :kind :message :label (:optional) :json-name "selectPropertiesByTypeId")
  (metadata-source-type-specific-options
   :index 135 :type cl-protobufs.google.protobuf::any :kind :message :label (:optional) :json-name "metadataSourceTypeSpecificOptions"))

(pi:define-message my-sql-source-error-info
    (
     :name "MySQLSourceErrorInfo")
  ;; Fields
  (mysql-error-code
   :index 1 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "mysqlErrorCode"))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
(pi:add-file-descriptor #P"metadata_source.proto" 'metadata_source)
)

(cl:export '(check-artifact-property-table
             check-artifact-table
             check-association-table
             check-attribution-table
             check-context-property-table
             check-context-table
             check-event-path-table
             check-event-table
             check-execution-property-table
             check-execution-table
             check-mlmd-env-table
             check-mlmd-env-table-existence
             check-parent-context-table
             check-parent-type-table
             check-tables-in-v0-13-2
             check-type-property-table
             check-type-table
             column-names
             create-artifact-property-table
             create-artifact-table
             create-association-table
             create-attribution-table
             create-context-property-table
             create-context-table
             create-event-path-table
             create-event-table
             create-execution-property-table
             create-execution-table
             create-mlmd-env-table
             create-parent-context-table
             create-parent-type-table
             create-type-property-table
             create-type-table
             db-verification
             delete-artifact-property
             delete-artifacts-by-id
             delete-artifacts-properties-by-artifacts-id
             delete-associations-by-contexts-id
             delete-associations-by-executions-id
             delete-attributions-by-artifacts-id
             delete-attributions-by-contexts-id
             delete-context-property
             delete-contexts-by-id
             delete-contexts-properties-by-contexts-id
             delete-event-paths
             delete-events-by-artifacts-id
             delete-events-by-executions-id
             delete-execution-property
             delete-executions-by-id
             delete-executions-properties-by-executions-id
             delete-parent-contexts-by-child-ids
             delete-parent-contexts-by-parent-id-and-child-ids
             delete-parent-contexts-by-parent-ids
             delete-parent-type
             downgrade-queries
             downgrade-verification
             drop-artifact-property-table
             drop-artifact-table
             drop-association-table
             drop-attribution-table
             drop-context-property-table
             drop-context-table
             drop-event-path-table
             drop-event-table
             drop-execution-property-table
             drop-execution-table
             drop-mlmd-env-table
             drop-parent-context-table
             drop-parent-type-table
             drop-type-property-table
             drop-type-table
             insert-artifact
             insert-artifact-property
             insert-artifact-type
             insert-association
             insert-attribution
             insert-context
             insert-context-property
             insert-context-type
             insert-event
             insert-event-path
             insert-execution
             insert-execution-property
             insert-execution-type
             insert-parent-context
             insert-parent-type
             insert-schema-version
             insert-type-property
             key
             metadata-source-query-config
             metadata-source-query-config.db-verification
             metadata-source-query-config.migration-scheme
             metadata-source-query-config.migration-scheme.verification-scheme
             metadata-source-query-config.migration-schemes-entry
             metadata-source-query-config.template-query
             metadata-source-type
             metadata-source-type-int-to-keyword
             metadata-source-type-keyword-to-int
             metadata-source-type-specific-options
             metadata_source
             migration-schemes
             my-sql-source-error-info
             mysql-error-code
             parameter-num
             post-migration-verification-queries
             previous-version-setup-queries
             query
             record-set
             record-set.record
             records
             schema-version
             secondary-indices
             select-all-types
             select-artifact-by-id
             select-artifact-by-type-id-and-name
             select-artifact-property-by-artifact-id
             select-artifacts-by-external-ids
             select-artifacts-by-type-id
             select-artifacts-by-uri
             select-association-by-context-id
             select-associations-by-execution-ids
             select-attribution-by-context-id
             select-attributions-by-artifact-ids
             select-context-by-id
             select-context-by-type-id-and-name
             select-context-property-by-context-id
             select-contexts-by-external-ids
             select-contexts-by-type-id
             select-event-by-artifact-ids
             select-event-by-execution-ids
             select-event-path-by-event-ids
             select-execution-by-id
             select-execution-by-type-id-and-name
             select-execution-property-by-execution-id
             select-executions-by-external-ids
             select-executions-by-type-id
             select-last-insert-id
             select-parent-context-by-context-id
             select-parent-context-by-parent-context-id
             select-parent-contexts-by-context-ids
             select-parent-contexts-by-parent-context-ids
             select-parent-type-by-type-id
             select-properties-by-type-id
             select-property-by-type-id
             select-type-by-id
             select-type-by-name
             select-type-by-name-and-version
             select-types-by-external-ids
             select-types-by-id
             select-types-by-names
             select-types-by-names-and-versions
             total-num-indexes
             total-num-tables
             update-artifact
             update-artifact-property
             update-context
             update-context-property
             update-execution
             update-execution-property
             update-schema-version
             update-type
             upgrade-queries
             upgrade-verification
             value
             values))
