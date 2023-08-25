(in-package #:ai-project-tools/core)

;;;; ======= Base mixin/concept classes. =======
(defclass has-data ()
  ((%data
    :initarg :data
    :initform nil
    :accessor data
    :documentation "Contains arbitrary data")))

(defclass has-metadata ()
  ((%metadata
    :initarg :metadata
    :accessor metadata))
  (:documentation
   "Base class for all objects that have simple metadata referring only to themselves."))

(defclass has-metadata-store ()
  ((%metadata-store
    :initarg :metadata-store
    :accessor metadata-store))
  (:documentation
   "Base class for all objects that have a metadata store. A metadata-store differs
from simple metadata in that simple metadata holds only data about the object
itself. A metadata-store may hold data for/about other objects, and may provide
a scoping mechanism."))

(defclass has-notes ()
  ((%notes :initarg :notes :accessor notes))
  (:documentation "Base class for all objects that have arbitrary notes."))

(defclass has-state ()
  ((%state
    :initarg :state
    :accessor state))
  (:documentation
   "Base class for all objects that have a state. Distinction between having data is
that state is more specific data about the owning object."))

(defclass has-name ()
  ((%name
    :initarg :name
    :accessor name))
  (:documentation "Base class for all objects that have a name."))

(defclass has-description ()
  ((%description
    :initarg :description
    :accessor description))
  (:documentation "Base class for all objects that have a description."))

(defclass has-role ()
  ((%role
    :initarg :role
    :accessor role))
  (:documentation "Base class for all objects that have a role."))

(defclass belongs-to-project ()
  ((%project
    :initarg :project
    :accessor project))
  (:documentation "Base class for all objects that belong to a project."))

(defclass belongs-to-session ()
  ((%session
    :initarg :session
    :accessor session))
  (:documentation
   "Base class for all objects that belong to a session (which in turn, belongs to a
project)."))

(defclass timed ()
  ((%start-time
    :initarg :start-time
    :accessor start-time)
   (%end-time
    :initarg :end-time
    :accessor end-time)
   (%duration
    :initarg :duration
    :accessor duration)
   ;; (%time-units :initarg :time-units :accessor time-units)
   )
  (:documentation
   "A thing that is measured in the time dimension. It has a start time, end time,
and duration."))

(defclass node () ()
  (:documentation
   "Base marker class for our system nodes."))

(defclass simple-leaf-node (node)
  ((%parent
    :initarg :parent
    :initform nil
    :accessor parent
    :documentation "The parent of the node, if any."))
  (:documentation
   "Base marker class for all simple leaf nodes. Nodes that are not composites. They
contain no children."))

(defclass tree-node (simple-leaf-node)
  ((children
    :initarg :children
    :initform ()
    :accessor children
    :type list
    :documentation
    "The list of children of the node,
which may be more nodes, or other values. If the parent slot is nil, this is a root node."))
  (:documentation "Base class for all parent nodes. Nodes that are composites."))


;;;; ======= Core system runtime execution objects. The verbs. =======
(defclass runnable () ()
  (:documentation "Marker base class for executions"))

(defclass stepped-runnable () ()
  (:documentation "Marker class for an execution that can be broken down into steps."))

(defclass timed-runnable (timed runnable) ()
  (:documentation
   "A timed runnable that measures start/end/duration in an unspecified time unit."))

(defclass realtime-timed-runnable (timed-runnable) ()
  (:documentation
   "A timed runnable that uses internal-real-time to measure start/end/duration."))

(defclass timestamp-timed-runnable (timed-runnable) ()
  (:documentation
   "A timed runnable that uses local-time timestamps to measure start/end/duration."))


;;;; See: https://cloud.google.com/vertex-ai/docs/ml-metadata/data-model
;;;; ======= Core system runtime components. The nouns. =======
(defclass artifact (has-name has-description has-data has-metadata)
  ()
  (:documentation
   "An artifact is a discrete entity or piece of data produced and consumed by a
workflow. Examples of artifacts include datasets, models, input
files, and training logs."))

(defclass execution-node (has-data has-metadata has-state has-description node runnable)
  ()
  (:documentation
   " An execution node is a record of an individual machine learning workflow step,
typically annotated with its runtime parameters. Examples of executions include
data ingestion, data validation, model training, model evaluation, and model
deployment. The product of an execution is one or more artifact."))

(defclass execution-tree-node (execution-node tree-node)
  ()
  (:documentation
   "Base marker class for all execution tree-nodes. Nodes that are composites. The
scope of their execution/run encmpasses that of their children as well."))

(defclass execution-event (has-data has-metadata belongs-to-session timed)
  ((%input-keys 
    :initarg :input-keys 
    :initform (error "Must supply one or more input keys.")
    :accessor input-keys
    :type list
    :documentation "Names of expected input artifacts.")
   (%output-keys 
    :initarg :output-keys
    :initform (error "Must supply one or more output keys.")
    :accessor output-keys
    :type list
    :documentation "Names of produced output artifacts.")
   (%inputs 
    :initarg :inputs 
    :initform nil
    :accessor inputs
    :type list
    :documentation
    "Inputs to the execution event made available in the context of the execution.")
   (%outputs 
    :initarg :outputs 
    :initform nil
    :accessor outputs
    :type list
    :documentation
    "Outputs of the execution event produced in the context of the execution.")
   (%execution-node 
    :initarg :execution-node 
    :initform nil
    :accessor execution-node
    :type (or nil execution-node)
    :documentation
    "The execution-node that consumed/produced the artifacts for this execution-event
record."))
  (:documentation
   "An execution-event describes the relationship between artifacts and executions. Each
artifact can be produced by an execution and consumed by other executions.
Events help you to determine the provenance of artifacts in their ML workflows
by chaining together artifacts and executions. An event is a record of an action
that occurred at a particular time and date. An event is also an edge in a graph
of artifacts and executions."))

;;;; ======= Core system runtime containers. Everything happens within these contexts. =======
(defclass configuration-set () ())

(defclass metadata-schema ()
  ()
  (:documentation
   "A MetadataSchema describes the schema for particular types of artifacts,
executions, or contexts. MetadataSchemas are used to validate the key-value
pairs during creation of the corresponding Metadata resources. Schema validation
is only performed on matching fields between the resource and the
MetadataSchema."))

(defclass metadata-store ()
  ((%schema
    :initarg :schema
    :initform (error "Must supply a metadata schema")
    :accessor schema
    :documentation "The metadata schema for this store."))
  (:documentation
   "A metadata store is a database that stores metadata for artifacts, executions,
events, and contexts. A MetadataStore is the top-level container for metadata
resources. MetadataStore is associated with a specific project. Typically,
an organization uses one shared MetadataStore for metadata resources within each
project."))

(defclass simple-memory-metadata-store (metadata-store)
  ((%store
    :initarg :store
    :initform (make-hash-table :test #'equal)
    :accessor store))
  (:documentation
   "A simple in-memory metadata store that stores metadata in a hash table."))

(defclass scoped-metadata-store (has-name metadata-store tree-node)
  ((%store
    :initarg :store
    :initform nil ;; Allow using parent's store, checked in initialize-instance
    :accessor store)
   (%scope-delimeter
    :initarg :scope-delimeter
    :initform "/"
    :accessor scope-delimeter
    :documentation
    "The scope delimeter for this store.")
  (%scope-path
   :initform nil
   :reader scope-path
   :documentation
   "The scope path for this store."))
  (:documentation
   "A scoped metadata store is a metadata store that is provides a scope or
namespace for the metadata resources within it."))

(defclass memory-scoped-metadata-store (scoped-metadata-store simple-memory-metadata-store)
  ()
  (:documentation
   "An in-memory scoped metadata store is a scoped metadata store that stores
metadata in a hash table, using the chain of parent node names as a scoped key
for each value."))

(defclass run-context ()
  ((%execution-events
    :initarg :execution-events
    :initform ()
    :accessor execution-events
    :documentation
    "The execution events (executions, artifacts, and events) that belong to this session."))
  (:documentation
   "A run-context is used to group graphs of artifacts and executions together under
a single,queryable, and typed category. Run-contexts can be used to represent
sets of metadata. An example of a run-context would be a run of an LLM chat
session, Q&A + retrieval, or machine learning pipeline."))

(defclass session (has-name has-description has-state has-notes has-metadata-store timed
                   belongs-to-project run-context)
  ((%metadata-store
    :initarg :metadata-store
    :accessor metadata-store
    :type scoped-metadata-store)))

(defclass project (has-name has-description has-notes has-metadata-store)
  ((%sessions
    :initarg :sessions
    :initform ()
    :accessor sessions
    :documentation "The sessions/run-contexts that belong to this project.")
   (%metadata-store
    :initarg :metadata-store
    :accessor metadata-store
    :type scoped-metadata-store))
   (:documentation
    "Container for all resources and activities used to attain a goal."))

(defclass system-configuration (configuration-set has-name has-description)
  ((%designator
    :initarg :designator
    :initform (error "Must provide a designator.")
    :reader designator)
   (%configuration
    :initarg :configuration
    :initform nil
    :accessor configuration))
  (:documentation
   "A configuration for the LLM Toolkit. This will be used to configure the top
level API entry points for the toolkit like session creation, project creation,
whether the project and sessions are persistent, etc."))

(defclass application (has-name has-description has-metadata-store)
  ((%system-configuration
    :initarg :system-configuration
    :accessor system-configuration)
   (%project
    :initarg :project
    :accessor project)
   (%metadata-store
    :initarg :metadata-store
    :initform (error "Must provide a root metadata store.")
    :accessor metadata-store
    :type scoped-metadata-store))
  (:documentation
   "An application is the top-level container. It represents a targeted purpose,
such as a chat application, Q&A, code generation, etc.This provides the API
surface for clients at the REPL, HTTP, etc. The application is responsible for
building all the top-level components using the configuration, and the metadata
store."))

;;;; ======= Core conditions. =======
(define-condition immediate-task-stop ()
  ((%result
    :initarg :result
    :reader result))
  (:documentation
   "A condition that is signaled when a task is to be stopped immediately, returning the
result."))
