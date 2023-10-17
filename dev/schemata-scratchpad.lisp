(in-package :schemata)

;; Marker class for objects that know how to validate themselves.
(defclass validating-object-mixin ()
  ((%schema :initarg :schema
            :initform (error "No schema specified")
            :reader object-schema)))

(defmethod initialize-instance :after ((instance validating-object-mixin) &rest args)
  (declare (ignore args)
  ;; (setf (%schema instance) (schemata:find-schema (%schema instance)))
  (schemata:validate-with-schema (object-schema instance) instance)))

(defclass validating-object-schema (schemata:object-schema)
  ((instance-validator :initarg :instance-validator
              :accessor instance-validator
              :initform nil
              :type (or null trivial-types:function-designator)
              :documentation "A function that validates the whole object, after the attribute/slots have been
validated."))
  (:documentation "An extension of schemata:object-schema that adds validation for the whole
object, in addition to the attributes/slots."))

(defmethod parse-schema-type ((schema-type (eql 'validating-object)) schema-spec)
  (destructuring-bind (name attributes &optional options) (rest schema-spec)
    (apply #'make-instance 'validating-object-schema
           :name name
           :attributes (mapcar #'parse-attribute attributes)
           options)))

(defclass accessor-test (validating-object-mixin)
  ((%slot-w-reader :initarg :slot-w-reader :initform nil :reader slot-w-reader)
   (%slot-w-writer :initarg :slot-w-writer :initform nil :writer slot-w-writer)
   (%slot-w-accessor :initarg :slot-w-accessor :initform nil :accessor slot-w-accessor)
   (%slot-bare :initarg :slot-bare :initform nil)))

(defclass subclass-accessor-test (accessor-test)
  ((%slot-subw-reader :initarg :slot-subw-reader :reader slot-subw-reader :type integer)
   (%slot-subw-writer :initarg :slot-subw-writer :writer slot-subw-writer :type string)
   (%slot-subw-accessor :initarg :slot-subw-accessor :accessor slot-subw-accessor :type float)
   (%slot-subbare :initarg :slot-subbare :type integer)))

(defun %acceptable-str-slot (x)
  (member x '("MOO" "BAHAHA" "WOOF") :test #'equalp))

(defun %acceptable-accessor-test-instance (instance)
  (log:info "Calling instance validator with instance: ~a" instance)
  (> 20 (+ (slot-w-reader instance) (slot-value instance '%slot-bare))))
;;(%acceptable-accessor-test-instance *slot-object*)
;;(+ (slot-w-reader *slot-object*) (slot-value *slot-object* '%slot-bare))

(defschema accessor-test-schema
    (validating-object
     "acessor-test"
     ((%slot-w-reader integer :reader slot-w-reader :required t)
      (%slot-w-writer (satisfies %acceptable-str-slot) :writer slot-w-writer :required nil)
      (%slot-w-accessor float :accessor slot-w-accessor :required t)
      (%slot-bare integer :required t))
     (:instance-validator %acceptable-accessor-test-instance)))

#+(or)(defparameter *slot-object*
        (make-instance 'accessor-test :slot-w-reader 15
                                      :slot-w-writer "MOO" :slot-w-accessor 3.5
                                      :slot-bare 4))
#+(or)(defparameter *subclass-slot-object*
        (make-instance 'subclass-accessor-test :slot-w-reader 1 :slot-w-writer "MOO" :slot-w-accessor 3.5 :slot-bare 4
                       :slot-subw-reader 1 :slot-subw-writer "MOO" :slot-subw-accessor 3.5 :slot-subbare 4))

;;;; ================================================================================
;;;; CLOS utilities
;;;; ================================================================================
;; https://stackoverflow.com/questions/38452350/is-there-a-way-to-gather-slot-definition-readers-from-all-the-inheritance-tree
(defun all-direct-slots (class)
  (append (c2mop:class-direct-slots class)
          (alexandria:mappend #'all-direct-slots
                   (c2mop:class-direct-superclasses class))))
;;(all-direct-slots (find-class 'subclass-accessor-test))

(defun all-slot-readers (class)
  (alexandria:mappend #'c2mop:slot-definition-readers
           (all-direct-slots class)))

(defun all-slot-writers (class)
  (alexandria:mappend #'c2mop:slot-definition-writers
           (all-direct-slots class)))
;; From cl-mop
;; https://github.com/inaimathi/cl-mop (MIT License)
;;;;;;;;;;;;;;; basic operations
(defmethod slot-names ((object error))
  (slot-names (class-of object)))

(defmethod slot-names ((object standard-object))
  (slot-names (class-of object)))

(defmethod slot-names ((class standard-class))
  (mapcar #'slot-definition-name (class-slots class)))

(defgeneric map-slots (function instance)
  (:documentation "Takes a binary function and an instance.
Returns the sequence resulting from calling the function on each bound (slot-name slot-value) of instance"))

(defmethod map-slots ((fn function) (instance error))
  (loop for slot in (class-slots (class-of instance))
     for slot-name = (slot-definition-name slot)
     when (slot-boundp instance slot-name)
     collect (funcall fn slot-name (slot-value instance slot-name))))

(defmethod map-slots ((fn function) (instance standard-object))
  "The default case of map-slots specializes on STANDARD-OBJECT."
  (loop for slot in (class-slots (class-of instance))
	for slot-name = (slot-definition-name slot)
	when (slot-boundp instance slot-name)
	  collect (funcall fn slot-name (slot-value instance slot-name))))

(defmethod to-alist ((instance error))
  (map-slots (lambda (k v) (cons k v)) instance))

(defmethod to-alist ((instance standard-object))
  "Returns an assoc list of (k . v) pairs from the given instances' slots and slot-values.
This is meant to provide an easy way of showing "
  (map-slots (lambda (k v) (cons k v)) instance))

;; Added by me
(defmethod to-plist ((instance error))
  (flatten (map-slots (lambda (k v) (list (alexandria:make-keyword k) v)) instance)))

(defmethod to-plist ((instance standard-object))
  "Returns a plist of ((keyword k) v ...) pairs from the given instances' slots and slot-values.
This is meant to provide an easy way of showing "
  (flatten (map-slots (lambda (k v) (list (alexandria:make-keyword k) v)) instance)))


(defun %slot-reader-fn (class slot-name)
  "Get the slot-definition for SLOT-NAME in CLASS, and return a function that reads
the slot. First try to use the accessor function, then the reader, otherwise
return a lambda that uses slot-value."
  (let ((dslots (all-direct-slots class)))
    (alexandria:if-let ((slot (loop :for ds in dslots
                                    ;; :do (log:info "checking ~a, against ~a" (c2mop:slot-definition-name ds) slot-name)
                                    :when (string= slot-name (c2mop:slot-definition-name ds))
                                      :return ds)))
      (if (c2mop:slot-definition-readers slot)
        (lambda (obj) (funcall (fdefinition (first (c2mop:slot-definition-readers slot))) obj))
        (lambda (obj) (slot-value obj slot-name)))
      (log:info "No slot definition found for ~a in ~a" slot-name class))))

(defun %slot-writer-fn (class slot-name)
  "Get the slot-definition for SLOT-NAME in CLASS, and return a function that
writes the slot. First try to use the accessor function, then the writer,
otherwise return a lambda that uses setf on slot-value."
  (let ((dslots (all-direct-slots class)))
    (alexandria:if-let ((slot (loop :for ds in dslots
                         :when (string= slot-name (c2mop:slot-definition-name ds))
                         :return ds)))
      (if (c2mop:slot-definition-writers slot)
          (lambda (value obj)
            (funcall (fdefinition (first (c2mop:slot-definition-writers slot))) value obj))
        (lambda (value obj) (setf (slot-value obj slot-name) value))))))

(defun generate-schema-from-class (class)
  "Generate a schema from CLASS, using reflection."
  (let ((attributes
          (loop for slot in (all-direct-slots class)
                if (null (c2mop:slot-definition-type slot))
                  do (warn "Cannot create a schema attribute for ~a because it has no type." (c2mop:slot-definition-name slot))
                else
                  collect (make-instance 'attribute
                                         :name (c2mop:slot-definition-name slot)
                                         :required (not (typep nil (c2mop:slot-definition-type slot)))
                                         :writer (%slot-writer-fn class (c2mop:slot-definition-name slot))
                                         :reader (%slot-reader-fn class (c2mop:slot-definition-name slot))
                                         ;; TODO. FIXME.
                                         ;; :documentation (c2mop:slot-definition-documentation slot)
                                         :type (make-instance 'type-schema :type (c2mop:slot-definition-type slot))
                                         ;; :type nil
                                         ))))
    (make-instance 'object-schema
                   :name (class-name class)
                   :documentation (documentation class t)
                   :attributes attributes
                   :class (class-name class))
    ))
#+(or)(defparameter *subclass-accessor-test-schema*
        (generate-schema-from-class (find-class 'subclass-accessor-test)))
;;(schemata::schema-validate *subclass-accessor-test-schema* *subclass-slot-object*)

(defmethod schema-validate ((schema object-schema) data)
  "Validate data using schema object. "

  (unless (trivial-types:association-list-p data)
    ;; (validation-error "Not an object data: ~s" data)
    (log:warn "data parameter is not an association list, coercing to one...")
    (setf data (to-alist data)))

  ;; Check unknown attributes first
  (unless (or *ignore-unknown-object-attributes*
              (ignore-unknown-attributes schema))
    (alexandria:when-let ((unknown-attributes
                           (set-difference (mapcar 'car data)
                                           (mapcar 'attribute-name (object-attributes schema))
                                           :test 'equalp
                                           :key 'string)))
      (validation-error "Attributes not part of schema: ~a" unknown-attributes)))

  ;; Validate each attribute of object
  (loop
    :for schema-attribute :in (object-attributes schema)
    :for data-attribute := (assoc (string (attribute-name schema-attribute))
                                  data
                                  :test #'equalp
                                  :key #'string)
    :do
       (cond
         ((and (not data-attribute)
               (not (attribute-optional-p schema-attribute)))
          (let ((error-msg (or (attribute-required-message schema-attribute)
                               (format nil "Attribute required: ~a"
                                       (or (attribute-external-name schema-attribute)
                                           (attribute-name schema-attribute))))))
            (validation-error error-msg)))
         ((not data-attribute)
          ;; Nothing to validate
          )
         ((not (and (attribute-optional-p schema-attribute)
                    (null data-attribute)))
          (schema-validate schema-attribute
                           (cdr data-attribute))))))

(defmethod schema-validate ((schema validating-object-schema) data)
  "Validate instance using schema object. "
  ;; Validate each attribute of object
  (loop
    :for schema-attribute :in (object-attributes schema)
    :do (let* ((data-reader (attribute-reader schema-attribute))
               (data-attribute (funcall data-reader data)))
          ;; (setf *last-attribute* schema-attribute)
            (cond
              ((and (not data-attribute)
                    (not (attribute-optional-p schema-attribute)))
               (let ((error-msg (or (attribute-required-message schema-attribute)
                                    (format nil "Attribute required: ~a"
                                            (or (attribute-external-name schema-attribute)
                                                (attribute-name schema-attribute))))))
                 (validation-error error-msg)))
              ((not data-attribute)
               ;; Nothing to validate
               )
              ((not (and (attribute-optional-p schema-attribute)
                         (null data-attribute)))
               (schema-validate schema-attribute data-attribute)))
          ))
  ;;After validating attributes, validate instance
  (let ((instance-validator (instance-validator schema)))
    (when instance-validator
      (unless (funcall instance-validator data)
        (validation-error "Instance validator failed."))))
  (log:info "WOOHOO! Validated instance: ~a. ALL GOOD" data)
  data)
