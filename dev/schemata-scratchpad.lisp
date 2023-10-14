(in-package :schemata)

(defclass accessor-test ()
  ((%slot-w-reader :initarg :slot-w-reader :initform nil :reader slot-w-reader)
   (%slot-w-writer :initarg :slot-w-writer :initform nil :writer slot-w-writer)
   (%slot-w-accessor :initarg :slot-w-accessor :initform nil :accessor slot-w-accessor)
   (%slot-bare :initarg :slot-bare :initform nil)))

(defclass subclass-accessor-test (accessor-test)
  ((%slot-subw-reader :initarg :slot-subw-reader :reader slot-subw-reader :type integer)
   (%slot-subw-writer :initarg :slot-subw-writer :writer slot-subw-writer :type string)
   (%slot-subw-accessor :initarg :slot-subw-accessor :accessor slot-subw-accessor :type float)
   (%slot-subbare :initarg :slot-subbare :type integer)))

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

#+(or)(defparameter *slot-object*
        (make-instance 'accessor-test :slot-w-reader 15
                                      :slot-w-writer "MOO" :slot-w-accessor 3.5
                                      :slot-bare 4))
#+(or)(defparameter *subclass-slot-object*
        (make-instance 'subclass-accessor-test :slot-w-reader 1 :slot-w-writer "MOO" :slot-w-accessor 3.5 :slot-bare 4
                       :slot-subw-reader 1 :slot-subw-writer "MOO" :slot-subw-accessor 3.5 :slot-subbare 4))
;;(slot-w-writer 5 *slot-object*)
;;(cl-mop::to-plist *slot-object*)
;;(cl-mop::to-plist *subclass-slot-object*)
;;(c2mop:class-direct-slots (find-class 'subclass-accessor-test))
;;(c2mop:class-slots (find-class 'subclass-accessor-test))
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
;;(schemata:find-schema 'accessor-test-schema)
(defparameter *last-attribute* nil)
(schemata::schema-validate 'accessor-test-schema *slot-object*)
(schemata::validate-with-schema 'accessor-test-schema *slot-object* :collect-errors t)
(slot-w-writer "MOO" *slot-object*)


(defmethod schema-validate ((schema validating-object-schema) data)
  "Validate instance using schema object. "
  ;; Validate each attribute of object
  (loop
    :for schema-attribute :in (object-attributes schema)
    :do (let* ((data-reader (attribute-reader schema-attribute))
               (data-attribute (funcall data-reader data)))
          (setf *last-attribute* schema-attribute)
            ;; (log:info "attr-reader: ~a, with schema: ~a"
            ;;           attr-reader schema-attribute)
            ;; (log:info "Validating attribute: ~a, with schema: ~a"
            ;;           data-attribute schema-attribute)
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

(defun %slot-definitions-of-class (class)
  "Return a list of slot-definitions for CLASS."
  (c2mop:class-slots class))
;;(defparameter *sub-slot-definitions* (%slot-definitions-of-class (find-class 'subclass-accessor-test)))

(defun %direct-slots-of-class (class)
  "Return a list of slot-definitions for CLASS."
  (c2mop:class-direct-slots class))
;;(defparameter *sub-direct-slots* (%direct-slots-of-class (find-class 'subclass-accessor-test)))

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
;;(all-slot-readers (find-class 'subclass-accessor-test))
;;(all-slot-writers (find-class 'subclass-accessor-test))

;;(defparameter *slot-definitions* (%slot-definitions-of-class (find-class 'accessor-test)))
;;(defparameter *slots* (c2mop:class-slots (find-class 'accessor-test)))
;;(defparameter *direct-slots* (%direct-slots-of-class (find-class 'accessor-test)))
;;(nth 0 *slot-definitions*)
;;(c2mop:slot-definition-readers (nth 0 *direct-slots*))
;;(c2mop:slot-definition-readers (nth 0 *slots*))
;;(c2mop:slot-definition-readers (nth 3 *slots*))
;;(sb-mop:slot-definition-readers (nth 0 *slot-definitions*))

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
;;(%slot-reader-fn (find-class 'accessor-test) '%slot-w-reader)
;;(%slot-reader-fn (find-class 'accessor-test) '%slot-w-accessor)
;;(%slot-reader-fn (find-class 'accessor-test) "%slot-bare")
;;(%slot-reader-fn (find-class 'accessor-test) '%slot-bare)
#+(or)(let ((rfn (%slot-reader-fn (find-class 'accessor-test) '%slot-w-reader))) (funcall rfn *slot-object*))
#+(or)(let ((rfn (%slot-reader-fn (find-class 'accessor-test) '%slot-w-accessor))) (funcall rfn *slot-object*))
#+(or)(let ((rfn (%slot-reader-fn (find-class 'accessor-test) '%slot-bare))) (funcall rfn *slot-object*))

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

#+(or)(let ((wfn (%slot-writer-fn (find-class 'accessor-test) '%slot-w-writer))) (funcall wfn 255 *slot-object*))
#+(or)(let ((wfn (%slot-writer-fn (find-class 'accessor-test) '%slot-w-writer))) (funcall wfn 255 *slot-object*))
#+(or)(let ((wfn (%slot-writer-fn (find-class 'accessor-test) '%slot-w-accessor))) (funcall wfn 353 *slot-object*))
#+(or)(let ((wfn (%slot-writer-fn (find-class 'accessor-test) '%slot-bare))) (funcall wfn 1000 *slot-object*))
;;(%slot-writer-fn (find-class 'accessor-test) '%slot-w-accessor)

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
                                         ;; TODO: use accessors, writers, readers specificed in slots
                                         :writer (%slot-writer-fn class (c2mop:slot-definition-name slot))
                                         :reader (%slot-reader-fn class (c2mop:slot-definition-name slot))
                                         ;; TODO. FIXME.
                                         ;; :documentation (c2mop:slot-definition-documentation slot)
                                         :type (make-instance 'type-schema :type (c2mop:slot-definition-type slot))))))
    (make-instance 'object-schema
                   :name (class-name class)
                   :documentation (documentation class t)
                   :attributes attributes
                   :class (class-name class))))
;;(defparameter *subclass-accessor-test-schema* (generate-schema-from-class (find-class 'subclass-accessor-test)))
;;(schemata::schema-validate *subclass-accessor-test-schema* *subclass-slot-object*)

(defmethod schema-validate ((schema object-schema) data)
  "Validate data using schema object. "

  (unless (trivial-types:association-list-p data)
    ;; (validation-error "Not an object data: ~s" data)
    (log:warn "data parameter is not an association list, coercing to one...")
    (setf data (cl-mop::to-alist data))
    )

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
