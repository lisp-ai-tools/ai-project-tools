(in-package #:ai-project-tools/core)

;;(ql:quickload '(:clavier))
;; (uiop:add-package-local-nickname #:v #:ai-project-tools/core)

(let ((username-validator (clavier:not-blank "Must provide full username")))
  (defun %valid-username-p (name)
    (funcall username-validator name))
  (defun %valid-username (name)
    (funcall username-validator name :error-p t)))

(let ((email-validator (clavier:valid-email "Must provide valid email")))
  (defun %valid-email-p (email)
    (funcall email-validator email))
  (defun %valid-email (email)
    (funcall email-validator email :error-p t)))

(let ((age-validator (clavier:&&
                      (clavier:is-an-integer "Must be an integer")
                      (clavier:&&
                       (clavier:greater-than 18 "Must be at least 18")
                       (clavier:less-than 120 "Must be at most 120")))))
(defun %valid-age-p (age)
    (funcall age-validator age))
(defun %valid-age (age)
    (funcall age-validator age :error-p t)))

(deftype valid-username () `(satisfies %valid-username-p))
(deftype valid-email () `(satisfies %valid-email-p))
(deftype valid-age () `(satisfies %valid-age-p))

(defclass test-person ()
  ((name :initarg :name :accessor name :type valid-username)
   (age :initarg :age :accessor age :type valid-age)
   (email :initarg :email :accessor email :type valid-email)))

#+(or)(let ((lt-val (clavier:less-than 120 "Must be at most 120")))
        (clavier:validate lt-val 220 :error-p t))
#+(or) (let ((validator (make-instance 'clavier:and-validator
                                       :x (make-instance 'clavier:greater-than-validator :number 10)
                                       :y (make-instance 'clavier:less-than-validator :number 20))))
         (funcall validator 33)
         (funcall validator 15)
         (funcall validator 9))


#+(or)(defparameter *valid-tp-1*
        (make-instance 'test-person :name "John Doe"
                                    :age 20 :email "jd@foo.blah"))

#+(or)(defparameter *test-person-schema*
        (schemata:generate-schema-from-class (find-class 'test-person)))
#+(or)(progn
(schemata:validate-with-schema *test-person-schema* *valid-tp-1*)
(schemata:validate-with-schema *test-person-schema* '((:name . "John Doe")
                                                      (:age . 20)
                                                      (:email . "blah@foo.com")))
        )

#+(or)(defparameter *all-direct-slots* (cl-mop::all-direct-slots (find-class 'test-person)))

;;(defparameter *age-st* (c2mop:slot-definition-type (first *all-direct-slots*)))
#+(or)(typep nil *age-st*)
;;(c2mop:slot-definition-name (first *all-direct-slots*))
;;(schemata::%slot-reader-fn (find-class 'test-person) "NAME")

#+(or)(let* ((slot (first *all-direct-slots*))
             (slot-name (c2mop:slot-definition-name slot))
             (slot-type (c2mop:slot-definition-type slot))
             (slot-required-p (not (typep nil slot-type)))
             (slot-reader-fn (schemata::%slot-reader-fn (find-class 'test-person) slot-name))
             (slot-writer-fn (schemata::%slot-writer-fn (find-class 'test-person) slot-name))
             (slot-type-schema (make-instance 'schemata:type-schema :type slot-type))
             )
        (make-instance 'schemata:attribute
                       :name slot-name
                       :required slot-required-p
                       ;; :required nil
                       :writer slot-writer-fn
                       :reader slot-reader-fn
                       :type slot-type-schema
                       ;; :type nil
                        )
        )
