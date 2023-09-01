(in-package #:ai-project-tools/core)

;;
;; Basic hash-table based flat memory-metadata-store implementation
;;
(defmethod lookup ((store simple-memory-metadata-store) key &rest args &key default)
  (gethash key (store store) default))

(defmethod (setf lookup) (value (store simple-memory-metadata-store) key)
  (setf (gethash key (store store)) value))

(defmethod discard ((store metadata-store) key)
  (remhash key (store store)))

(defmethod clear ((store metadata-store))
  (clrhash (store store)))

;;
;; Scoped metadata store implementation
;;

;; Auxillary functions for managing scoped hash table based implementation
(defun %path-segs (key &optional (separator-string "/"))
  (split-sequence:split-sequence
   separator-string
   (string-left-trim separator-string key) :test #'string=))

(defun %direct-child-path-p (prefix key)
  (let ((prefix-segs (%path-segs prefix))
        (key-segs (%path-segs key)))
    (and (= (1+ (length prefix-segs)) (length key-segs))
         (every #'string= prefix-segs (butlast key-segs)))))

(defun %get-direct-children (table prefix)
  (let ((prefix-children nil))
    (maphash (lambda (key value)
               (when (%direct-child-path-p prefix key)
                 (push (cons key value) prefix-children)))
             table)
    prefix-children))

;; Get direct children of a store that match a predicate
(defun %get-direct-children-matching (table prefix pred-fn)
  (let ((prefix-children nil))
    (maphash (lambda (key value)
               (when (and (%direct-child-path-p prefix key)
                          (funcall pred-fn value))
                 (push (cons key value) prefix-children)))
             table)
    prefix-children))

;; Get all descendants of a prefix
(defun %get-subtree (table prefix)
  (let ((descendants nil))
    (maphash (lambda (key value)
               (when (search prefix key)
                 (push (cons key value) descendants)))
             table)
    descendants))

;; Get all descendants of a prefix that match a predicate
(defun %get-subtree-matching (table prefix pred-fn)
  (let ((descendants nil))
    (maphash (lambda (key value)
               (when (and (search prefix key)
                          (funcall pred-fn value))
                 (push (cons key value) descendants)))
             table)
    descendants))

;; Clear direct children of a store, leaving siblings intact
(defun %discard-direct-children (table prefix)
  (maphash (lambda (key value)
             (when (%direct-child-path-p prefix key)
               (remhash key table)))
           table))

;; Clear all descendants of a prefix
(defun %discard-subtree (table prefix)
  (maphash (lambda (key value)
             (when (search prefix key)
               (remhash key table)))
           table))

(defmethod scoped-path ((store scoped-metadata-store) &rest args)
  (declare (ignore args))
  (let* ((names-from-root (nreverse (loop :for current := store :then (parent current)
                                          :until (null current)
                                          :collect (name current))))
         (merged-names (reduce (lambda (a b)
                                 (concatenate 'string a (scope-delimeter store) b))
                               names-from-root)))
    (concatenate 'string (scope-delimeter store) merged-names)))

(defmethod scoped-key-name ((store scoped-metadata-store) (key string)
                            &rest args)
  (declare (ignore args))
  (concatenate 'string (scoped-path store) (scope-delimeter store) key))

(defmethod lookup ((store memory-scoped-metadata-store) (key-string string) &rest args &key default)
  (let ((scoped-key (scoped-key-name store key-string)))
    (gethash scoped-key (store store) default)))

(defmethod (setf lookup) (value (store memory-scoped-metadata-store) (key-string string))
  (let ((scoped-key (scoped-key-name store key-string)))
    (setf (gethash scoped-key (store store)) value)))

(defmethod discard ((store memory-scoped-metadata-store) (key-string string))
  (let ((scoped-key (scoped-key-name store key-string)))
    (remhash scoped-key (store store))))

(defmethod clear ((store memory-scoped-metadata-store))
  (if (not (parent store))
    (clrhash (store store))
    (progn
      (%discard-subtree (store store) (scoped-path store)))))

(defmethod initialize-instance :after ((scoped-store scoped-metadata-store)
                                       &key schema (store nil store-provided-p)
                                         (scope-delimeter nil scope-delimeter-provided-p)
                                         (parent nil parent-provided-p)
                                         name)
  (declare (ignorable name store schema store-provided-p
                      scope-delimeter-provided-p parent-provided-p))
  (if parent
      (progn
        (when store-provided-p
          (error "Cannot provide store when parent is provided. This store will use parent's store instead"))
        (when (not (string= (scope-delimeter parent) (scope-delimeter scoped-store)))
          (error "Scope delimeter mismatch between parent and child scoped store"))
        (setf (children parent) (pushnew scoped-store (children parent))
              (store scoped-store) (store parent)
              (scope-path scoped-store) (scoped-path scoped-store)))
    (progn
      (unless (store scoped-store)
        (setf (store scoped-store) (make-hash-table :test 'equal)))
      (setf (scope-path scoped-store) (scoped-path scoped-store)))))

;; has-metadata-store implementation -- containing object acts as a proxy
(defmethod lookup ((store has-metadata-store) (key string) &rest args &key default)
  (lookup (metadata-store store) key :default default))
(defmethod (setf lookup) (value (store has-metadata-store) (key string))
  (setf (lookup (metadata-store store) key) value))
(defmethod discard ((store has-metadata-store) (key string))
    (discard (metadata-store store) key))
(defmethod clear ((store has-metadata-store))
    (clear (metadata-store store)))

(defun %generate-new-scope-name ()
  "Generate a random name for the new scope based on timestamp"
  (let ((timestamp (get-internal-real-time)))
    (format nil "scope-~a" timestamp)))
;;(%generate-new-scope-name)

(defun call-with-new-metadata-scope (thunk
                                     &key
                                       (scope-name nil scope-name-provided-p)
                                       (schema '(:fake schema)))
  (let* ((parent-scope (or *current-metadata-store* (error "No current metadata store")))
        (new-scope-name (if scope-name-provided-p scope-name (%generate-new-scope-name)))
        (new-scope (make-instance 'scoped-metadata-store
                                  :parent parent-scope
                                  :name new-scope-name
                                  :schema schema)))
    (let ((*current-metadata-store* new-scope))
      (funcall thunk))))

(defmacro with-new-metadata-scope ((&key
                                      (scope-name nil scope-name-provided-p)
                                      (schema '(:fake schema)))
                                   &body body)
  `(call-with-new-metadata-scope
    (lambda () ,@body)
    ,@(when scope-name-provided-p :scope-name `,scope-name)))

(defun call-with-root-metadata-scope (thunk
                                      &key
                                        (scope-name nil scope-name-provided-p)
                                        (schema '(:fake schema)))
  (let* ((new-scope-name (if scope-name-provided-p scope-name "root"))
        (new-scope (make-instance 'scoped-metadata-store
                                  :parent nil
                                  :name new-scope-name
                                  :schema schema)))
    (let ((*root-metadata-store* new-scope)
          (*current-metadata-store* new-scope))
      (funcall thunk))))

(defmacro with-root-metadata-scope ((&key
                                       (scope-name nil scope-name-provided-p)
                                       (schema '(:fake schema)))
                                    &body body)
  `(call-with-root-metadata-scope
    (lambda () ,@body)
    ,@(when scope-name-provided-p :scope-name `,scope-name)))

#+(or)
(with-root-metadata-scope ()
  (with-new-metadata-scope ()
    (log:info "My scope is ~a" (scoped-path *current-metadata-store*))))
