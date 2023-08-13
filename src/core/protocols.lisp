(in-package #:ai-project-tools/core)

;;;; configuration-set prtotocol
(defgeneric configuration-set-p (configuration-set)
  (:documentation "Returns true if the object is a configuration set."))

(defmethod configuration-set-p ((configuration-set configuration-set))
  t)

(defgeneric system-configuration-p (system-configuration)
  (:documentation "Returns true if the object is a llm toolkit configuration."))

(defmethod system-configuration-p ((system-configuration system-configuration))
  t)

(defun available-system-configurations ()
  ;; search *features* for all keywords beginning with :LLM-TOOLKIT.CONFIGURATIONS/
  ;; and return a list of the keywords without the prefix
  (let ((prefix "LLM-TOOLKIT.CONFIGURATIONS/"))
    (flet ((keyword-begins-with-prefix (keyword)
             ;; test if keyword begins with prefix
             (let ((keyword-string (symbol-name keyword)))
               (and (>= (length keyword-string) (length prefix))
                    (string= prefix (subseq keyword-string 0 (length prefix))))))
           (strip-prefix (keyword)
             ;; strip prefix from keyword
             (let ((keyword-string (symbol-name keyword)))
               (alexandria:make-keyword
                (subseq keyword-string (length prefix) (length keyword-string))))))
      (loop for keyword in *features*
            when (keyword-begins-with-prefix keyword)
              collect (strip-prefix keyword)))))

(defgeneric load-system-configuration (designator &rest args))

(defmethod load-system-configuration :before ((designator t) &rest args)
  (unless (member designator (available-system-configurations))
    (error
     (format nil
             "Unknown system-configuration designator: ~a. ~
             The registered system-configuration designators are:  ~a."
     designator (available-system-configurations)))))

;; Explicit hook that can be used by an asdf :after load-op method call
;; (defmethod perform :after ((op load-op) (sys (eql (find-system "ai-project-tools/core"))))
;; (register-system-components :ai-project-tools/core)
(defgeneric register-system-components (designator &rest args))

(defgeneric %make-project (system-configuration designator &rest args))

(defgeneric %make-session (system-configuration designator &rest args))

(defun make-project (designator &rest args))

(defun make-session (designator &rest args))
;;
;; Metadata store protocol
;;
(defgeneric lookup (metadata-store key &rest args)
  (:documentation "Fetch the data associated with the given key in the metadata-store."))

(defgeneric (setf lookup) (value metadata-store key)
  (:documentation "Store the data associated with the given key in the metadata-store."))

(defgeneric discard (metadata-store key)
  (:documentation "Remove the data associated with the given key in the metadata-store."))

(defgeneric clear (metadata-store)
  (:documentation "Delete all data in the metadata-store."))

;; Scoped metadata store protocol
(defgeneric scoped-key-name (scoped-metadata-store key &rest args))

;;;
;;; Core Runtime Component Resource Lookup Protocol
;;;
(defgeneric lookup-system-configuration (designator &rest args))

(defgeneric %lookup-metadata-store (system-configuration designator &rest args))

(defgeneric %lookup-project (system-configuration designator &rest args))

(defgeneric %lookup-session (system-configuration designator &rest args))

(defun lookup-metadata-store (designator &rest args)
  (let ((system-configuration (get-current-system-configuration)))
    (if system-configuration
        (%lookup-metadata-store system-configuration designator args)
        (error "No system-configuration is currently set."))))

(defun lookup-project (designator &rest args)
  (let ((system-configuration (get-current-system-configuration)))
    (if system-configuration
        (%lookup-project system-configuration designator args)
        (error "No system-configuration is currently set."))))

(defun lookup-session (designator &rest args)
  (let ((system-configuration (get-current-system-configuration)))
    (if system-configuration
        (%lookup-session system-configuration designator args)
        (error "No system-configuration is currently set."))))

(defun get-current-system-configuration (&key (designator :default))
  (or *current-system-configuration* (lookup-system-configuration designator)))

(defun get-root-metadata-store ()
  (or *root-metadata-store* (lookup-metadata-store :root)))

(defun get-current-metadata-store (&key (designator :default))
  (or *current-metadata-store* (lookup-metadata-store designator)))

(defun get-current-project (&key (designator :default))
  (or *current-project* (lookup-project designator)))

(defun get-current-session (&key (designator :default))
  (or *current-session* (lookup-session designator)))

(defun call-with-system-configuration (fn &key
                                        (system-configuration nil system-configuration-p)
                                        (system-configuration-designator :default))
  (let ((*current-system-configuration*
          (if (and system-configuration-p system-configuration)
              system-configuration
              (lookup-system-configuration system-configuration-designator)))
    (funcall fn))))

(defun call-with-metadata-store (fn &key
                                  (metadata-store nil metadata-store-p)
                                  (metadata-store-designator :default))
  (let ((*current-metadata-store* (if (and metadata-store-p metadata-store)
                                      metadata-store
                                      (get-current-metadata-store :designator metadata-store-designator))))
    (funcall fn)))

(defun call-with-root-metadata-store (fn)
  (let ((*current-metadata-store* (get-root-metadata-store)))
    (funcall fn)))

(defun call-with-project (fn &key
                           (project nil project-p)
                           (project-designator :default))
  (let ((*current-project* (if (and project-p project)
                               project
                               (lookup-project project-designator)))
    (funcall fn))))

(defun call-with-session (fn &key
                           (session nil session-p)
                           (session-designator :default))
  (let ((*current-session* (if (and session-p session)
                               session
                               (lookup-session session-designator))))
    (funcall fn)))

;; Macros for using the context-setting functions
(defmacro with-system-configuration ((&key
                                        (system-configuration :default)
                                        (system-configuration-p nil system-configuration-p))
                                     &body body)
  "Run BODY with SYSTEM-CONFIGURATION as the current system configuration."
  (with-thunk (body)
    `(call-with-system-configuration ,body
      ,@(if (and system-configuration-p system-configuration)
            `(:system-configuration ,system-configuration)
            `(:system-configuration-designator ,system-configuration)))))

(defmacro with-metadata-store ((&key
                                  (metadata-store nil metadata-store-p)
                                  (metadata-store-designator :default))
                               &body body)
  "Run BODY with METADATA-STORE as the current metadata-store."
  (with-thunk (body)
    `(call-with-metadata-store ,body
      ,@(if (and metadata-store-p metadata-store)
            `(:metadata-store ,metadata-store)
            `(:metadata-store-designator ,metadata-store-designator)))))

(defmacro with-root-metadata-store (() &body body)
  "Run BODY with METADATA-STORE as the current root-metadata-store."
  (with-thunk (body)
    `(call-with-root-metadata-store ,body)))

(defmacro with-project ((&key
                           (project nil project-p)
                           (project-designator :default))
                        &body body)
  "Run BODY with PROJECT as the current project."
  (with-thunk (body)
    `(call-with-project ,body
      ,@(if (and project-p project)
            `(:project ,project)
            `(:project-designator ,project-designator)))))

(defmacro with-session ((&key
                           (session nil session-p)
                           (session-designator :default))
                        &body body)
  "Run BODY with SESSION as the current session."
  (with-thunk (body)
    `(call-with-session ,body
      ,@(if (and session-p session)
            `(:session ,session)
            `(:session-designator ,session-designator)))))

#+nil (macroexpand-1 '(with-session (:session "FOO") (format nil "SESSION: ~a" (get-current-session))))
#+nil (macroexpand-1 '(with-session (:session-designator :default) (format nil "SESSION: ~a" (get-current-session))))
#+nil (let ((sess "SESS"))(with-session (:session sess) (format nil "SESSION: ~a" (get-current-session))))
#+nil (with-session (:session-designator :default) (format nil "SESSION: ~a" (get-current-session)))


;; Top-level macro that encloses all the other context-setting macros
(defmacro with-standard-runtime-context ((&key
                                            (system-config nil system-config-p)
                                            (system-config-designator :default)
                                            (metadata-store nil metadata-store-p)
                                            (project nil project-p)
                                            (project-designator :default)
                                            (session nil session-p)
                                            (session :default))
                                          &body body)
  "Run BODY within a standard runtime context, with system configuration,
  project, and session variables appropriately set."
  `(with-system-configuration (:config ,system-config)
        (with-metadata-store (:metadata-store ,metadata-store)
          (with-project (:project ,project)
            (with-session (:session ,session)
              ,@body)))))

;;;; runnable protocol
(defgeneric runnable-p (runnable)
  (:documentation "Returns true if the object is runnable."))

(defmethod runnable-p ((runnable runnable))
    t)

(defgeneric run (runnable &rest args)
  (:documentation "Runs the runnable object."))

;;;; stepped-runnable protocol
(defgeneric stepped-runnable-p (stepped-runnable)
  (:documentation "Returns true if the object is stepped-runnable."))

(defmethod stepped-runnable-p ((stepped-runnable stepped-runnable))
    t)

(defgeneric run-step (runnable &rest args)
  (:documentation "Runs one step in the runnable object."))
