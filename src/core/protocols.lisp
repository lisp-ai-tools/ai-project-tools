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
(defgeneric lookup (metadata-store key)
  (:documentation "Fetch the data associated with the given key in the metadata-store."))

(defgeneric (setf lookup) (value metadata-store key)
  (:documentation "Store the data associated with the given key in the metadata-store."))

(defgeneric remove-entry (metadata-store key)
  (:documentation "Remove the data associated with the given key in the metadata-store."))

(defgeneric clear (metadata-store)
  (:documentation "Delete all data in the metadata-store."))

;; Scoped metadata store protocol
(defgeneric scoped-key-name (scoped-metadata-store key &rest args))

(defgeneric lookup-system-configuration (designator &rest args))

(defgeneric lookup-project (designator &rest args))

(defgeneric lookup-session (designator &rest args))

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

(defun call-with-system-configuration (fn &key (config :default))
  (let ((*current-system-configuration* (get-current-system-configuration :designator config)))
    (funcall fn)))

(defun call-with-metadata-store (fn &key (metadata-store :default))
  (let ((*current-metadata-store* (get-current-metadata-store :designator metadata-store)))
    (funcall fn)))

(defun call-with-root-metadata-store (fn)
  (let ((*current-metadata-store* (get-root-metadata-store)))
    (funcall fn)))

(defun call-with-project (fn &key (project :default))
  (let ((*current-project* (get-current-project :designator project)))
    (funcall fn)))

(defun call-with-session (fn &key (session :default))
  (let ((*current-session* (get-current-session :designator session)))
    (funcall fn)))

;; Macros for using the context-setting functions
(defmacro with-system-configuration ((&key (config :default)) &body body)
  "Run BODY with CONFIG as the current system configuration."
  (with-thunk (body)
    `(call-with-system-configuration ,body :config ,config)))

(defmacro with-metadata-store ((&key (metadata-store :default)) &body body)
  "Run BODY with METADATA-STORE as the current metadata-store."
  (with-thunk (body)
    `(call-with-metadata-store ,body :metadata-store ,metadata-store)))

(defmacro with-root-metadata-store ((&key (metadata-store :default)) &body body)
  "Run BODY with METADATA-STORE as the current metadata-store."
  (with-thunk (body)
    `(call-with-root-metadata-store ,body)))

(defmacro with-project ((&key (project nil project-p) (project-designator :default) ) &body body)
  "Run BODY with PROJECT as the current project."
  (with-thunk (body)
    `(call-with-project ,body :project ,project)))

(defmacro with-session ((&key (session nil session-p) (session-designator :default)) &body body)
  "Run BODY with SESSION as the current session."
  (with-thunk (body)
    `(call-with-session ,body :session ,session)))

;; Top-level macro that encloses all three
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

