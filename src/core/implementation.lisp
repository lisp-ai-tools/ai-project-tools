(in-package #:ai-project-tools/core)

(defmethod run :around ((runner realtime-timed-runnable) &rest args)
  (unwind-protect
       (progn
         ;; reset the start/end time and duration
         (setf (end-time runner) nil
               (duration runner) nil
               (start-time runner)(get-internal-real-time))
         (apply #'call-next-method runner args))
    (setf (end-time runner) (get-internal-real-time)
          (duration runner) (- (end-time runner) (start-time runner)))))

(defmethod run :around ((runner timestamp-timed-runnable) &rest args)
  (unwind-protect
       (progn
         ;; reset the start/end time and duration
         (setf (end-time runner) nil
               (duration runner) nil
               (start-time runner)(local-time:now))
         (apply #'call-next-method runner args))
    (setf (end-time runner) (local-time:now)
          (duration runner) (local-time:timestamp-difference (end-time runner) (start-time runner)))))

(defclass simple-in-memory-system-configuration (system-configuration) ()
  (:documentation "A simple in-memory system configuration that does not persist."))

(defclass file-backed-system-configuration (system-configuration)
  ((%working-directory :initarg :working-directory :accessor working-directory))
  (:documentation "A file-backed system configuration that persists to disk."))

(defun get-system-configuration-registry ()
  (or system-configuration-registry
      (setf system-configuration-registry (make-hash-table :test 'equal))))

(defun get-system-configuration (designator)
  (multiple-value-bind (configuration exists-p)
      (gethash designator system-configuration-registry)
    (if exists-p
        configuration
        (error "No configuration found for ~a." designator))))

(defun register-system-configuration (designator configuration)
  (setf (gethash designator system-configuration-registry)
        configuration))

(defun remove-system-configuration (designator)
  (remhash designator system-configuration-registry))

(defun clear-system-configuration-registry ()
  (clrhash system-configuration-registry))

(defun %ensure-project-root-directory (project)
  (let ((root-directory (root-directory project)))
    (unless (uiop/filesystem:directory-exists-p root-directory)
      (uiop/filesystem:ensure-all-directories-exist (list root-directory)))))

(defun %ensure-session-working-directory (session)
  (let* ((project-dir (root-directory (project session)))
         (session-dir-path (format nil "sessions/~a"
                                   (local-time:format-timestring nil (local-time:now)
                                                                 :format local-time:+iso-8601-date-format+)))
         (working-directory (uiop/pathname:ensure-directory-pathname
                             (uiop/pathname:merge-pathnames* session-dir-path project-dir))))
    (unless (uiop/filesystem:directory-exists-p working-directory)
      (uiop/filesystem:ensure-all-directories-exist (list working-directory)))
    (setf (working-directory session) working-directory)))

(defmethod %make-project ((config simple-in-memory-system-configuration)
                          (designator (eql :default))
                          &rest args)
  (let ((root-metadata-store (metadata-store config))
        (project (make-instance 'project
                 :name "default-ad-hoc"
                 :description "Default ad hoc project for noodling around with LLMs at the REPL"
                 :root-directory (uiop/pathname:ensure-directory-pathname (uiop:xdg-data-pathname "ad-hoc" :output)))))
        (%ensure-project-root-directory project)
    (setf (metadata-store project) (make-instance 'memory-scoped-metadata-store
                                    :name (name project)
                                    :parent root-metadata-store
                                    :store (store root-metadata-store)
                                    :schema (list :fake t))
          (lookup (root-metadata-store config)
                  (format nil "/projects/~a" (name project))) project)
    (log:info "Created ad-hoc project: ~a." project)
    project))

(defmethod %make-session ((config simple-in-memory-system-configuration)
                          (designator (eql :default))
                          &rest args &key project)
  (let*  ((session (make-instance 'session
                                  :name "ad-hoc-session"
                                  :description "Ad hoc session for noodling around with LLMs at the REPL"
                                  :project project
                                  :start-time (local-time:now))))
    (setf (metadata-store session) (make-instance 'memory-scoped-metadata-store
                                    :name (name session)
                                    :parent (metadata-store project)
                                    :store (store (metadata-store project))
                                    :schema (list :fake t))
          (sessions project) (cons (sessions project) session)
          (lookup (metadata-store config) (format nil "/sessions/~a" (name session))) session)
    (log:info "Created ad-hoc non-persistent in-memory session: ~a." session)
    session))

(defmethod initialize-instance :after ((session session) &key project)
  (unless (null project)
    (setf (sessions project) (pushnew session (sessions project))))
  (unless (and (slot-boundp session '%start-time) (start-time session))
    (setf (start-time session) (local-time:now))))

;; Basic memory-metadata-store implementation
(defmethod lookup ((store simple-memory-metadata-store) key &rest args &key default)
  (gethash key (store store) default))

(defmethod (setf lookup) (value (store simple-memory-metadata-store) key)
  (setf (gethash key (store store)) value))

(defmethod discard ((store metadata-store) key)
  (remhash key (store store)))

(defmethod clear ((store metadata-store))
  (clrhash (store store)))

;; Scoped metadata store implementation
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

(defmethod initialize-instance :after ((scoped-store scoped-metadata-store)
                                       &key name parent store schema)
  (declare (ignorable name store schema))
  (if (not (null parent))
    (setf (children parent) (pushnew scoped-store (children parent))
          (scope-path scoped-store) (scoped-path scoped-store)
          (store scoped-store) (store parent))
    (unless (store scoped-store)
      (setf (store scoped-store) (make-hash-table :test 'equal)))))

;; has-metadata-store implementation
(defmethod lookup ((store has-metadata-store) (key string) &rest args &key default)
  (lookup (metadata-store store) key :default default))
(defmethod (setf lookup) (value (store has-metadata-store) (key string))
  (setf (lookup (metadata-store store) key) value))
(defmethod discard ((store has-metadata-store) (key string))
    (discard (metadata-store store) key))
(defmethod clear ((store has-metadata-store))
    (clear (metadata-store store)))

(defun get-default-system-configuration ()
    (multiple-value-bind (configuration exists-p)
      (gethash :default system-configuration-registry)
      configuration))

(defmethod lookup-system-configuration ((designator (eql :default)) &rest args)
  (declare (ignore args))
  (get-default-system-configuration))

(defmethod %lookup-project ((app application) (designator (eql :default)) &rest args)
  (declare (ignore args))
  (let ((key (concatenate 'string "/runtime/projects/" (string-downcase (string :default)))))
    (lookup (metadata-store app) key)))

(defmethod %lookup-session ((app application) (designator (eql :default)) &rest args)
  (declare (ignore args))
  (let ((key (concatenate 'string "/runtime/sessions/" (string-downcase (string :default)))))
    (lookup (metadata-store app) key)))

;; This takes care of the registration of the system configuration, and setting default if requested.
(defmethod load-system-configuration :around ((designator t) &rest args &key (set-default nil))
  (flet ((maybe-set-default (config)
           (multiple-value-bind (default-config default-exists-p)
                   (gethash :default (get-system-configuration-registry))
             (when (or (not default-exists-p) set-default)
                       (unless (and default-exists-p (eql designator (designator default-config)))
                         (log:info "Setting default system configuration to ~a." config)
                         (setf (gethash :default (get-system-configuration-registry)) config)))
             config)))
  (multiple-value-bind (config exists-p)
      (gethash designator (get-system-configuration-registry))
    (log:info "config: ~a, exists-p: ~a" config exists-p)
    (if exists-p
        (maybe-set-default config)
        (let ((config (call-next-method)))
          (setf (gethash designator (get-system-configuration-registry)) config)
          (maybe-set-default config)
          config)))))

(defmethod load-system-configuration ((designator (eql :simple-in-memory-system-configuration))
                                      &rest args
                                      &key
                                        (set-default nil)
                                        (project-designator :default project-designator-p)
                                        (session-designator :default session-designator-p))
  (declare (ignore args))
  (let* ((config (make-instance 'simple-in-memory-system-configuration
                 :designator designator
                 :name "Simple in-memory system configuration."
                 :description "A simple in-memory system configuration that does not persist."
                 :root-metadata-store (make-instance 'memory-scoped-metadata-store
                                                     :name "/root"
                                                     :store (make-hash-table :test #'equal)
                                                     :parent nil
                                                     :schema (list :fake t))))
         (project (%make-project config project-designator))
         (session (%make-session config session-designator :project project)))
    config))

;; shorter alias
(defmethod load-system-configuration ((designator (eql :ad-hoc-in-memory)) &rest args)
    (apply #'load-system-configuration (list :simple-in-memory-system-configuration args)))

(defmethod load-system-configuration ((designator (eql :default)) &rest args &key (set-default nil))
  (let ((configuration (get-default-system-configuration)))
    (if configuration
        ;; If we already have set a default configuration, use its designator.
        (apply #'load-system-configuration (append (list (designator configuration)) args))
        ;; otherwise, just make a simple-in-memory-system-configuration
        (apply #'load-system-configuration (append (list :simple-in-memory-system-configuration) args)))))

(defmethod register-system-components ((designator (eql :ai-project-tools/core)) &rest args)
  (pushnew :ai-project-tools.configurations/default *features*)
  (pushnew :ai-project-tools.configurations/simple-in-memory-system-configuration *features*)
  (pushnew :ai-project-tools.configurations/ad-hoc-in-memory *features*)
  ;; (load-system-configuration :simple-in-memory-system-configuration)
  )
