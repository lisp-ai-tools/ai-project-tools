;; (ql:quickload '(:ai-project-tools :cl-protobufs))
(ql:quickload :cl-protobufs)


(defun get-ai-project-tools-root-path ()
  ;; assumes ai-project-tools system has been loaded
  ;; from there, we can find files and directories relative to the root
    (let ((system (asdf:find-system :ai-project-tools)))
        (or (and system (asdf:system-source-directory system))
            (error "ai-project-tools system not found"))))

;;(get-ai-project-tools-root-path)

(defun get-cl-protobufs-root-path ()
  ;; assumes ai-project-tools system has been loaded
  ;; from there, we can find files and directories relative to the root
    (let ((system (asdf:find-system :cl-protobufs)))
        (or (and system (asdf:system-source-directory system))
            (error "cl-protobufs system not found"))))

;;(get-cl-protobufs-root-path)

(defun setup-protoc-env-vars ()
  (let ((cl-protobufs-root (get-cl-protobufs-root-path))
        (local-protoc-bin-path (merge-pathnames "protobuf-install/bin" (get-cl-protobufs-root-path)))
        (local-protoc-lib-path (merge-pathnames "protobuf-install/lib" (get-cl-protobufs-root-path)))
        (local-protoc-include-path (merge-pathnames "protobuf-install/include" (get-cl-protobufs-root-path))))
    (setf (uiop/os:getenv "LD_LIBRARY_PATH")
          (concatenate'string  (namestring local-protoc-lib-path) ":" (uiop/os:getenv "LD_LIBRARY_PATH")))
    (setf (uiop:getenv "PATH")
          (concatenate 'string (namestring local-protoc-bin-path) ":" (uiop:getenv "PATH") ))))

;;(setup-protoc-env-vars)


(defun get-ml-metadata-path ()
  (let ((root (get-ai-project-tools-root-path)))
    (uiop/filesystem:directory-exists-p (merge-pathnames "data/google-ml-metadata" root))))

;;(uiop/filesystem:directory-exists-p  (get-ml-metadata-path))

(defun get-query-config-pbtxt-path ()
  (let ((root (get-ml-metadata-path)))
    (merge-pathnames "query_config_sqlite.pbtxt" root)))

;;(uiop/filesystem:file-exists-p  (get-query-config-pbtxt-path))

(defun get-ml-metadata-lisp-pb-path ()
  (let ((root (get-ml-metadata-path)))
    (uiop/filesystem:directory-exists-p
     (uiop/filesystem:directory-exists-p (merge-pathnames "generated-lisp-pb" root)))))
;; (uiop/filesystem:directory-exists-p  (get-ml-metadata-lisp-pb-path))

(defun get-ml-metadata-source-lisp-pb-path ()
  (let ((root (get-ml-metadata-lisp-pb-path)))
    (merge-pathnames "metadata-source.lisp" root)))

(defun get-ml-metadata-store-lisp-pb-path ()
  (let ((root (get-ml-metadata-lisp-pb-path)))
    (merge-pathnames "metadata-store.lisp" root)))

(defun get-ml-metadata-store-service-lisp-pb-path ()
  (let ((root (get-ml-metadata-lisp-pb-path)))
    (merge-pathnames "metadata-store-service.lisp" root)))

;; (uiop/filesystem:file-exists-p  (get-ml-metadata-source-lisp-pb-path))

(defun load-ml-metadata-source-lisp-pb ()
  (let ((path (get-ml-metadata-source-lisp-pb-path)))
    (load path)))
;;(load-ml-metadata-source-lisp-pb)

(defun load-ml-metadata-store-lisp-pb ()
    (let ((path (get-ml-metadata-store-lisp-pb-path)))
        (load path)))

(defun metadata-store-descriptor ()
  (cl-protobufs:find-file-descriptor 'cl-protobufs.ml-metadata.proto:metadata_store)
  ;; (cl-protobufs:find-file-descriptor #p"metadata_store.proto")
  )
;; (metadata-store-descriptor)

;; Loaded protobufs tests
#+(or) (let* ((md-store-descriptor (metadata-store-descriptor)))
         (format t "md-store-descriptor: ~a~%" md-store-descriptor)
         (format t "md-store-descriptor imports: ~a~%" (cl-protobufs:proto-imports md-store-descriptor))
         md-store-descriptor
    )
