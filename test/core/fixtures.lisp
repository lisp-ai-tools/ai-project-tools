(in-package #:ai-project-tools/core-tests)

(defvar *test-config* nil)
(defvar *test-root-store* nil)
(defvar *test-project* nil)
(defvar *test-projects-store* nil)
(defvar *test-project-store* nil)
(defvar *test-session* nil)
(defvar *test-sessions-store* nil)
(defvar *test-session-store* nil)

(defun clear-vars ()
  (setf *test-config* nil
        *test-root-store* nil
        *test-project* nil
        *test-session* nil
        *test-projects-store* nil
        *test-project-store* nil
        *test-sessions-store* nil
        *test-session-store* nil))

;; (hash-table-count (store *test-root-store*))

(defun make-scoped-project-sessions (&key (clear-vars t))
  (when clear-vars (clear-vars))

  (let* ((config (make-instance 'core::simple-in-memory-system-configuration
                                :designator :ephemeral-chat-config
                                :name "Simple in-memory system configuration."
                                :description "A simple in-memory system configuration that does not persist."))
         (store (make-instance 'core::memory-scoped-metadata-store
                               :name "root"
                               :parent nil
                               :schema (list :fake :schema)
                               :store (make-hash-table :test #'equal)))
         (projects-store (make-instance 'core::memory-scoped-metadata-store
                                        :name "projects"
                                        :parent store
                                        :schema (list :fake :schema)))
         (project-store (make-instance 'core::memory-scoped-metadata-store
                                       :name "ephemeral-chat-project"
                                       :parent projects-store
                                       :schema (list :fake :schema)))
         (project (make-instance 'core:project
                                 :name "ephemeral-chat-project"
                                 :description "Project for a quick-n-dirty chat app for testing in the REPL."
                                 :metadata-store project-store))
         (sessions-store (make-instance 'core::memory-scoped-metadata-store
                                        :name "sessions"
                                        :parent project-store
                                        :schema (list :fake :schema)))
         (session-store (make-instance 'core::memory-scoped-metadata-store
                                       :name "ephemeral-chat-session"
                                       :parent sessions-store
                                       :schema (list :fake :schema)))
         (session (make-instance 'core:session
                                 :name "ephemeral-chat-session"
                                 :description "Session for a quick-n-dirty chat app for testing in the REPL."
                                 :project project
                                 :metadata-store session-store)))
    (setf *test-config* config
          *test-root-store* store
          *test-project* project
          *test-session* session
          *test-projects-store* projects-store
          *test-project-store* project-store
          *test-sessions-store* sessions-store
          *test-session-store* session-store)
    (values config store project session
            projects-store project-store
            sessions-store session-store)))

(defvar *test-scoped-hash-table* nil)

(defun setup-scoped-hash-table ()
  (let ((entries '(("/a" . 1)
                   ("/a/b" . 2)
                   ("/a/c" . 3)
                   ("/a/b/d" . 4)
                   ("/a/c/e" . 5)
                   ("/a/b/f" . 6)
                   ("/a/c/g" . 7)
                   ("/a/b/d/h" . 8)
                   ("/a/c/e/i" . 9)
                   ("/a/b/f/j" . 10)
                   ("/a/c/g/k" . 11))))
    (setf *test-scoped-hash-table* (alexandria:alist-hash-table entries))
    *test-scoped-hash-table*))
;; (setup-scoped-hash-table)

;; (ql:quickload :split-sequence)
;; Get direct children of a store
(defun %path-segs (key &optional (separator-string "/"))
  (split-sequence:split-sequence
   separator-string
   (string-left-trim separator-string key) :test #'string=))
;; (string-left-trim "/" "/a/b/c")
;; (%path-segs "/a/b/c")
;; => ("a" "b" "c")
;; (butlast (%path-segs "/a/b/c"))
;; (butlast (%path-segs "/a"))
;; (every #'string= '("a" "b" "c") (%path-segs "/a/b/c"))

(defun direct-child-path-p (prefix key)
  (let ((prefix-segs (%path-segs prefix))
        (key-segs (%path-segs key)))
    (and (= (1+ (length prefix-segs)) (length key-segs))
         (every #'string= prefix-segs (butlast key-segs)))))

(defun get-direct-children (table prefix)
  (let ((prefix-children nil))
    (maphash (lambda (key value)
               (when (direct-child-path-p prefix key)
                 (push (cons key value) prefix-children)))
             table)
    prefix-children))
;; (get-direct-children *test-scoped-hash-table* "/a/b")

;; Get direct children of a store that match a predicate
(defun get-direct-children-matching (table prefix pred-fn)
  (let ((prefix-children nil))
    (maphash (lambda (key value)
               (when (and (direct-child-path-p prefix key)
                          (funcall pred-fn value))
                 (push (cons key value) prefix-children)))
             table)
    prefix-children))

;; Get all descendants of a prefix
(defun get-subtree (table prefix)
  (let ((descendants nil))
    (maphash (lambda (key value)
               (when (search prefix key)
                 (push (cons key value) descendants)))
             table)
    descendants))
;; (get-subtree *test-scoped-hash-table* "/a/b")

;; Get all descendants of a prefix that match a predicate
(defun get-subtree-matching (table prefix pred-fn)
  (let ((descendants nil))
    (maphash (lambda (key value)
               (when (and (search prefix key)
                          (funcall pred-fn value))
                 (push (cons key value) descendants)))
             table)
    descendants))
;; (get-subtree-matching *test-scoped-hash-table* "/a/b" (lambda (value) (< value 6)))

;; Clear direct children of a store, leaving siblings intact
(defun discard-direct-children (table prefix)
  (maphash (lambda (key value)
             (when (direct-child-path-p prefix key)
               (remhash key table)))
           table))
;; (discard-direct-children *test-scoped-hash-table* "/a/b")

;; Clear all descendants of a prefix
(defun discard-subtree (table prefix)
  (maphash (lambda (key value)
             (when (search prefix key)
               (remhash key table)))
           table))
;; (discard-subtree *test-scoped-hash-table* "/a/b")
