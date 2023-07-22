(in-package #:ai-project-tools/core)

(defvar *ai-project-tools-version* "0.0.1")

(defvar *current-system-configuration* nil)
(defvar *root-metadata-store* nil)
(defvar *current-metadata-store* nil)
(defvar *current-project* nil)
(defvar *current-session* nil)

(defparameter default-system-configuration nil)
(defvar system-configuration-registry (make-hash-table :test 'equal))
;;(setf system-configuration-registry (make-hash-table :test 'equal))

(declaim (special *current-system-configuration*
                  *current-metadata-store*
                  *current-project*
                  *current-session*))
