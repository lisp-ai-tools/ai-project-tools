(in-package #:ai-project-tools/core)

(defvar *ai-project-tools-version* "0.0.1")

(defvar *current-application* nil)
(defvar *current-system-configuration* nil)
(defvar *root-metadata-store* nil)
(defvar *current-metadata-store* nil)
(defvar *current-project* nil)
(defvar *current-session* nil)

(defparameter default-system-configuration nil)
(defvar system-configuration-registry (make-hash-table :test 'equal))

(declaim (special
          *current-application*
          *current-system-configuration*
          *root-metadata-store*
          *current-metadata-store*
          *current-project*
          *current-session*))
