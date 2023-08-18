(in-package #:ai-project-tools/app)

(defgeneric destroy (app)
  (:documentation "Destroy the application."))

(defgeneric start (app &rest args))
(defgeneric stop (app &rest args))

(defgeneric run-loop-step (app &rest args))

(defgeneric run-task (app task)
  (:documentation "Run a task in the application."))
