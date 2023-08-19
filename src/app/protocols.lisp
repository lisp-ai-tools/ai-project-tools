(in-package #:ai-project-tools/app)

(defgeneric destroy (app)
  (:documentation "Destroy the application."))

(defgeneric start (app &rest args))
(defgeneric stop (app &rest args))

(defgeneric should-continue-running-p (app))
(defgeneric run-loop-step (app &rest args))

(defgeneric run-task (app task &rest args)
  (:documentation "Run a task in the application."))

(defgeneric process-task-queue (app &rest args)
  (:documentation "Process the task queue."))
