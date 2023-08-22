(in-package #:ai-project-tools/app)

(defmacro with-app-lock-held ((app lock-var) &body body)
  "Hold app lock for the duration of the `body`."
  (alx:once-only (app)
    `(let ((,lock-var (app-lock ,app)))
       (bt:with-recursive-lock-held ((app-lock ,app))
         ,@body))))

(defun wait-for-app-start (app)
  (unless (running-p app)
    (wait-for-latch (app-started-latch app))))

(defun wait-for-app-stop (app)
  (when (running-p app)
    (wait-for-latch (app-stopped-latch app))))

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
