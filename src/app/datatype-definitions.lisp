(in-package #:ai-project-tools/app)

(defclass run-loop-application (application runnable)
  ((%app-lock
    :initform (bt:make-recursive-lock "app-lock")
    :reader app-lock
    :type bt:lock
    :documentation
    "The lock for the application. Needed to ensure that only one thread runs the
lifecycle methods at a time.")
   (%app-thread
    :initform nil
    :accessor app-thread
    :type (or null bt:thread)
    :documentation "The thread the application is running in.")
   (%max-iterations
    :initarg :max-iterations
    :initform -1
    :accessor max-iterations
    :type integer
    :documentation
    "The maximum number of iterations to run the application for. Use negative values for no limit.")
   (%iteration-count
    :initform 0
    :accessor iteration-count
    :type integer
    :documentation "The number of iterations (run-loop-step) the application has run for.")
   (%running-p
    :initform nil
    :accessor running-p
    :type boolean
    :documentation "Whether the application is running or not."))
  (:documentation "An application that runs run-loop-step method in the run method"))

(defclass base-lparallel-application (run-loop-application)
  ((%kernel
    :initarg :kernel
    :initform nil
    :accessor kernel
    :type (or null lp:kernel)
    :documentation "The lparallel kernel for the application.")
   (%task-queue
    :initform nil
    :accessor task-queue
    :type (or null lpq:queue)
    :documentation "The lparallel task queue for the application.")
   (%task-queue-capacity
    :initarg :task-queue-capacity
    :initform 8
    :accessor task-queue-capacity
    :type integer
    :documentation "The capacity of the task queue before a push-queue call blocks.")
   (%task-queue-timeout
    :initarg :task-queue-timeout
    :initform 0.1
    :accessor task-queue-timeout
    :type number
    :documentation
    "The timeout for the task queue in seconds. Use negative values for no timeout.
Use float values for fractional seconds."))
  (:documentation "An application that uses lparallel to run tasks in the run/run-loop-step methods"))
