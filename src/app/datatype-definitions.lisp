(in-package #:ai-project-tools/app)


(defclass runnable-application (application runnable) ())

(defclass base-lparallel-application (runnable-application)
  ((%kernel
    :initarg :kernel
    :initform nil
    :accessor kernel
    ;; :type lp:kernel
    :documentation "The lparallel kernel for the application.")
   (%task-queue
    :initform nil
    :accessor task-queue
    ;; :type lp:queue
    :documentation "The lparallel task queue for the application.")
   (%task-queue-capacity
    :initform 8
    :accessor task-queue-capacity
    :type integer
    :documentation "The capacity of the task queue.")
   (%running-p
    :initform nil
    :accessor running-p
    :type boolean
    :documentation "Whether the application is running or not."))
  (:documentation "An application that uses lparallel to run tasks in the run/run-loop-step methods"))
