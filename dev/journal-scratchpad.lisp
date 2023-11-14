(in-package :cl-user)

(defpackage :journal-scratchpad
  (:use :cl :journal))

(in-package :journal-scratchpad)

(defclass user ()
  ((id :initarg :id :reader user-id)
   (message :initarg :message :reader user-message)))

(defmethod print-object ((user user) stream)
  (print-unreadable-object (user stream :type t :identity t)
    (format stream "[~S: ~A]" (slot-value user 'id) (slot-value user 'message))))

(defvar *users* (make-hash-table))
;;(setf *users* (make-hash-table))

(defun find-user (id)
  (gethash id *users*))

(defun add-user (id &key (message (format nil "User ~S" id) message-p))
  (setf (gethash id *users*) (make-instance 'user :id id :message message)))

(defparameter *user7* (add-user 7 :message "==User 7=="))
;;(setf *user7* nil)
;;(setf *user7* (add-user 7 :message "==User 7=="))
;;(setf (slot-value *user7* 'message) "Hello, world! I changed the message!")

(defun get-message ()
  (replayed (user-recording
             :values (values-> #'user-id)
             :replay-values (values<- #'find-user))
    (values *user7* (user-message *user7*))))

(defun run-user-journal-example ()
  (jtrace user-id find-user get-message)

  (let ((bundle (make-file-bundle "/tmp/user-example/")))
    (format t "Recording")
    (with-bundle (bundle)
      (get-message))
    (format t "~%Replaying")
    (with-bundle (bundle)
      (get-message))))
;; (run-user-journal-example)

#+(or)(defun get-message ()
        (let ((#:log-record550 :record) (#:version551 :infinity))
          (let ((#:%log-record552
                  (and (null #:version551) #:log-record550
                       (journal::resolve-log-record #:log-record550))))
            (flet ((#:user-recording-journaled-block553 ()
                     (values *user7* (user-message *user7*))))
              (declare (dynamic-extent (function #:user-recording-journaled-block553))
                       (inline #:user-recording-journaled-block553))
              (if (or journal::*record-streamlet* journal::*replay-streamlet*
                      #:%log-record552)
                  (journal::call-journaled #'#:user-recording-journaled-block553
                                           journal::*record-streamlet* #:%log-record552
                                           'user-recording #:version551 nil
                                           (values-> #'user-id) nil
                                           journal::*replay-streamlet* nil
                                           (values<- #'find-user) nil
                                           journal::*replay-eoj-error-p*)
                  (#:user-recording-journaled-block553))))))
