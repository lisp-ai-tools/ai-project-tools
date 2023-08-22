(in-package #:ai-project-tools/app)

;;;; Pulled from Pavel Korolev's (borodust) cl-muth project. MIT license.
(defclass simple-latch ()
  ((counter :initarg :count :initform 1)
   (state-changed :initform (bt:make-condition-variable :name "latch-state"))
   (lock :initform (bt:make-recursive-lock "latch-lock"))))


(declaim (inline make-latch))
(defun make-latch (&optional (count 1))
  (make-instance 'simple-latch :count count))


(declaim (ftype (function (simple-latch) *) wait-for-latch))
(defun wait-for-latch (latch)
  (with-slots (state-changed lock counter) latch
    (bt:with-recursive-lock-held (lock)
      (loop while (> counter 0) do
	   (bt:condition-wait state-changed lock)))))


(declaim (ftype (function (simple-latch) *) open-latch))
(defun open-latch (latch)
  (with-slots (state-changed lock counter) latch
    (bt:with-recursive-lock-held (lock)
      (when (> counter 0)
	(decf counter)
	(bt:condition-notify state-changed)))))


(defmacro wait-with-latch ((latch-name &optional (count 1)) &body body)
  `(let ((,latch-name (make-latch ,count)))
     (prog1 (progn ,@body)
       (wait-for-latch ,latch-name))))
