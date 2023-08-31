

(defgeneric %push-execution-node (app execution-node)
  (:documentation "Pushes an execution node onto the execution node stack."))
(defgeneric %pop-execution-node (app)
  (:documentation "Pops an execution node off the execution node stack."))
(defmacro with-execution-node (app execution-node &body body)
  "Pushes an execution node onto the execution node stack, executes the body,
and then pops the execution node off the execution node stack."
  `(progn
     (%push-execution-node ,app ,execution-node)
     (unwind-protect
          (progn ,@body)
       (%pop-execution-node ,app))))
