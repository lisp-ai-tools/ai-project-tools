(in-package #:ai-project-tools/app-tests)

(def-suite ai-project-tools/app-tests-suite :in ai-project-tools/tests-suite)

(in-suite ai-project-tools/app-tests-suite)

(test ai-project-tools/app-tests-suite-exists
  (is-true t))

(test base-lparallel-app-bindings
  (let ((cur-app-bound-p)
        (cur-sysconf-bound-p)
        (cur-md-store-bound-p)
        (root-md-store-bound-p)
        (cur-proj-bound-p)
        (cur-sess-bound-p)
        (in-mem-app (make-in-mem-app)))
    (flet ((check-app-specials ()
             (setf cur-app-bound-p (boundp 'core::*current-application*))))
      (unwind-protect
           (progn
             (run-task in-mem-app #'check-app-specials))
        (stop in-mem-app))
      (is (eq cur-app-bound-p t)))))

;; (ql:quickload '(:ai-project-tools :ai-project-tools/app-tests))
;; (run! 'ai-project-tools/app-tests-suite-exists)
;; (run! 'base-lparallel-app-bindings)
;; (run! 'ai-project-tools/app-tests-suite)

;; Predicates: is is-every is-false is-true signals finishes

;; (setf (app::running-p *in-mem-app*) nil)
;; (stop *in-mem-app*)

(defvar *app-bound-p* nil)

(let ((*app-bound-p* nil)
      (task (lambda ()
              (log:info "Running task with dynamic binding of *current-application*" core::*current-application*)
              (log:info "Is *current-application* bound? ~a" (boundp 'core::*current-application*))
              (setf *app-bound-p* (boundp 'core::*current-application*))
              (log:info "app-bound-p: ~a" *app-bound-p*))))
  (log:info "Running task...")
  (run-task *in-mem-app* task))
  (log:info "app-bound-p: ~a" *app-bound-p*)
  (format nil "app-bound-p: ~a~%" *app-bound-p*)
