(in-package #:ai-project-tools/app-tests)

(def-suite ai-project-tools/app-tests-suite :in ai-project-tools/tests:ai-project-tools/tests-suite)

(in-suite ai-project-tools/app-tests-suite)

(test ai-project-tools/app-tests-suite-exists
  (is-true t))

(test simple-creation-of-top-level-components-1
  (let ((app (make-in-mem-app)))
    (is-true (typep app 'application))
    (is-true (typep (project app) 'project))
    (is-true (typep (system-configuration app) 'system-configuration))
    (is-true (typep (metadata-store app) 'scoped-metadata-store))))

(test simple-creation-of-top-level-components-2
  (let ((app (make-in-mem-app)))
    (is (eql (get-current-system-configuration) (system-configuration app)))
    (is (eql (get-default-system-configuration) (system-configuration app)))))

;; (test base-lparallel-app-bindings
;;   (let ((cur-app-bound-p)
;;         (cur-sysconf-bound-p)
;;         (cur-md-store-bound-p)
;;         (root-md-store-bound-p)
;;         (cur-proj-bound-p)
;;         (cur-sess-bound-p)
;;         (in-mem-app (make-in-mem-app)))
;;     (flet ((check-app-specials ()
;;              (setf cur-app-bound-p (boundp 'core::*current-application*))))
;;       (unwind-protect
;;            (progn
;;              (run-task in-mem-app #'check-app-specials))
;;         (stop in-mem-app))
;;       (is (eq cur-app-bound-p t)))))

;; (ql:quickload '(:ai-project-tools :ai-project-tools/app-tests))
;; (run! 'ai-project-tools/app-tests-suite-exists)
;; (run! 'base-lparallel-app-bindings)
;; (run! 'ai-project-tools/app-tests-suite)

;; Predicates: is is-every is-false is-true signals finishes

;; (setf (app::running-p *in-mem-app*) nil)
;; (stop *in-mem-app*)

#+(or)(let ((app-bound-p nil)
            (task (lambda ()
                    (log:info "Running task with dynamic binding of *current-application*" core::*current-application*)
                    (log:info "Is *current-application* bound? ~a" (boundp 'core::*current-application*))
                    (setf app-bound-p (boundp 'core::*current-application*))))
            (result-channel (lp:make-channel)))
        (log:info "Running task...")
        (run-task *in-mem-app* task)
        (log:info "app-bound-p: ~a" app-bound-p)
        (let ((result (receive result-channel)))
          (log:info "result: ~a" result)
          (log:info "app-bound-p: ~a" app-bound-p)))
