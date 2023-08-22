(in-package #:ai-project-tools/app-tests)

(def-suite ai-project-tools/app-tests-suite :in ai-project-tools/tests:ai-project-tools/tests-suite)

(in-suite ai-project-tools/app-tests-suite)

(test ai-project-tools/app-tests-suite-exists
  (is-true t))

(test simple-creation-of-top-level-components-1
  (let ((app (make-in-mem-app)))
    (is-true (typep app 'application))
    (is-true (typep (core:project app) 'core:project))
    (is-true (typep (core:system-configuration app) 'core:system-configuration))
    (is-true (typep (core:metadata-store app) 'core:scoped-metadata-store))
    (is (eql (core:get-current-system-configuration) (core:system-configuration app)))
    (is (eql (core:get-default-system-configuration) (core:system-configuration app)))))

(test pre-start-state-1
  (let ((app (make-in-mem-app :start nil)))
    (is (not (app:running-p app)))
    (is (= (app:iteration-count app) 0))
    (is (null (app:app-thread app)))))
;; (run! 'pre-start-state-1)

(test run-state-1
  (let ((app (make-in-mem-app :start nil :max-iterations 1)))
    (is (not (app:running-p app)))
    (is (= (app:iteration-count app) 0))
    (is (null (app:kernel app)))
    (is (null (app:task-queue app)))
    (app:start app)
    (app:wait-for-app-start app)
    (app:run-task app (lambda () (sleep 0.3)))
    (is (app:running-p app))
    (is (not (null (app:app-thread app))))
    (is (not (null (app:task-queue app))))
    (is (not (null (app:kernel app))))
    (app:wait-for-app-stop app)
    (is (not (app:running-p app)))
    (is (= (app:iteration-count app) 1))
    (is (null (app:app-thread app)))
    (is (null (app:kernel app)))
    (is (null (app:task-queue app)))
    ))
;; (run! 'run-state-1)


(test base-lparallel-app-bindings
  (let ((app (make-in-mem-app :start t :max-iterations 1)))
    (flet ((check-app-specials ()
             (every (lambda (x) (boundp x))
                    (list 'core::*current-application*
                          'core::*current-system-configuration*
                          'core::*current-metadata-store*
                          'core::*root-metadata-store*
                          'core::*current-project*
                          'core::*current-session*))))
      (app:wait-for-app-start app)
      (let ((result-channel (lp:make-channel)))
        (app:wait-for-app-start app)
        (app:run-task app #'check-app-specials :result-channel result-channel)
        (is (lp:receive-result result-channel))))))

;; (run! 'base-lparallel-app-bindings)
;; (ql:quickload '(:ai-project-tools :ai-project-tools/app-tests))
;; (run! 'ai-project-tools/app-tests-suite-exists)
;; (run! 'simple-creation-of-top-level-components-1)
;; (run! 'pre-start-state-1)
;; (run! 'base-lparallel-app-bindings)
;; (run! 'ai-project-tools/app-tests-suite)

;; Predicates: is is-every is-false is-true signals finishes

;; (setf (app::running-p *in-mem-app*) nil)
;; (stop *in-mem-app*)
#+(or) (let ((app (make-in-mem-app :start nil :max-iterations 1)))
         (app:start app)
         (app:wait-for-app-start app)
         (run-task *in-mem-app*
                   (lambda ()
                     (log:info "Sleepy task start...")
                     (sleep 1)
                     (log:info "Sleepy task end..."))))

