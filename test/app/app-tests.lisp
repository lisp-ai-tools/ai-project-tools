(in-package #:ai-project-tools/app-tests)

(def-suite ai-project-tools/app-tests-suite
  :in ai-project-tools/tests:ai-project-tools/tests-suite)

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
    (is (null (app:task-queue app)))))
;; (run! 'run-state-1)


(defparameter *app-specials* (list 'core::*current-application*
                                   'core::*current-system-configuration*
                                   'core::*current-metadata-store*
                                   'core::*root-metadata-store*
                                   'core::*current-project*
                                   'core::*current-session*))

(defun symbol-bound-and-not-null (sym)
  (and (boundp sym) (not (null (symbol-value sym)))))

(defun do-app-specials (fn)
  (dolist (x *app-specials*)
    (funcall fn x)))

(defun map-app-specials (fn)
  (mapcar fn *app-specials*))

(defun check-app-specials ()
  (every #'symbol-bound-and-not-null *app-specials*))

(defun log-app-specials ()
  (loop for x in *app-specials*
        do (log:info "Symbol ~a bound to ~a" x (symbol-value x))))

(defun log-app-specials-to-string ()
  (with-output-to-string (s)
    (loop for x in *app-specials*
          do (format s "Symbol ~a bound to ~a ~%" x (symbol-value x)))
    s))

(test base-lparallel-app-bindings
  (let ((app (make-in-mem-app :start t :max-iterations 1)))
    (app:wait-for-app-start app)
    (let ((result-channel (lp:make-channel)))
      (app:wait-for-app-start app)
      (app:run-task app #'check-app-specials :result-channel result-channel)
      (is-true (lp:receive-result result-channel))
      ;; (app:run-task app #'log-app-specials-to-string :result-channel result-channel)
      ;; (let ((specials-str (lp:receive-result result-channel)))
      ;;   (log:info "Specials: ~a" specials-str))
      ))
    ;; outside of context of running app, those bindings should be unbound)
    (is-false (check-app-specials)))

;; (run! 'base-lparallel-app-bindings)
;; (check-app-specials)
;; (log-app-specials)
;; (log-app-specials-to-string)

;; (run! 'base-lparallel-app-bindings)
;; (ql:quickload '(:ai-project-tools :ai-project-tools/app-tests))
;; (run! 'ai-project-tools/app-tests-suite-exists)
;; (run! 'simple-creation-of-top-level-components-1)
;; (run! 'pre-start-state-1)
;; (run! 'base-lparallel-app-bindings)
;; (run! 'ai-project-tools/app-tests-suite)

;; Predicates: is is-every is-false is-true signals finishes

#+(or)
(let ((app (make-in-mem-app :start t :max-iterations 2)))
  (app:wait-for-app-start app)
  (let ((result-channel-1 (lp:make-channel))
        (result-channel-2 (lp:make-channel))
        (ret-1)
        (ret-2))
    (app:wait-for-app-start app)
    (app:run-task app #'check-app-specials :result-channel result-channel-1)
    (setf ret-1 (lp:receive-result result-channel-1))
    (app:run-task app #'log-app-specials-to-string :result-channel result-channel-2)
    (setf ret-2 (lp:receive-result result-channel-2))
    (values ret-1 ret-2)))
;; (core:metadata-store *in-mem-app*)
#+(or)
(progn
  (app:stop *in-mem-app*)
  (setf *in-mem-app* nil
        *in-mem-app-proj* nil
        *in-mem-app-session* nil))

;; (make-in-mem-app :start t :max-iterations -1)
;; (setf (app::running-p *in-mem-app*) nil)
;; (app:stop *in-mem-app*)
;; (app::process-task-queue *in-mem-app*)
;; (app::%shutdown-kernel *in-mem-app*)
;; (lp:end-kernel :wait t)
;; lp:*kernel*
;; (app:run-task *in-mem-app* #'log-app-specials-to-string)
;; (app:run-task *in-mem-app* #'check-app-specials)
#+(or) (let ((app (make-in-mem-app :start nil :max-iterations 1)))
         (app:start app)
         (app:wait-for-app-start app)
         (run-task *in-mem-app*
                   (lambda ()
                     (log:info "Sleepy task start...")
                     (sleep 1)
                     (log:info "Sleepy task end..."))))
