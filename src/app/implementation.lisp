(in-package #:ai-project-tools/app)

(defmethod start :before ((app run-loop-application) &rest args)
  (log:info "Starting app ~a" app))

(defmethod start :around ((app run-loop-application) &rest args)
  (with-app-lock-held (app app-run-lock)
    (when (running-p app)
      (error "Cannot start application ~a because it is already running." app))
   (call-next-method)))

(defmethod start ((app run-loop-application) &rest args)
  (let ((app-thread (bt:make-thread (lambda () (apply #'run app args))
                                    :name (format nil "~A-thread-run-loop" (name app)))))
    (setf (app-thread app) app-thread)))

(defun %shutdown-kernel (app)
  (log:info "Shutting down kernel for app ~a" app)
  (setf (running-p app) nil)
  (lp:end-kernel :wait t)
  (log:info "Kernel shut down. App: ~a is no longer running.lp:*kernel*: " app lp:*kernel*)
  (when (and (task-queue app) (> 0 (lpq:queue-count (task-queue app))))
    (log:warn "App task-queue count: ~a. NOTE: These tasks will NOT be run. Discarding..."
              (lpq:queue-count (task-queue app))))
  (when (kernel app) (setf (kernel app) nil))
  (when (task-queue app) (setf (task-queue app) nil)))

(defmethod stop :around ((app run-loop-application) &rest args)
  (with-app-lock-held (app app-run-lock)
    (if (not (running-p app))
        (warn "Cannot stop application ~a because it is not running." app)
        (call-next-method))))

(defmethod stop ((app run-loop-application) &rest args)
  (when (running-p app)
    (setf (running-p app) nil)))

(defmethod stop ((app base-lparallel-application) &rest args)
  (when (running-p app)
    (%shutdown-kernel app)))

(defun %kernel-run-task (task &key result-channel)
  (let ((result-channel (or result-channel (lp:make-channel))))
    (lp:submit-task result-channel
                    (lambda ()
                      (let ((result (funcall task)))
                        result)))
    (lp:receive-result result-channel)))

(defmethod run-task ((app base-lparallel-application) task &rest args &key result-channel)
  (unless (running-p app)
    (error "Cannot run task ~a in application ~a because it is not running." task app))
  (lpq:push-queue (if result-channel (cons task result-channel) task) (task-queue app)))

(defmethod process-task-queue :before ((app base-lparallel-application) &rest args)
  (unless (running-p app)
    (error "Cannot process task queue in application ~a because it is not running." app)))

(defmethod process-task-queue ((app base-lparallel-application) &rest args)
  (multiple-value-bind (new-task no-timeout-p) (lpq:try-pop-queue (task-queue app)
                                                                  :timeout (task-queue-timeout app))
    (if (not no-timeout-p)
        (if (eq new-task :stop)
            (progn
              (log:info "Got :STOP in task-queue. Stopping app ~a" app)
              (setf (running-p app) nil))
            (progn
              (log:info "Running task ~a" new-task)
              (etypecase new-task
                (cons (let ((task (car new-task))
                            (result-channel (cdr new-task)))
                        (%kernel-run-task task :result-channel result-channel)))
                (function (%kernel-run-task new-task))))))))

(defmethod should-continue-running-p ((app run-loop-application))
  (and (running-p app)
       (if (alx:negative-fixnum-p (max-iterations app))
           t
           (< (iteration-count app) (max-iterations app)))))

(defmethod run-loop-step :before ((app run-loop-application) &rest args)
  (incf (iteration-count app)))

(defmethod run-loop-step :around ((app base-lparallel-application) &rest args)
  (if (should-continue-running-p app)
      (call-next-method)
      (log:info "App ~a should no longer run. Shutting down..." app)))

(defmethod run-loop-step ((app base-lparallel-application) &rest args)
  (process-task-queue app))

(defmethod run :around ((app base-lparallel-application)
                                              &rest args
                                              &key
                                                (project (project app) project-supplied-p)
                                                (session nil session-supplied-p))
  (unwind-protect
       (let* ((*current-application* app)
              (*current-system-configuration* (system-configuration app))
              (*root-metadata-store* (metadata-store app))
              (*current-metadata-store* (metadata-store app))
              (*current-project* project)
              (*current-session* session))
         (setf (kernel app) (lp:make-kernel 8 :name (format nil "~a-kernel" (name app))
                                              :bindings `((*current-application* . ,*current-application*)
                                                          (*current-system-configuration* . ,*current-system-configuration*)
                                                          (*current-metadata-store* . ,*current-metadata-store*)
                                                          (*root-metadata-store* . ,*root-metadata-store*)
                                                          (*current-project* . ,*current-project*)
                                                          (*current-session* . ,*current-session*)))
               lp:*kernel* (kernel app)
               (task-queue app) (lpq:make-queue :fixed-capacity (task-queue-capacity app))
               (running-p app) t)
         (call-next-method))
    (progn (%shutdown-kernel app)
           (log:info "App ~a shut down. DONE." app))))

(defmethod run ((app run-loop-application) &rest args)
  (loop :while (should-continue-running-p app)
        :do (run-loop-step app)))
