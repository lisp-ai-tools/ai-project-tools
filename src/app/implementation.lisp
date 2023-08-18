(in-package #:ai-project-tools/app)

(defun %kernel-run-task-async (task)
  (lp:submit-task result-channel (lambda () (funcall task))))

(defun %kernel-run-task-sync (task)
  (let ((result-channel (lp:make-channel)))
    (lp:submit-task result-channel
                    (lambda ()
                      (let ((result (funcall task)))
                        result)))
    (lp:receive-result result-channel)))

(defmethod run-task ((app base-lparallel-application) task)
  (unless (running-p app)
    (error "Cannot run task ~a in application ~a because it is not running." task app))
  (lpq:push-queue task (task-queue app)))

(defmethod start ((app base-lparallel-application) &rest args)
  (if (running-p app)
    (warn "Cannot start application ~a because it is already running. Ignoring..." app)
    (apply #'run app args)))

(defmethod stop ((app base-lparallel-application) &rest args)
  (if (running-p app)
    (lpq:push-queue :stop (task-queue app))
    (warn "Cannot stop application ~a because it is not running. Ignoring..." app)))

(defun %shutdown-app (app)
  (lp:end-kernel :wait t)
  (setf (running-p app) nil)
  (log:info "Kernel shut down. App ~a is no longer running." app)
  (log:info "lp:*kernel* = ~a" lp:*kernel*)
  (when (and (task-queue app) (> 0 (lpq:queue-count (task-queue app))))
    (log:warn "App task-queue count: ~a. NOTE: These tasks will NOT be run. Discarding..."
              (lpq:queue-count (task-queue app))))
  (when (kernel app) (setf (kernel app) nil))
  (when (task-queue app) (setf (task-queue app) nil)))

(defmethod run-loop-step ((app base-lparallel-application) &rest args)
  (multiple-value-bind (new-task no-timeout-p) (lpq:try-pop-queue (task-queue app) :timeout 5)
    (incf *iterations*)
    (if (not no-timeout-p)
        (progn
          (incf *queue-timeouts*)
        (log:info "App task queue timeout. *Iterations*: ~a. Queue timeouts: ~a" *iterations* *queue-timeouts*))
        (if (eq new-task :stop)
            (progn
              (log:info "Got :STOP in task-queue. Stopping app ~a" app)
              (setf (running-p app) nil)
              (return-from run-loop-step))
            (progn
              (log:info "Running task ~a" new-task)
              (%kernel-run-task-sync new-task))))))

(defmethod run :around ((app base-lparallel-application)
                                              &rest args
                                              &key
                                                (project (project app) project-supplied-p)
                                                (session nil session-supplied-p))
  (when (running-p app)
    (error "Cannot run application ~a because it is already running!" app))

  (unwind-protect
       (let* ((*current-application* app)
              (*current-system-configuration* (system-configuration app))
              (*root-metadata-store* (metadata-store app))
              (*current-metadata-store* (metadata-store app))
              (*current-project* project)
              (*current-session* session)
              (*iterations* 0)
              (*queue-timeouts* 0))
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
    (progn (%shutdown-app app)
           (log:info "App ~a shut down. DONE." app))))

(defmethod run ((app runnable-application)
                &rest args
                &key
                  (project (project app) project-supplied-p)
                  (session nil session-supplied-p))
  (loop :while (running-p app)
        :do (run-loop-step app)))
