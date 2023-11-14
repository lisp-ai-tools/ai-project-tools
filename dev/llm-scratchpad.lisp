(in-package #:ai-project-tools/core)

;; (ql:quickload '(:schemata))
;;XXX these should go in a common api package

(define-condition missing-credentials-error (error)
  ((%reason :reader reason :initarg :reason))
   (:report (lambda (c stream) (format stream (reason c)))))

(defun %get-api-key-from-environment (env-key)
  (uiop:getenvp env-key))

(defun %get-api-key-from-path (api-key-path &optional (parser-fn nil))
  (when (uiop:file-exists-p api-key-path)
    (let ((file-string (uiop:read-file-string api-key-path)))
      (if parser-fn
          (funcall parser-fn file-string)
          file-string))))

(defun %get-api-key-or-lose (env-key &optional path)
  (if-let ((key (or (%get-api-key-from-environment env-key)
                    (%get-api-key-from-path path))))
    key
    (error 'missing-credentials-error
           :reason (format nil "No api-key with ENV_VAR: ~a, or FILE_PATH: ~a found" env-key path))))

(defclass required-auth-key-mixin () ())

(defclass required-auth-key-from-env (required-auth-key-mixin)
  ((env-var :initarg :env-var :accessor env-var :initform nil))
  (:documentation "A required auth key that is read from an environment variable"))

(defclass required-auth-key-from-file (required-auth-key-mixin)
  ((file-path :initarg :file-path :accessor file-path :initform nil))
  (:documentation "A required auth key that is read from a file-path"))

(defgeneric required-auth-key-p (self)
  (:method ((self t)) nil)
  (:method ((self required-auth-key-mixin)) t))

(defgeneric get-required-auth-key (self)
  (:method ((self t)) nil)
  (:method ((self required-auth-key-from-env)) (%get-api-key-or-lose (env-var self) nil))
  (:method ((self required-auth-key-from-file)) (%get-api-key-or-lose nil (file-path self))))

#+(or)(let ((auth (make-instance 'required-auth-key-from-env :env-var "OPENAI_API_KEY")))
       (values (required-auth-key-p auth) (get-required-auth-key auth)))
;;(uiop:getenvp "OPENAI_API_KEY")

(defclass has-aliases ()
  ((aliases :initarg :aliases :accessor aliases :initform nil))
  (:documentation "A class that has aliases in addition to a name"))

(defclass llm (has-name has-description)
  ()
  (:documentation "Large Language model that can be used to generate text."))

(defclass remote-llm (llm)
  ((%client :initarg :client :accessor client :initform nil))
  (:documentation "A remote llm is a language model that is backed by a remote service that is accessed by a client."))

(defclass remote-llm-with-auth-from-env (remote-llm required-auth-key-from-env)
  ()
  (:documentation "A remote llm that requires an auth key that is read from an environment variable"))
#+(or)(let ((llm (make-instance 'remote-llm-with-auth-from-env :name "OpenAI Chat" :description "Some LLM" :env-var "OPENAI_API_KEY")))
            (values (required-auth-key-p llm) (get-required-auth-key llm)))

(defclass base-openai-completion-configuration ()
  ((model
    :initarg :model
    :accessor model
    :type (or string symbol)
    :documentation "ID of the model to use.")
  (prompt
    :initarg :prompt
    :accessor prompt
    :type (or null string list)
    :initform "<|endoftext|>"
    :documentation "The prompt(s) to generate completions for.")
   (max-tokens
    :initarg :max-tokens
    :accessor max-tokens
    :type (or null integer)
    :initform 16
    :documentation "The maximum number of tokens to generate in the completion.")
   (temperature
    :initarg :temperature
    :accessor temperature
    :type (or null number)
    :initform 1
    :documentation "What sampling temperature to use between 0 and 2.")
   (top-p
    :initarg :top-p
    :accessor top-p
    :type (or null number)
    :initform 1
    :documentation "An alternative to sampling with temperature, called nucleus sampling.")
   (n
    :initarg :n
    :accessor n
    :type (or null integer)
    :initform 1
    :documentation "How many completions to generate for each prompt.")
   (stream-response
    :initarg :stream-response
    :accessor stream-response
    :type (or null boolean)
    :initform nil
    :documentation "Whether to stream back partial progress.")
   (loggprobabilities
    :initarg :logprobs
    :accessor logprobs
    :type (or null integer)
    :initform nil
    :documentation "Include the log probabilities on the logprobs most likely tokens.")
   (stop
    :initarg :stop
    :accessor stop
    :type (or null string sequence)
    :initform nil
    :documentation "Up to 4 sequences where the API will stop generating further tokens.")
   (presence-penalty
    :initarg :presence-penalty
    :accessor presence-penalty
    :type (or null number)
    :initform 0
    :documentation "Number between -2.0 and 2.0.")
   (frequency-penalty
    :initarg :frequency-penalty
    :accessor frequency-penalty
    :type (or null number)
    :initform 0
    :documentation "Number between -2.0 and 2.0.")
   (best-of
    :initarg :best-of
    :accessor best-of
    :type (or null integer)
    :initform 1
    :documentation "Generates `best-of' completions server-side.")
   (logit-bias
    :initarg :logit-bias
    :accessor logit-bias
    :type (or null list)
    :initform nil
    :documentation "Modify the likelihood of specified tokens appearing in the completion.")
   (user
    :initarg :user
    :accessor user
    :type (or null string)
    :initform nil
    :documentation "A unique identifier representing your end-user.")))

(defclass openai-completion-configuration (base-openai-completion-configuration)
  ((suffix
    :initarg :suffix
    :accessor suffix
    :type (or null string)
    :initform nil
    :documentation "The suffix that comes after a completion of inserted text.")
   (echo
    :initarg :echo
    :accessor echo
    :type (or null boolean)
    :initform nil
    :documentation "Echo back the prompt in addition to the completion.")
   (best-of
    :initarg :best-of
    :accessor best-of
    :type (or null integer)
    :initform 1
    :documentation "Generates `best-of' completions server-side.")))

(defclass openai-chat-completion-configuration (base-openai-completion-configuration)
 ((context
   :initarg :context
   :accessor context
   :type (or null string list)
   :initform nil
   :documentation "List should be composed of alternating pairs of prompt and its associated
completion, for context.")
  (system-instruction
   :initarg :system-instruction
   :accessor system-instruction
   :type (or null string)
   :initform nil
   :documentation "Should be an instruction to chatgpt of what role it should assume.")))

(defclass openai-llm (remote-llm openai-completion-configuration belongs-to-session)
  ((service-point :initform "completions" :accessor service-point)))

(defclass openai-chat-llm (remote-llm required-auth-key-from-env openai-chat-completion-configuration belongs-to-session)
  ((service-point :initform "chat/completions" :accessor service-point)))

;;(c2mop:finalize-inheritance (find-class 'openai-chat-completion-configuration))
#+(or)(defparameter *openai-chat-completion-schema*
        (schemata:generate-schema-from-class (find-class 'openai-chat-completion-configuration)))

#+(or)(let ((oai-chat-llm (make-instance 'openai-chat-llm :name "OpenAI Chat" :description "Some LLM" :env-var "OPENAI_API_KEY")))
            (schemata::schema-validate *openai-chat-completion-schema* oai-chat-llm))
