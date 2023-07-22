(in-package :ai-project-tools/core)

(defun vector-to-hex-string (vec)
  "Converts a vector of bytes to a string of lowercase hex digits.
Example:
(defparameter *my-vector* #(#x4A #x64 #xE9 #xE5 #xCE #xBF #x4A #xBC #x1D #x2B #x26 #x48 #xC4 #xE8 #xDD #x74))
(vector-to-hex-string *my-vector*) => \"4a64e9e5cebf4abc1d2b2648c4e8dd74\""
  (with-output-to-string (s)
    (dotimes (i (length vec))
      (format s "~(~2,'0x~)" (aref vec i)))))

(defun file-md5-sum (filename)
  (vector-to-hex-string (ironclad:digest-file 'ironclad:md5 filename)))
;; (file-md5-sum "typedefs.lisp")

(defun collect-keyword-args (args)
  (loop for (key val) on args by #'cddr
        when (keywordp key)
          collect (cons key val)))

(defun extract-args (keys plist)
  (let ((result '()))
    (loop for key in keys
          for val = (getf plist key)
          do (setf result (append result (list key val))))
    result))
;; (%extract-args '(:a :g) '(:a 1 :b 2 :d 4 :g 5))

(defmethod clos-class-initargs (class)
  (let ((direct-slots (closer-mop:class-direct-slots class)))
    (loop for slot in direct-slots
          append (closer-mop:slot-definition-initargs slot))))
;; (clos-class-initargs (find-class 'simple-prompt-node))

(defmethod clos-class-initargs-all (class)
  (let* ((precedence-list (closer-mop:class-precedence-list class))
         (slots (mapcan #'closer-mop:class-direct-slots precedence-list)))
    (loop for slot in slots
          append (closer-mop:slot-definition-initargs slot))))
;; (clos-class-initargs-all (find-class 'simple-prompt-node))
