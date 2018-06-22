(in-package :jsonq)

(defpackage :jsonq-reader
  (:export #:true
           #:false
           #:null))

(defvar jsonq-reader:true t)
(defvar jsonq-reader:false nil)
(defvar jsonq-reader:null nil)

(define-condition json-reader-error (error)
  ((stearm :initarg :stream)))

(define-condition json-invalid-token-error (json-reader-error)
  ((token :initarg :token))
  (:report (lambda (condition stream)
             (format stream "Invalid token ~a!"
                     (slot-value condition 'token)))))

(defgeneric to-lisp (x)
  (:method (x)
    x)
  (:method ((x (eql 'jsonq-reader:true)))
    jsonq-reader:true)
  (:method ((x (eql 'jsonq-reader:false)))
    jsonq-reader:false)
  (:method ((x (eql 'jsonq-reader:null)))
    jsonq-reader:null))

(defun read-obj (stream char)
  (declare (ignore char))
  (let ((list (read-delimited-list #\} stream t)))
    (loop for x in list
          if (and (symbolp x)
                  (not (eq x 'jsonq-reader:true))
                  (not (eq x 'jsonq-reader:false))
                  (not (eq x 'jsonq-reader:null)))
            do (do-symbols (symbol :jsonq-reader)
                 (when (and (not (eq symbol 'jsonq-reader:true))
                            (not (eq symbol 'jsonq-reader:false))
                            (not (eq symbol 'jsonq-reader:null)))
                   (unintern symbol :jsonq-reader)))
               (error 'json-invalid-token-error :token x :stream stream))
    (apply #'obj* (loop for (key value . rest) on list by #'cddr
                        collect (cons key (to-lisp value))))))

(defun read-arr (stream char)
  (declare (ignore char))
  (arr (mapcar #'to-lisp (read-delimited-list #\] stream t))))

(defun read-colon (stream char)
  (declare (ignore char))
  (read stream t nil t))

(defun read-comma (stream char)
  (declare (ignore char))
  (read stream t nil t))

(defun read-json (stream)
  (let ((*readtable* (copy-readtable nil))
        (*package* (find-package :jsonq-reader))
        (*read-eval* nil))
    (set-macro-character #\{ #'read-obj)
    (set-macro-character #\} (get-macro-character #\) nil))
    (set-macro-character #\[ #'read-arr)
    (set-macro-character #\] (get-macro-character #\) nil))
    (set-macro-character #\: #'read-colon)
    (set-macro-character #\, #'read-comma)
    (read stream)))

(defun read-json-from-string (string)
  (with-input-from-string (stream string)
    (read-json stream)))
