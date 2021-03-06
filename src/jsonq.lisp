(in-package :jsonq)

(defun camel-case (string)
  (with-output-to-string (out)
    (loop for c across string
          with separator = nil
          do (cond ((char= #\- c)
                    (setf separator t)())
                   (separator
                    (write-char (char-upcase c) out)
                    (setf separator nil))
                   (t
                    (write-char (char-downcase c) out))))))

(defstruct (obj (:print-object print-obj)) values)
(defstruct *obj obj)
(defstruct (arr (:print-object print-arr)) values)

(defun print-obj (obj stream)
  (write-char #\{ stream)
  (loop with sep = ""
        for (key . value) in (obj-values obj)
        do (write-string sep stream)
           (setf sep ", ")
           (print-key key stream)
           (write-string ": " stream)
           (print-value value stream))
  (write-char #\} stream))

(defun print-arr (arr stream)
  (write-char #\[ stream)
  (loop with sep = ""
        for value in (arr-values arr)
        do (write-string sep stream)
           (setf sep ", ")
           (print-value value stream))
  (write-char #\] stream))

(defun symbol-to-json-key (symbol)
  (camel-case (symbol-name symbol)))

(defgeneric print-key (key stream)
  (:method (key stream)
    (format stream "~s" key))
  (:method ((key symbol) stream)
    (print-key (symbol-to-json-key key) stream)))

(defgeneric print-value (value stream)
  (:method (value stream)
    (format stream "~s" value))
  (:method ((value null) stream)
    (write-string "null" stream))
  (:method ((value (eql t)) stream)
    (write-string "true" stream))
  (:method ((value string) stream)
    (write-char #\" stream)
    (loop for c across value
          do (case c
               (#\" (write-string "\\\"" stream))
               (#\cr (write-string "\\r" stream))
               (#\lf (write-string "\\n" stream))
               (#\\ (write-string "\\\\" stream))
               (#\tab (write-string "\\t" stream))
               (t (write-char c stream))))
    (write-char #\" stream))
  (:method ((value ratio) stream)
    (print-value (coerce value 'double-float) stream))
  (:method ((value double-float) stream)
    (format stream "~f" value)))

(defmacro obj (&rest args)
  `(make-obj :values (normalize-obj-values ,@(%obj args))))

(defun obj* (&rest args)
  (make-obj :values (apply #'normalize-obj-values args)))

(defun %obj (args)
  (if (endp args)
      nil
      (let ((car (car args))
            (cadr (cadr args)))
        (cond ((and (symbolp car) (string-equal car "*"))
               (cons `(make-*obj :obj ,cadr)
                     (%obj (cddr args))))
              ((keywordp car)
               (cons `(cons ,(symbol-to-json-key car) ,cadr)
                     (%obj (cddr args))))
              ((stringp car)
               (cons `(cons ,car ,cadr)
                     (%obj (cddr args))))
              ((symbolp car)
               (cons `(cons ,(symbol-to-json-key car) ,car)
                     (%obj (cdr args))))
              (t
               nil)))))

(defun normalize-obj-values (&rest args)
  (let ((values nil))
    (labels ((f (args)
               (when args
                 (let ((car (car args)))
                   (if (*obj-p car)
                       (f (obj-values (*obj-obj car)))
                       (let* ((key (car car))
                              (value (to-json (cdr car)))
                              (cons (assoc key values :test #'equal)))
                         (if cons
                             (setf (cdr cons) value)
                             (setf values (acons key value values))))))
                 (f (cdr args)))))
      (f args)
      (nreverse values))))

(defun arr (list &rest args &key &allow-other-keys)
  (make-arr :values (mapcar (lambda (x) (apply #'to-json x args)) list)))

(defgeneric to-json (x &key &allow-other-keys)
  (:method (x &key &allow-other-keys) x))

(defmethod to-json ((object standard-object)
                    &key (slots (mapcar #'sb-mop:slot-definition-name
                                        (sb-mop:class-slots (class-of object)))))
  (apply #'obj*
         (loop for slot in slots
               if (and (consp slot)
                       (functionp (cadr slot)))
                 collect (cons (car slot)
                               (funcall (cadr slot) object))
               else if (and (consp slot)
                            (handler-case (progn (slot-value object (car slot))
                                                 t)
                              (unbound-slot () nil)))
                      collect (cons (car slot)
                                    (to-json (slot-value object (car slot))
                                             :slots (cdr slot)))
               else if (and (not (consp slot))
                            (not (slot-exists-p object slot)))
                      collect (cons slot (funcall slot object))
               else if (and (not (consp slot))
                            (handler-case (progn (slot-value object slot)
                                                 t)
                              (unbound-slot () nil)))
                      collect (cons slot (slot-value object slot)))))

(defmethod to-json ((cons cons) &rest args &key &allow-other-keys)
  (apply #'arr cons args))

(defgeneric lisp (x)
  (:method (x)
    x)
  (:method ((x cons))
    (cons (to-lisp (car x)) (to-lisp (cdr x))))
  (:method ((x obj))
    (to-lisp (obj-values x)))
  (:method ((x arr))
    (to-lisp (arr-values x))))
