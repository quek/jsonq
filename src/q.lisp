(in-package :jsonq)

(defun q (json &rest properties)
  (if (null properties)
      (values json t)
      (let ((property (car properties)))
        (apply #'q
               (etypecase property
                 (integer (etypecase json
                            (arr (unless (< property (length (arr-values json)))
                                   (return-from q (values nil nil)))
                             (nth property (arr-values json)))))
                 (symbol (etypecase json
                           (obj (let ((x (assoc property (obj-values json) :test #'string-equal)))
                                  (unless x
                                    (return-from q (values nil nil)))
                                  (cdr x)))))
                 (string (typecase json
                           (obj (let ((x (assoc property (obj-values json) :test #'equal)))
                                  (unless x
                                    (return-from q (values nil nil)))
                                  (cdr x))))))
               (cdr properties)))))
