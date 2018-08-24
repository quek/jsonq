(in-package :jsonq)

(defun q (json &rest properties)
  (if (null properties)
      json
      (let ((property (car properties)))
        (apply #'q
               (etypecase property
                 (integer (etypecase json
                            (arr (nth property (arr-values json)))))
                 (symbol (etypecase json
                           (obj (cdr (assoc property (obj-values json) :test #'string-equal)))))
                 (string (typecase json
                           (obj (cdr (assoc property (obj-values json) :test #'equal))))))
               (cdr properties)))))
