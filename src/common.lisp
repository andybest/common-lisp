(in-package :shadow)

(defun ensure-keyword (x)
  (etypecase x
    ((or number string symbol)
     (alexandria:make-keyword (format nil "~a" x)))))

(defun parts->string (parts &optional (filter #'identity))
  (with-output-to-string (s)
    (flet ((convert (parts)
             (mapcar
              (lambda (part)
                (etypecase part
                  ((or symbol string) (funcall filter part))
                  (integer part)))
              parts)))
      (loop :for (part . rest) :on (convert parts)
            :for separator = "" :then "."
            :do (etypecase part
                  ((or symbol string) (format s "~a~a" separator part))
                  (integer (format s "[~a]" part)))))))

(defun has-qualifier-p (type qualifier)
  (member qualifier (varjo:qualifiers type) :test #'varjo.internals:qualifier=))
