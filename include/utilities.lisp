(in-package #:mfiano.scripts.base)

(defgeneric validate-option (key value)
  (:method (key value)))

(defun parse-options (ui &optional options)
  (u:mvlet ((args options (if *interactive*
                              (ui:parse-options ui options)
                              (ui:parse-options-or-exit ui))))
    (u:do-hash (k v options)
      (validate-option k v))
    (values args options)))

(defun parse-integer (&optional string)
  (u:mvlet* ((value (or string ""))
             (parsed position (cl:parse-integer value :junk-allowed t)))
    (or (and (= position (length string)) parsed)
        (error 'user-error :message (format nil "Integer expected but got ~s." value)))))

(defun parse-float (&optional string)
  (u:mvlet* ((value (or string ""))
             (parsed position (pf:parse-float value :junk-allowed t)))
    (or (and (= position (length string)) parsed)
        (error 'user-error
               :message (format nil "Floating-point number expected but got ~s." value)))))

(defun in-range (low high)
  (lambda (x) (and (realp x) (<= low x high))))

(defun print-help (ui &key (padding 20))
  (let ((printer (if *interactive* #'ui:print-help #'ui:print-help-and-exit)))
    (funcall printer ui :program-name *program-name* :option-width padding)
    t))

(defun print-version ()
  (flet ((get-copyright-years ()
           (let ((year (load-time-value
                        (cl:parse-integer
                         (lt:format-timestring nil (lt:now) :format '((:year 4)))))))
             (if (> year *initial-year*)
                 (format nil "~a-~a" *initial-year* year)
                 year))))
    (format t "~a v~a~%" *program-name* *version*)
    (when *authors*
      (format t "Copyright (c) ~a ~{~a~^, ~} and contributors~%" (get-copyright-years) *authors*))
    (format t "Compiled for ~a with ~a v~a on ~a~%"
            (load-time-value (machine-type))
            (load-time-value (lisp-implementation-type))
            (load-time-value (lisp-implementation-version))
            (load-time-value (lt:format-timestring nil (lt:now) :format lt:+rfc-1123-format+)))
    (if *interactive* t (ui:exit))))

(defun dispatch-terminating-options (ui options)
  (cond
    ((u:href options 'help)
     (print-help ui))
    ((u:href options 'version)
     (print-version))))
