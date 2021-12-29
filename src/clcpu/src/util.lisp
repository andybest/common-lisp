(in-package #:mfiano.cmd.freebsd.clcpu)

(defun color->ansi (color &key (type :fg))
  (let ((modifier (if (eq type :bg) 10 0)))
    (cond
      ((<= 0 color 7)
       (+ color 30 modifier))
      ((<= 8 color 15)
       (+ color 82 modifier))
      (t
       (lib:user-error "Invalid color: ~d" color)))))

(defun parse-color-pair (color)
  (destructuring-bind (fg &optional bg) (u:split-sequence #\, color :count 2)
    (list (if (u:emptyp fg)
              90
              (color->ansi (lib:parse-integer fg) :type :fg))
          (if (u:emptyp bg)
              40
              (color->ansi (lib:parse-integer bg) :type :bg)))))
