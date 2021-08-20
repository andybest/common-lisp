(in-package #:mfiano-utils)

(defun noop (&rest args)
  "Do nothing."
  (declare (ignore args))
  (values))
