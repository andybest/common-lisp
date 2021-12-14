(in-package #:mfiano.scripts.cpu-usage)

(defun get-cpu-times ()
  (bsd:with-sysctl-by-name (ptr "kern.cp_time")
    (map 'list #'identity (c:foreign-array-to-lisp ptr '(:array :long 5) :element-type 'u:ub64))))

(defun get-stat-hz ()
  (bsd:with-sysctl-by-name (ptr "kern.clockrate")
    (c:foreign-slot-value ptr '(:struct bsd:clock-info) :stat-hz)))
