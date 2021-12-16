(in-package #:mfiano.scripts.cpu-usage)

(defun get-cpu-times ()
  (bsd:with-sysctl-by-name (ptr "kern.cp_time")
    (map 'list #'identity (c:foreign-array-to-lisp ptr '(:array :long 5) :element-type 'u:ub64))))

(defun get-stat-hz ()
  (bsd:with-sysctl-by-name (ptr "kern.clockrate")
    (c:foreign-slot-value ptr '(:struct bsd:clock-info) :stat-hz)))

(defun get-column-count ()
  (let ((tty-name (bsd:tty-name (bsd:file-number bsd:+stdin+))))
    (bsd:with-open (file-descriptor tty-name :flags '(:read-only))
      (c:with-foreign-object (ptr '(:struct bsd:window-size))
        (bsd:ioctl file-descriptor :tiocgwinsz :pointer ptr)
        (c:foreign-slot-value ptr '(:struct bsd:window-size) :columns)))))
