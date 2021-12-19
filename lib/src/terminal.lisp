(in-package #:freebsd-tools.lib)

(declaim (inline save-cursor))
(defun save-cursor ()
  (write-string "7"))

(declaim (inline restore-cursor))
(defun restore-cursor ()
  (write-string "8"))

(declaim (inline move-cursor-up))
(defun move-cursor-up (rows)
  (when (plusp rows)
    (format t "[~aA" rows)))

(declaim (inline erase-down))
(defun erase-down ()
  (write-string "[J"))

(declaim (inline repeat-character))
(defun repeat-character (character count)
  (dotimes (i count)
    (write-char character)))

(declaim (inline reset-display-attributes))
(defun reset-display-attributes ()
  (format t "[0m"))

(declaim (inline print-color-code))
(defun print-color-code (color)
  (format t "[~{~a~^;~}m" color))

(defun prepare-terminal-output (line-count &key replace)
  (when (and replace (null *interactive*))
    (repeat-character #\newline line-count)
    (move-cursor-up line-count)
    (save-cursor)))

(defun finish-terminal-output (&key replace)
  (finish-output)
  (cond
    (replace
     (restore-cursor)
     (erase-down))
    (t
     (fresh-line))))

(defun get-terminal-column-count ()
  (let ((tty-name (bsd:tty-name (bsd:file-number bsd:+stdin+))))
    (bsd:with-open (file-descriptor tty-name :flags '(:read-only))
      (c:with-foreign-object (ptr '(:struct bsd:window-size))
        (bsd:ioctl file-descriptor :tiocgwinsz :pointer ptr)
        (c:foreign-slot-value ptr '(:struct bsd:window-size) :columns)))))
