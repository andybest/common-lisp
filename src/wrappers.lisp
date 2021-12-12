(in-package #:cl-freebsd)

(defun close (file-descriptor)
  (with-error-check (= -1)
    (%close file-descriptor)))

(defun file-eof-p (stream)
  (not (zerop (%file-eof-p stream))))

(defun file-eof-unlocked-p (stream)
  (not (zerop (%file-eof-unlocked-p stream))))

(defun file-error-p (stream)
  (not (zerop (%file-error-p stream))))

(defun file-error-unlocked-p (stream)
  (not (zerop (%file-error-unlocked-p stream))))

(defmacro ioctl (file-descriptor request &rest args)
  `(with-error-check (= -1)
     (%ioctl ,file-descriptor ,request ,@args)))

(defun open (path &key flags (mode #o000))
  (with-error-check (= -1)
    (%open path flags :uint16 mode)))

(defun open-at (file-descriptor path &key flags (mode #o000))
  (with-error-check (= -1)
    (%open-at file-descriptor path flags :uint16 mode)))

(defun sysctl-by-name (name old-ptr old-length-ptr new-ptr new-length)
  (with-error-check (/= 0)
    (%sysctl-by-name name old-ptr old-length-ptr new-ptr new-length)))

(defun tty-name (file-descriptor)
  (or (%tty-name file-descriptor)
      (error 'freebsd-error)))

(defun tty-name-reentrant (file-descriptor buffer length)
  (with-error-check (/= 0)
    (%tty-name-reentrant file-descriptor buffer length)))

(defun tty-p (file-descriptor)
  (= (%tty-p file-descriptor) 1))
