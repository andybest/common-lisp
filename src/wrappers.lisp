(in-package #:cl-freebsd)

(defun close (file-descriptor)
  (with-error-check (= -1)
    (%close file-descriptor)))

(defmacro ioctl (file-descriptor request &rest args)
  `(with-error-check (= -1)
     (%ioctl ,file-descriptor ,request ,@args)))

(defun open (path &key flags (mode #o000))
  (with-error-check (= -1)
    (%open path flags :uint16 mode)))

(defun open-at (file-descriptor path &key flags (mode #o000))
  (with-error-check (= -1)
    (%open-at file-descriptor path flags :uint16 mode)))

(defun sysctl (name name-length old-ptr old-length-ptr new-ptr new-length)
  (with-error-check (/= 0)
    (%sysctl name name-length old-ptr old-length-ptr new-ptr new-length)))

(defun sysctl-by-name (name old-ptr old-length-ptr new-ptr new-length)
  (with-error-check (/= 0)
    (%sysctl-by-name name old-ptr old-length-ptr new-ptr new-length)))

(defun sysctl-name-to-mib (name mib-ptr size-ptr)
  (with-error-check (/= 0)
    (%sysctl-name-to-mib name mib-ptr size-ptr)))

(defun tty-name (file-descriptor)
  (or (%tty-name file-descriptor)
      (error 'freebsd-error)))

(defun tty-name-reentrant (file-descriptor buffer length)
  (with-error-check (/= 0)
    (%tty-name-reentrant file-descriptor buffer length)))
