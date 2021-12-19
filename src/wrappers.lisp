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

(defun read (file-descriptor buffer byte-count)
  (with-error-check (= -1)
    (%read file-descriptor buffer byte-count)))

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

(defun write (file-descriptor buffer byte-count)
  (with-error-check (= -1)
    (%write file-descriptor buffer byte-count)))

;; This is a high-level convenience function that wraps WRITE.
(defun write-all (file-descriptor array)
  "Writes the data stored in ARRAY to the file designated by FILE-DESCRIPTOR. ARRAY should be a
  vector with ELEMENT-TYPE (UNSIGNED-BYTE (CFFI:FOREIGN-TYPE-SIZE :CHAR)). It's almost always going
  to be (UNSIGNED-BYTE 8), but that's not strictly guaranteed."
  (let ((length (length array)))
    (c:with-foreign-array (buffer array `(:array :char ,length))
      (u:until (zerop length)
        (let ((written (write file-descriptor buffer length)))
          (decf length written))))))
