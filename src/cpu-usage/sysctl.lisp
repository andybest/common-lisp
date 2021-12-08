(in-package #:mfiano.scripts.cpu-usage)

(cffi:define-foreign-library libc
  (:freebsd "/lib/libc.so.7"))

(cffi:use-foreign-library libc)

(cffi:defcfun ("__error" error-number) (:pointer :int))

(cffi:defcfun ("strerror" get-error-message) :string
  (error-number :int))

(cffi:defcfun ("sysctlbyname" %sysctl-by-name) :int
  (name (:pointer :char))
  (old-ptr (:pointer :void))
  (old-length-ptr (:pointer :size))
  (new-ptr (:pointer :void))
  (new-length :size))

(cffi:defcstruct clock-info
  (hz :int)
  (tick :int)
  (stat-hz :int :offset 12)
  (prof-hz :int))

(define-condition sysctl-error (base:user-error)
  ((%number :initarg :number
            :reader sysctl-error-number)
   (%code :initarg :code
          :reader sysctl-error-code))
  (:report (lambda (condition stream)
             (let ((*print-circle* nil))
               (format stream
                       "libc sysctl(3) error ~d~%~%~a: ~a~%~@
                        Note: libc error messages are generic and not necessarily indicative of ~@
                        the actual problem. Instead, consult the \"ERRORS\" section of the ~@
                        sysctl(3) manpage for what the error code (~a in this case) could mean."
                       (sysctl-error-number condition)
                       (sysctl-error-code condition)
                       (base:user-error-message condition)
                       (sysctl-error-code condition))))))

(defun error-number->error-code (number)
  (ecase number
    (1 "EPERM")
    (2 "ENOENT")
    (12 "ENOMEM")
    (14 "EFAULT")
    (20 "ENOTDIR")
    (21 "EISDIR")
    (22 "EINVAL")))

(defun sysctl-by-name (name old-ptr old-length-ptr new-ptr new-length)
  (unless (zerop (%sysctl-by-name name old-ptr old-length-ptr new-ptr new-length))
    (let ((error-number (cffi:mem-ref (error-number) :int)))
      (error 'sysctl-error
             :number error-number
             :code (error-number->error-code error-number)
             :message (get-error-message error-number))))
  (values))

(defmacro with-sysctl ((ptr name) &body body)
  (u:with-gensyms (str null size)
    `(let ((,null (cffi:null-pointer)))
       (cffi:with-foreign-string (,str ,name)
         (cffi:with-foreign-pointer (,size ,(cffi:foreign-type-size :size))
           (sysctl-by-name ,str ,null ,size ,null 0)
           (cffi:with-foreign-object (,ptr :char (cffi:mem-ref ,size :size))
             (sysctl-by-name ,str ,ptr ,size ,null 0)
             ,@body))))))

(defun get-cpu-times ()
  (with-sysctl (ptr "kern.cp_time")
    (let ((array (cffi:foreign-array-to-lisp ptr '(:array :uint64 5) :element-type 'u:ub64)))
      (map 'list #'identity array))))

(defun get-stat-hz ()
  (with-sysctl (ptr "kern.clockrate")
    (cffi:foreign-slot-value ptr '(:struct clock-info) 'stat-hz)))
