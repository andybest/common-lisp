(in-package #:cl-freebsd)

(defmacro with-open ((var path &key flags mode) &body body)
  `(let ((,var (open ,path :flags ,flags ,@(when mode `(:mode ,mode)))))
     (unwind-protect (progn ,@body)
       (close ,var))))

(defmacro with-open-at ((var file-descriptor path &key flags mode) &body body)
  `(let ((,var (open-at ,file-descriptor ,path :flags ,flags ,@(when mode `(:mode ,mode)))))
     (unwind-protect (progn ,@body)
       (close ,var))))

(defmacro with-sysctl ((ptr mib) &body body)
  "Evaluates BODY in a lexical environment where PTR is bound to the foreign result of querying the
  sysctl MIB. MIB should be a Lisp array of integers corresponding to the MIB as returned by
  'sysctlnametomib'"
  (u:with-gensyms (null size mib-length foreign-mib)
    `(let ((,null (c:null-pointer))
           (,mib-length (length ,mib)))
       (c:with-foreign-array (,foreign-mib ,mib '(:array :int ,mib-length))
         (c:with-foreign-pointer (,size ,(c:foreign-type-size :size))
           (sysctl ,foreign-mib ,mib-length ,null ,size ,null 0)
           (c:with-foreign-object (,ptr :char (c:mem-ref ,size :size))
             (sysctl ,foreign-mib ,mib-length ,ptr ,size ,null 0)
             ,@body))))))

(defmacro with-sysctl-by-name ((ptr name) &body body)
  (u:with-gensyms (null size)
    `(let ((,null (c:null-pointer)))
       (c:with-foreign-pointer (,size ,(c:foreign-type-size :size))
         (sysctl-by-name ,name ,null ,size ,null 0)
         (c:with-foreign-object (,ptr :char (c:mem-ref ,size :size))
           (sysctl-by-name ,name ,ptr ,size ,null 0)
           ,@body)))))

(defmacro with-sysctl-mib-from-name ((ptr size name) &body body)
  "Evaluates BODY in a lexical environment where PTR is bound to the foreign result of querying the
  sysctl NAME. SIZE is bound to the length of the MIB array in INTs"
  (u:with-gensyms (size-ptr)
    `(let ((,size ,(1+ (count #\. name))))
       (c:with-foreign-object (,ptr :int ,size)
         (c:with-foreign-pointer (,size-ptr ,(c:foreign-type-size :size))
           (setf (c:mem-ref ,size-ptr :size) ,size)
           (sysctl-name-to-mib ,name ,ptr ,size-ptr)
           ,@body)))))
