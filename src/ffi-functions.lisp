(in-package #:cl-freebsd)

(c:defcfun ("__error" error-number) (:pointer :int))

(c:defcfun ("clearerr" clear-error) :void
  (stream :pointer))

(c:defcfun ("clearerr_unlocked" clear-error-unlocked) :void
  (stream :pointer))

(c:defcfun ("close" %close) :int
  (file-descriptor :int))

(c:defcfun ("feof" file-eof-p) :boolean
  (stream :pointer))

(c:defcfun ("feof_unlocked" file-eof-unlocked-p) :boolean
  (stream :pointer))

(c:defcfun ("ferror" file-error-p) :boolean
  (stream :pointer))

(c:defcfun ("ferror_unlocked" file-error-unlocked-p) :boolean
  (stream :pointer))

(c:defcfun ("fileno" file-number) :int
  (stream :pointer))

(c:defcfun ("fileno_unlocked" file-number-unlocked) :int
  (stream :pointer))

(c:defcfun ("ioctl" %ioctl) :int
  (file-descriptor :int)
  (request ioctl-request)
  &rest)

(c:defcfun ("isatty" tty-p) :boolean
  (file-descriptor :int))

(c:defcfun ("open" %open) :int
  (path :string)
  (flags open-flags)
  &rest)

(c:defcfun ("openat" %open-at) :int
  (file-descriptor :int)
  (path :string)
  (flags open-flags)
  &rest)

(c:defcfun ("perror" print-error) :void
  (string :string))

(c:defcfun ("strerror" string-error) :string
  (error-number :int))

(c:defcfun ("strerror_l" string-error-locale) :string
  (error-number :int)
  (locale :int))

(c:defcfun ("strerror_r" string-error-reentrant) :int
  (error-number :int)
  (buffer :string)
  (length :size))

(c:defcfun ("sysctl" %sysctl) :int
  (name (:pointer :int))
  (name-length :uint)
  (old-ptr (:pointer :void))
  (old-length-ptr (:pointer :size))
  (new-ptr (:pointer :void))
  (new-length :size))

(c:defcfun ("sysctlbyname" %sysctl-by-name) :int
  (name :string)
  (old-ptr (:pointer :void))
  (old-length-ptr (:pointer :size))
  (new-ptr (:pointer :void))
  (new-length :size))

(c:defcfun ("sysctlnametomib" %sysctl-name-to-mib) :int
  (name :string)
  (mib-ptr (:pointer :int))
  (size-ptr (:pointer :size)))

(c:defcfun ("ttyname" %tty-name) :string
  (file-descriptor :int))

(c:defcfun ("ttyname_r" %tty-name-reentrant) :int
  (file-descriptor :int)
  (buffer :string)
  (length :size))
