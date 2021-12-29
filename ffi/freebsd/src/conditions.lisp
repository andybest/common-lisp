(in-package #:mfiano.ffi.freebsd)

(defgeneric %get-error-code (errnum)
  (:method ((errnum (eql 1)))
    "EPERM")
  (:method ((errnum (eql 2)))
    "ENOENT")
  (:method ((errnum (eql 3)))
    "ESRCH")
  (:method ((errnum (eql 4)))
    "EINTR")
  (:method ((errnum (eql 5)))
    "EIO")
  (:method ((errnum (eql 6)))
    "ENXIO")
  (:method ((errnum (eql 7)))
    "E2BIG")
  (:method ((errnum (eql 8)))
    "ENOEXEC")
  (:method ((errnum (eql 9)))
    "EBADF")
  (:method ((errnum (eql 10)))
    "ECHILD")
  (:method ((errnum (eql 11)))
    "EDEADLK")
  (:method ((errnum (eql 12)))
    "ENOMEM")
  (:method ((errnum (eql 13)))
    "EACCES")
  (:method ((errnum (eql 14)))
    "EFAULT")
  (:method ((errnum (eql 15)))
    "ENOTBLK")
  (:method ((errnum (eql 16)))
    "EBUSY")
  (:method ((errnum (eql 17)))
    "EEXIST")
  (:method ((errnum (eql 18)))
    "EXDEV")
  (:method ((errnum (eql 19)))
    "ENODEV")
  (:method ((errnum (eql 20)))
    "ENOTDIR")
  (:method ((errnum (eql 21)))
    "EISDIR")
  (:method ((errnum (eql 22)))
    "EINVAL")
  (:method ((errnum (eql 23)))
    "ENFILE")
  (:method ((errnum (eql 24)))
    "EMFILE")
  (:method ((errnum (eql 25)))
    "ENOTTY")
  (:method ((errnum (eql 26)))
    "ETXTBSY")
  (:method ((errnum (eql 27)))
    "EFBIG")
  (:method ((errnum (eql 28)))
    "ENOSPC")
  (:method ((errnum (eql 29)))
    "ESPIPE")
  (:method ((errnum (eql 30)))
    "EROFS")
  (:method ((errnum (eql 31)))
    "EMLINK")
  (:method ((errnum (eql 32)))
    "EPIPE")
  (:method ((errnum (eql 33)))
    "EDOM")
  (:method ((errnum (eql 34)))
    "ERANGE")
  (:method ((errnum (eql 35)))
    "EAGAIN")
  (:method ((errnum (eql 36)))
    "EINPROGRESS")
  (:method ((errnum (eql 37)))
    "EALREADY")
  (:method ((errnum (eql 38)))
    "ENOTSOCK")
  (:method ((errnum (eql 39)))
    "EDESTADDRREQ")
  (:method ((errnum (eql 40)))
    "EMSGSIZE")
  (:method ((errnum (eql 41)))
    "EPROTOTYPE")
  (:method ((errnum (eql 42)))
    "ENOPROTOOPT")
  (:method ((errnum (eql 43)))
    "EPROTONOSUPPORT")
  (:method ((errnum (eql 44)))
    "ESOCKTNOSUPPORT")
  (:method ((errnum (eql 45)))
    "EOPNOTSUPP")
  (:method ((errnum (eql 46)))
    "EPFNOSUPPORT")
  (:method ((errnum (eql 47)))
    "EAFNOSUPPORT")
  (:method ((errnum (eql 48)))
    "EADDRINUSE")
  (:method ((errnum (eql 49)))
    "EADDRNOTAVAIL")
  (:method ((errnum (eql 50)))
    "ENETDOWN")
  (:method ((errnum (eql 51)))
    "ENETUNREACH")
  (:method ((errnum (eql 52)))
    "ENETRESET")
  (:method ((errnum (eql 53)))
    "ECONNABORTED")
  (:method ((errnum (eql 54)))
    "ECONNRESET")
  (:method ((errnum (eql 55)))
    "ENOBUFS")
  (:method ((errnum (eql 56)))
    "EISCONN")
  (:method ((errnum (eql 57)))
    "ENOTCONN")
  (:method ((errnum (eql 58)))
    "ESHUTDOWN")
  (:method ((errnum (eql 59)))
    "ETOOMANYREFS")
  (:method ((errnum (eql 60)))
    "ETIMEDOUT")
  (:method ((errnum (eql 61)))
    "ECONNREFUSED")
  (:method ((errnum (eql 62)))
    "ELOOP")
  (:method ((errnum (eql 63)))
    "ENAMETOOLONG")
  (:method ((errnum (eql 64)))
    "EHOSTDOWN")
  (:method ((errnum (eql 65)))
    "EHOSTUNREACH")
  (:method ((errnum (eql 66)))
    "ENOTEMPTY")
  (:method ((errnum (eql 67)))
    "EPROCLIM")
  (:method ((errnum (eql 68)))
    "EUSERS")
  (:method ((errnum (eql 69)))
    "EDQUOT")
  (:method ((errnum (eql 70)))
    "ESTALE")
  (:method ((errnum (eql 71)))
    "EREMOTE")
  (:method ((errnum (eql 72)))
    "EBADRPC")
  (:method ((errnum (eql 73)))
    "ERPCMISMATCH")
  (:method ((errnum (eql 74)))
    "EPROGUNAVAIL")
  (:method ((errnum (eql 75)))
    "EPROGMISMATCH")
  (:method ((errnum (eql 76)))
    "EPROCUNAVAIL")
  (:method ((errnum (eql 77)))
    "ENOLCK")
  (:method ((errnum (eql 78)))
    "ENOSYS")
  (:method ((errnum (eql 79)))
    "EFTYPE")
  (:method ((errnum (eql 80)))
    "EAUTH")
  (:method ((errnum (eql 81)))
    "ENEEDAUTH")
  (:method ((errnum (eql 82)))
    "EIDRM")
  (:method ((errnum (eql 83)))
    "ENOMSG")
  (:method ((errnum (eql 84)))
    "EOVERFLOW")
  (:method ((errnum (eql 85)))
    "ECANCELED")
  (:method ((errnum (eql 86)))
    "EILSEQ")
  (:method ((errnum (eql 87)))
    "ENOATTR")
  (:method ((errnum (eql 88)))
    "EDOOFUS")
  (:method ((errnum (eql 89)))
    "EBADMSG")
  (:method ((errnum (eql 90)))
    "EMULTIHOP")
  (:method ((errnum (eql 91)))
    "ENOLINK")
  (:method ((errnum (eql 92)))
    "EPROTO")
  (:method ((errnum (eql 93)))
    "ENOTCAPABLE")
  (:method ((errnum (eql 94)))
    "ECAPMODE")
  (:method ((errnum (eql 95)))
    "ENOTRECOVERABLE")
  (:method ((errnum (eql 96)))
    "EOWNERDEAD")
  (:method ((errnum (eql 97)))
    "EINTEGRITY"))

(define-condition freebsd-error (error) ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (let ((error-number (c:mem-ref (error-number) :int)))
       (format stream "Error ~d (~a): ~a"
               error-number
               (%get-error-code error-number)
               (string-error error-number))))))