(in-package #:cl-user)

(defpackage #:mfiano.ffi.freebsd
  (:local-nicknames
   (#:c #:cffi)
   (#:u #:mfiano.misc.utils))
  (:use #:cl)
  (:shadow
   #:close
   #:open
   #:read
   #:write)
  ;; constants
  (:export
   #:+stderr+
   #:+stdin+
   #:+stdout+)
  ;; structures
  (:export
   #:clock-info
   #:window-size)
  ;; functions
  (:export
   #:clear-error
   #:clear-error-unlocked
   #:close
   #:file-eof-p
   #:file-eof-unlocked-p
   #:file-error-p
   #:file-error-unlocked-p
   #:file-number
   #:file-number-unlocked
   #:ioctl
   #:open
   #:open-at
   #:print-error
   #:read
   #:string-error
   #:string-error-locale
   #:string-error-reentrant
   #:sysctl-by-name
   #:tty-name
   #:tty-name-reentrant
   #:write
   #:write-all)
  ;; convenience macros
  (:export
   #:with-open
   #:with-open-at
   #:with-sysctl
   #:with-sysctl-by-name
   #:with-sysctl-mib-from-name))
