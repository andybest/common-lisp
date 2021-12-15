(in-package #:cl-user)

(defpackage #:cl-freebsd
  (:local-nicknames
   (#:c #:cffi)
   (#:u #:mfiano-utils))
  (:use #:cl)
  (:shadow
   #:close
   #:open)
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
   #:string-error
   #:string-error-locale
   #:string-error-reentrant
   #:sysctl-by-name
   #:tty-name
   #:tty-name-reentrant)
  ;; convenience macros
  (:export
   #:with-open
   #:with-open-at
   #:with-sysctl
   #:with-sysctl-by-name
   #:with-sysctl-mib-from-name))
