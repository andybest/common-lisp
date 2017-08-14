(in-package :cl-user)

(defpackage #:parsley
  (:use #:cl)
  (:export #:load-stream
           #:load-file
           #:parse
           #:octets=
           #:read-bytes
           #:read-string))
