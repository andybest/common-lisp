(in-package :cl-user)

(defpackage #:parsley
  (:use #:cl)
  (:export #:load-stream
           #:load-file
           #:parse
           #:octets=
           #:read-bytes
           #:read-uint-be
           #:read-uint-le
           #:read-int-be
           #:read-int-le
           #:read-string))
