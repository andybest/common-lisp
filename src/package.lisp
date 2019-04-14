(in-package :cl-user)

(defpackage #:parsley
  (:use #:cl)
  (:export
   #:buffer-bytes
   #:buffer-bits
   #:buffer-position
   #:buffer-sequence
   #:buffer-stream
   #:octets=
   #:read-bits
   #:read-bytes
   #:read-uint-be
   #:read-uint-le
   #:read-int-be
   #:read-int-le
   #:read-string
   #:split-string
   #:uncompress-bzip2
   #:uncompress-gzip
   #:uncompress-zlib
   #:uncompress-deflate
   #:with-buffer-read))
