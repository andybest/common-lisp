(in-package :cl-user)

(defpackage #:parsley
  (:use #:cl)
  (:export #:*byte-buffer*
           #:octets=
           #:read-bytes
           #:read-uint-be
           #:read-uint-le
           #:read-int-be
           #:read-int-le
           #:read-string
           #:uncompress-bzip2
           #:uncompress-gzip
           #:uncompress-zlib
           #:uncompress-deflate))
