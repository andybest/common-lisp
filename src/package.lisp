(in-package :cl-user)

(defpackage #:flac-metadata
  (:use #:cl
        #:alexandria
        #:parsley)
  (:export #:load-file
           #:dump-file))
