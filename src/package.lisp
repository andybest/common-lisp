(in-package :cl-user)

(defpackage #:flac-read
  (:use #:cl
        #:alexandria
        #:parsley)
  (:export #:load-file
           #:dump-file))
