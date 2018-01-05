(in-package :defpackage+-user-1)

(defpackage+ #:shadow
  (:use #:cl)
  (:export #:defun-gpu
           #:defstruct-gpu
           #:make-pipeline
           #:compile-program))
