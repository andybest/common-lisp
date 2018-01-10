(in-package :defpackage+-user-1)

(defpackage+ #:shadow
  (:use #:cl
        #:varjo
        #:varjo.internals)
  (:export #:defun-gpu
           #:defstruct-gpu
           #:make-program))
