(in-package :defpackage+-user-1)

(defpackage+ #:shadow
  (:use #:cl)
  (:export #:defun-gpu
           #:defstruct-gpu
           #:make-program
           #:build-program
           #:build-dictionary
           #:with-program
           #:uniform-bool
           #:uniform-int
           #:uniform-float
           #:uniform-vec
           #:uniform-mat2
           #:uniform-mat3
           #:uniform-mat4))
