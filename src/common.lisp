(in-package #:cl-user)

(defpackage #:origin.common
  (:use #:cl)
  (:export
   #:round-up
   #:round-down))

(in-package #:origin.common)

(defun round-up (x)
  (floor (+ x 1/2)))

(defun round-down (x)
  (ceiling (- x 1/2)))
