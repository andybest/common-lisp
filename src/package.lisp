(in-package #:cl-user)

(defpackage #:seedable-rng
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow
   #:float)
  (:export
   #:bool
   #:die
   #:element
   #:float
   #:generator
   #:get-seed
   #:int
   #:invalid-range
   #:make-generator
   #:shuffle))
