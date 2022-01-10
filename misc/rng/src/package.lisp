(in-package #:cl-user)

(defpackage #:mfiano.misc.rng
  (:local-nicknames
   (#:u #:mfiano.misc.utils))
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
   #:int/parity
   #:invalid-range
   #:make-generator
   #:shuffle))
