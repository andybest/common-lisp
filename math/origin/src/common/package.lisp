(in-package #:cl-user)

(defpackage #:mfiano.math.origin.common
  (:local-nicknames
   (#:u #:mfiano.misc.utils))
  (:use #:cl)
  (:shadow
   #:=)
  (:export
   #:=
   #:cwcmp
   #:cwcmp-or
   #:cwset
   #:make-accessor-symbol))
