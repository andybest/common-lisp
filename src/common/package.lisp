(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.origin.common
  (:local-nicknames
   (#:u #:net.mfiano.lisp.golden-utils))
  (:use #:cl)
  (:shadow
   #:=)
  (:export
   #:=
   #:cwcmp
   #:cwset
   #:make-accessor-symbol))
