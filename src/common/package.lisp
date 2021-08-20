(in-package #:cl-user)

(defpackage #:origin.common
  (:local-nicknames
   (#:u #:mfiano-utils))
  (:use #:cl)
  (:shadow
   #:=)
  (:export
   #:=
   #:cwcmp
   #:cwcmp-or
   #:cwset
   #:make-accessor-symbol))
