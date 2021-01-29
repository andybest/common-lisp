(in-package #:cl-user)

(defpackage #:origin.common
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow
   #:=)
  (:export
   #:=
   #:cwcmp
   #:cwcmp-or
   #:cwset
   #:make-accessor-symbol))
