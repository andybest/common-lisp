(in-package #:cl-user)

(defpackage #:mfiano.data-structures.dynamic-array
  (:local-nicknames
   (#:u #:mfiano.misc.utils))
  (:use #:cl)
  (:shadow
   #:aref
   #:fill-pointer
   #:find
   #:length
   #:make-array
   #:map
   #:pop
   #:push)
  (:export
   #:aref
   #:copy
   #:dynamic-array
   #:find
   #:length
   #:make-array
   #:map
   #:pop
   #:push))
