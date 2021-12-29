(in-package #:cl-user)

(defpackage #:mfiano.data-structures.slot-map
  (:local-nicknames
   (#:da #:mfiano.data-structures.dynamic-array)
   (#:u #:mfiano.misc.utils))
  (:use #:cl)
  (:shadow
   #:delete
   #:find)
  (:export
   #:delete
   #:find
   #:insert
   #:make-slot-map
   #:map-keys
   #:map-values))
