(in-package #:cl-user)

(defpackage #:mfiano.data-structures.identifier-pool
  (:local-nicknames
   (#:da #:mfiano.data-structures.dynamic-array)
   (#:u #:mfiano.misc.utils))
  (:use #:cl)
  (:shadow
   #:count
   #:map)
  (:export
   #:active-p
   #:count
   #:free
   #:generate
   #:id
   #:make-pool
   #:map
   #:version))
