(in-package #:cl-user)

(defpackage #:mfiano.data-structures.sparse-set
  (:local-nicknames
   (#:u #:mfiano.misc.utils))
  (:use #:cl)
  (:shadow
   #:delete
   #:find
   #:length
   #:map)
  (:export
   #:copy
   #:clear
   #:delete
   #:find
   #:insert
   #:length
   #:make-sparse-set
   #:map))
