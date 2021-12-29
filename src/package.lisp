(in-package #:cl-user)

(defpackage #:mfiano.data-structures.doubly-linked-list
  (:local-nicknames
   (#:u #:mfiano.misc.utils))
  (:use #:cl)
  (:shadow
   #:delete
   #:find
   #:length
   #:list
   #:make-list)
  (:export
   #:delete
   #:find
   #:head
   #:insert
   #:length
   #:list
   #:list-values
   #:make-list
   #:next
   #:node
   #:previous
   #:tail
   #:value))
