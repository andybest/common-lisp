(in-package #:cl-user)

(defpackage #:mfiano.data-structures.avl-tree
  (:local-nicknames
   (#:u #:mfiano.misc.utils))
  (:use #:cl)
  (:shadow
   #:delete
   #:find
   #:min)
  (:export
   #:clear
   #:copy
   #:delete
   #:find
   #:insert
   #:make-tree
   #:node
   #:tree
   #:valid-p
   #:walk))
