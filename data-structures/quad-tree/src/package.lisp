(in-package #:cl-user)

(defpackage #:mfiano.data-structures.quad-tree
  (:local-nicknames
   (#:u #:mfiano.misc.utils)
   (#:v2 #:mfiano.math.origin.vec2))
  (:use #:cl)
  (:export
   #:boundary
   #:insert
   #:make-boundary
   #:make-tree
   #:query
   #:tree))
