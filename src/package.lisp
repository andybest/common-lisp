(in-package #:cl-user)

(defpackage #:patchwork
  (:local-nicknames
   (#:bin #:binpack))
  (:use #:cl)
  (:export
   #:make-atlas
   #:make-atlas-from-directory
   #:unpack-atlas))
