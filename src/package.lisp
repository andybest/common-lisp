(in-package :cl-user)

(defpackage #:gamebox-sprite-packer
  (:nicknames #:box.packer)
  (:use #:cl
        #:alexandria)
  (:export #:make-atlas
           #:make-atlas-from-directory
           #:unpack-atlas))
