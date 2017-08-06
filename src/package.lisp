(in-package :cl-user)

(defpackage #:gamebox-sprite-packer
  (:nicknames #:box.packer)
  (:use #:cl
        #:alexandria)
  (:export #:collect-rects
           #:make-atlas))
