(in-package #:cl-user)

(defpackage #:mfiano.graphics.tools.convolution-kernel
  (:local-nicknames
   (#:tg #:mfiano.graphics.tools.tile-grid)
   (#:u #:mfiano.misc.utils))
  (:use #:cl)
  (:shadow
   #:count
   #:find
   #:map)
  (:export
   #:align
   #:collect
   #:convolve
   #:count
   #:detect
   #:do-kernel
   #:flood-fill
   #:find
   #:make-kernel
   #:map
   #:origin
   #:process
   #:reshape
   #:resolve))
