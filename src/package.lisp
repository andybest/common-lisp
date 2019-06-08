(in-package :cl-user)

(defpackage #:box.sprite-packer
  (:use #:cl)
  (:import-from #:binpack #:id #:x #:y #:w #:h)
  (:export #:make-atlas
           #:make-atlas-from-directory
           #:unpack-atlas))
