(in-package #:cl-user)

(defpackage #:mfiano.misc.uuid
  (:local-nicknames
   (#:rng #:mfiano.misc.rng)
   (#:u #:mfiano.misc.utils))
  (:use #:cl)
  (:export
   #:from-string
   #:make-uuid
   #:to-string
   #:uuid
   #:valid-string-p
   #:variant
   #:version))
