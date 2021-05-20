(in-package #:cl-user)

(defpackage #:%syntex.internal
  (:use #:cl)
  (:export
   #:check-file-exists
   #:check-image-dimension
   #:check-output-path
   #:check-seed
   #:file-not-found
   #:invalid-dimension
   #:invalid-harrison-candidate-count
   #:invalid-harrison-rounds
   #:invalid-harrison-kernel-size
   #:invalid-output-path
   #:invalid-seed
   #:invalid-wfc-pattern-size
   #:syntex-error
   #:wfc-contradiction))

(defpackage #:%syntex.image
  (:local-nicknames
   (#:png #:pngload)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:data
   #:from-argb
   #:height
   #:make-image
   #:width
   #:write-image))
