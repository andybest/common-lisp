(in-package #:cl-user)

(defpackage #:coherent-noise.internal
  (:local-nicknames
   (#:lp #:lparallel)
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl)
  ;; API
  (:export
   #:sample
   #:write-image)
  ;; Conditions
  (:export
   #:coherent-noise-error
   #:invalid-cellular-distance-method
   #:invalid-cellular-jitter
   #:invalid-cellular-output-type
   #:invalid-modifier-input
   #:invalid-open-simplex2-orientation
   #:invalid-seed))
