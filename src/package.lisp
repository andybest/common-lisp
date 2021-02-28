(in-package #:cl-user)

(defpackage #:syntex
  (:local-nicknames
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow
   #:method)
  (:export))
