(in-package #:cl-user)

(defpackage #:mfiano.scripts.base
  (:local-nicknames
   (#:lt #:local-time)
   (#:pf #:parse-float)
   (#:u #:mfiano-utils)
   (#:ui #:adopt)
   (#:wua #:with-user-abort))
  (:use #:cl)
  (:shadow
   #:parse-integer)
  (:export
   #:*authors*
   #:*initial-year*
   #:*option-help*
   #:*option-version*
   #:*program-name*
   #:*version*
   #:define-boolean-options
   #:define-option
   #:dispatch-terminating-options
   #:in-range
   #:parse-float
   #:parse-integer
   #:parse-options
   #:run-non-interactively
   #:string-match
   #:user-error
   #:user-error-message
   #:validate-option))
