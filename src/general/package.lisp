(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.origin
  (:local-nicknames
   (#:const #:net.mfiano.lisp.origin.constants)
   (#:u #:net.mfiano.lisp.golden-utils))
  (:use #:cl)
  ;; shaping
  (:export
   #:linear
   #:sine-out
   #:sine-in
   #:sine-in-out
   #:quadratic-out
   #:quadratic-in
   #:quadratic-in-out
   #:cubic-out
   #:cubic-in
   #:cubic-in-out
   #:quartic-out
   #:quartic-in
   #:quartic-in-out
   #:quintic-out
   #:quintic-in
   #:quintic-in-out
   #:exponential-out
   #:exponential-in
   #:exponential-in-out
   #:circular-out
   #:circular-in
   #:circular-in-out
   #:back-out
   #:back-in
   #:back-in-out
   #:elastic-out
   #:elastic-in
   #:elastic-in-out
   #:bounce-out
   #:bounce-in
   #:bounce-in-out
   #:hermite-curve
   #:quintic-curve))
