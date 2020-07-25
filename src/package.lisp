(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.glsl-metadata
  (:use #:cl)
  (:export))

(defpackage #:net.mfiano.lisp.glsl
  (:use #:cl)
  (:shadow
   #:defconstant
   #:defmacro
   #:defstruct
   #:defun)
  (:export
   #:defconstant
   #:defmacro
   #:defstruct
   #:defun))

(defpackage #:net.mfiano.lisp.%glsl
  (:local-nicknames
   (#:u #:net.mfiano.lisp.golden-utils)
   (#:glsl #:net.mfiano.lisp.glsl))
  (:use #:cl)
  (:export))
