(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.flac-metadata
  (:local-nicknames
   (#:parse #:net.mfiano.lisp.parsley)
   (#:u #:net.mfiano.lisp.golden-utils))
  (:use #:cl)
  (:export
   #:load-file
   #:dump-file
   #:parse-tree))
