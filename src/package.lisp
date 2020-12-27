(in-package #:cl-user)

(defpackage #:flac-metadata
  (:local-nicknames
   (#:parse #:parsley)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:load-file
   #:dump-file
   #:parse-tree))
