(in-package #:cl-user)

(defpackage #:mfiano.file-formats.flac
  (:local-nicknames
   (#:parse #:mfiano.misc.binary-parser)
   (#:u #:mfiano.misc.utils))
  (:use #:cl)
  (:export
   #:load-file
   #:dump-file
   #:parse-tree))
