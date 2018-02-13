(in-package :cl-user)

(defpackage #:flac-metadata
  (:use #:cl
        #:parsley)
  (:import-from #:alexandria
                #:with-gensyms
                #:symbolicate
                #:make-keyword
                #:hash-table-alist)
  (:export #:load-file
           #:dump-file
           #:parse-tree))
