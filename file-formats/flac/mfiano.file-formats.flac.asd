(asdf:defsystem #:mfiano.file-formats.flac
  :description "A utility for reading metadata embedded in FLAC audio files."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://github.com/mfiano/common-lisp/tree/master/file-formats/flac"
  :encoding :utf-8
  :depends-on (#:mfiano.misc.binary-parser
               #:mfiano.misc.utils)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "flac")
   (:file "datastream")
   (:file "metadata")
   (:file "metadata-types")
   (:file "dump-data")))
