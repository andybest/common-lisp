(asdf:defsystem #:flac-read
  :description "A utility for reading metadata embedded in FLAC audio files."
  :author ("Michael Fiano <michael.fiano@gmail.com>")
  :maintainer "Michael Fiano <michael.fiano@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/mfiano/flac-read"
  :bug-tracker "https://github.com/mfiano/flac-read/issues"
  :source-control (:git "git@github.com:mfiano/flac-read.git")
  :version "0.1.0"
  :encoding :utf-8
  :long-description #.(uiop:read-file-string
                       (uiop/pathname:subpathname *load-pathname* "README.md"))
  :depends-on (#:alexandria
               #:parsley)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "flac")
   (:file "datastream")
   (:file "metadata")
   (:file "metadata-types")
   (:file "dump-data")))
