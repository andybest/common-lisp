(asdf:defsystem #:flac-metadata
  :description "A utility for reading metadata embedded in FLAC audio files."
  :author ("Michael Fiano <mail@michaelfiano.com>")
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :homepage "https://github.com/mfiano/flac-metadata"
  :bug-tracker "https://github.com/mfiano/flac-metadata/issues"
  :source-control (:git "git@github.com:mfiano/flac-metadata.git")
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
