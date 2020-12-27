(asdf:defsystem #:flac-metadata
  :description "A utility for reading metadata embedded in FLAC audio files."
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "MIT"
  :homepage "https://mfiano.net/projects/flac-metadata"
  :source-control (:git "https://github.com/mfiano/flac-metadata")
  :bug-tracker "https://github.com/mfiano/flac-metadata/issues"
  :encoding :utf-8
  :depends-on (#:golden-utils
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
