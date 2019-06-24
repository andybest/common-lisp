(asdf:defsystem #:flac-metadata
  :description "A utility for reading metadata embedded in FLAC audio files."
  :author ("Michael Fiano <mail@michaelfiano.com>")
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :homepage "https://www.michaelfiano.com/projects/flac-metadata"
  :bug-tracker "https://github.com/mfiano/flac-metadata/issues"
  :source-control (:git "https://github.com/mfiano/flac-metadata.git")
  :encoding :utf-8
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
