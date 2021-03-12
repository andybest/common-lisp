(asdf:defsystem #:flac-metadata
  :description "A utility for reading metadata embedded in FLAC audio files."
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "MIT"
  :homepage "https://git.mfiano.net/mfiano/flac-metadata"
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
