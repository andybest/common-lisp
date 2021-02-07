(asdf:defsystem #:random-uuid
  :description "Create and parse RFC-4122 UUID version 4 identifiers."
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "MIT"
  :homepage "https://mfiano.net/projects/random-uuid"
  :source-control (:git "https://github.com/mfiano/random-uuid.git")
  :encoding :utf-8
  :depends-on (#:golden-utils
               #:seedable-rng)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "conditions")
   (:file "uuid")))
