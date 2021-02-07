(asdf:defsystem #:seedable-rng
  :description "A seedable random number generator."
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "MIT"
  :homepage "https://mfiano.net/projects/seedable-rng"
  :source-control (:git "https://github.com/mfiano/seedable-rng.git")
  :encoding :utf-8
  :depends-on (#:cl-pcg
               #:golden-utils
               #:ironclad)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "conditions")
   (:file "dictionary")
   (:file "generator")))
