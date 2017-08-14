(asdf:defsystem #:parsley
  :description ""
  :author ("Michael Fiano <michael.fiano@gmail.com>")
  :maintainer "Michael Fiano <michael.fiano@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/mfiano/parsley"
  :bug-tracker "https://github.com/mfiano/parsley/issues"
  :source-control (:git "git@github.com:mfiano/parsley.git")
  :version "0.1.0"
  :encoding :utf-8
  :long-description #.(uiop:read-file-string
                       (uiop/pathname:subpathname *load-pathname* "README.md"))
  :depends-on (#:fast-io
               #:chipz
               #:babel)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "common")
   (:file "processors")
   (:file "operations")
   (:file "object")))
