(asdf:defsystem #:mfiano.math.gfxmath
  :description "A graphics math library."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://github.com/mfiano/common-lisp/tree/master/math/gfxmath"
  :version "0.1.1"
  :encoding :utf-8
  :depends-on (#:mfiano.misc.utils
               #:str)
  :in-order-to ((asdf:test-op (asdf:test-op #:mfiano.math.gfxmath.test)))
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "macros")
   (:file "types")
   (:file "common")
   (:file "shared-ops")
   (:file "vector")
   (:file "matrix")
   (:file "quaternion")))
