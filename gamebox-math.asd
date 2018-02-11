(asdf:defsystem #:gamebox-math
  :description "A high performance math library useful for making games."
  :author "Michael Fiano <mail@michaelfiano.com>"
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :homepage "https://github.com/mfiano/gamebox-math"
  :bug-tracker "https://github.com/mfiano/gamebox-math/issues"
  :source-control (:git "git@github.com:mfiano/gamebox-math.git")
  :version "6.1.1"
  :encoding :utf-8
  :long-description #.(uiop:read-file-string (uiop/pathname:subpathname *load-pathname* "README.md"))
  :depends-on (#:alexandria
               #:defpackage-plus)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "common")
   (:module "vector"
    :components
    ((:file "package")
     (:file "swizzle")
     (:file "vector2")
     (:file "vector3")
     (:file "vector4")))
   (:module "matrix"
    :components
    ((:file "package")
     (:file "matrix2")
     (:file "matrix3")
     (:file "matrix4")))
   (:module "quaternion"
    :components
    ((:file "package")
     (:file "quaternion")
     (:file "dual-quaternion")))))
