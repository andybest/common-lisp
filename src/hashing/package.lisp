(in-package :defpackage+-user-1)

(defpackage+ #:umbra.hash
  (:use #:umbra
        #:umbra.swizzle)
  (:export #:blum-blum-shub
           #:blum-blum-shub/hq
           #:sgpp
           #:sgpp/2-per-corner
           #:sgpp/3-per-corner
           #:fast32
           #:fast32/2-per-corner
           #:fast32/3-per-corner
           #:fast32/4-per-corner
           #:fast32/cell
           #:fast32-2
           #:fast32-2/4-per-corner))
