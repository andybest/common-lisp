(in-package #:defpackage+-user-1)

(defpackage+ #:umbra
  (:inherit-from #:shadow
                 #:define-function
                 #:define-struct
                 #:define-macro
                 #:define-shader)
  (:inherit #:cl
            #:vari)
  ;; structs
  (:export #:mesh-attrs
           #:mesh/pos
           #:mesh/normal
           #:mesh/tangent
           #:mesh/color
           #:mesh/uv1
           #:mesh/uv2
           #:mesh/joints
           #:mesh/weights)
  ;; utilities
  (:export #:mvlet*)
  ;; math
  (:export #:+epsilon+
           #:+pi+
           #:+half-pi+
           #:log10
           #:saturate
           #:map-domain))

(defpackage+ #:umbra.swizzle
  (:local-nicknames (#:u #:golden-utils))
  (:use #:cl #:vari))

(defpackage+ #:umbra.user
  (:use #:umbra.swizzle)
  (:inherit #:umbra))
