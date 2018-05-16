(in-package :defpackage+-user-1)

(defpackage+ #:umbra.math
  (:use #:shadow)
  (:export #:saturate))

(defpackage+ #:umbra.color
  (:use #:shadow #:box.math.vari)
  (:local-nicknames (#:v3 #:box.math.vec3)
                    (#:v4 #:box.math.vec4)
                    (#:m3 #:box.math.mat3))
  (:export #:rgb->grayscale
           #:hue->rgb
           #:rgb->hcv
           #:rgb->hsv
           #:hsv->rgb
           #:rgb->hcy
           #:hcy->rgb
           #:rgb->hsl
           #:hsl-rgb
           #:rgb->srgb-approx
           #:rgb->srgb
           #:srgb->rgb-approx
           #:srgb->rgb
           #:rgb->xyz
           #:xyz->rgb
           #:xyy->xyz
           #:xyz->xyy
           #:rgb->xyy
           #:xyy->rgb))

(defpackage+ #:umbra.easing
  (:use #:shadow))
