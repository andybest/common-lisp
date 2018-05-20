(in-package :defpackage+-user-1)

(defpackage+ #:umbra.common
  (:inherit #:cl #:shadow #:box.math.vari))

(defpackage+ #:umbra.math
  (:use #:umbra.common)
  (:export #:+epsilon+
           #:+pi+
           #:+half-pi+
           #:log10
           #:saturate))

(defpackage+ #:umbra.color
  (:use #:umbra.common #:umbra.math)
  (:local-nicknames (#:v2 #:box.math.vec2)
                    (#:v3 #:box.math.vec3)
                    (#:v4 #:box.math.vec4)
                    (#:m3 #:box.math.mat3))
  ;; color space conversion
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
           #:xyy->rgb)
  ;; color grading
  (:export #:set-exposure
           #:set-saturation
           #:set-contrast
           #:set-brightness
           #:set-gamma
           #:color-filter
           #:tone-map-linear
           #:tone-map-reinhard
           #:tone-map-haarm-peter-duiker
           #:tone-map-hejl-burgess-dawson
           #:tone-map-uncharted2))

(defpackage+ #:umbra.easing
  (:use #:umbra.common #:umbra.math)
  (:export #:linear
           #:sine-out
           #:sine-in
           #:sine-in-out
           #:quadratic-out
           #:quadratic-in
           #:quadratic-in-out
           #:cubic-out
           #:cubic-in
           #:cubic-in-out
           #:quartic-out
           #:quartic-in
           #:quartic-in-out
           #:quintic-out
           #:quintic-in
           #:quintic-in-out
           #:exponential-out
           #:exponential-in
           #:exponential-in-out
           #:circular-out
           #:circular-in
           #:circular-in-out
           #:back-out
           #:back-in
           #:back-in-out
           #:elastic-out
           #:elastic-in
           #:elastic-in-out
           #:bounce-out
           #:bounce-in
           #:bounce-in-out))

(defpackage+ #:umbra.graph
  (:use #:umbra.common)
  (:local-nicknames (#:v2 #:box.math.vec2)
                    (#:v4 #:box.math.vec4))
  (:export #:graph))
