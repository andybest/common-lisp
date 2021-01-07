(in-package #:cl-user)

(defpackage #:umbra.common
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:shadow.glsl)
  ;; structs
  (:export
   #:mesh-attrs
   #:mesh/pos
   #:mesh/normal
   #:mesh/tangent
   #:mesh/color
   #:mesh/uv1
   #:mesh/uv2
   #:mesh/joints
   #:mesh/weights)
  ;; utilities
  (:export
   #:mvlet*)
  ;; math
  (:export
   #:+epsilon+
   #:+pi+
   #:+half-pi+
   #:log10
   #:saturate
   #:map-domain))

(defpackage #:umbra.shaping
  (:use
   #:shadow.glsl
   #:umbra.common)
  ;; penner
  (:export
   #:linear
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
   #:bounce-in-out)
  ;; iq
  (:export
   #:almost-identity
   #:impulse
   #:cubic-pulse
   #:exponential-step
   #:gain
   #:parabola
   #:power-curve
   #:sinc-curve)
  ;; levin
  (:export
   #:exponential-emphasis
   #:double-exponential-seat
   #:double-exponential-sigmoid
   #:logistic-sigmoid
   #:double-circle-seat
   #:double-circle-sigmoid
   #:double-elliptical-seat
   #:double-elliptical-sigmoid
   #:blinn-wyvill-raised-inverted-cosine
   #:double-cubic-seat
   #:double-cubic-seat/linear-blend
   #:double-odd-polynomial-seat
   #:quadratic-point)
  ;; misc
  (:export
   #:hermite-curve
   #:quintic-curve
   #:quintic-curve/interpolate-derivative
   #:quintic-curve/derivative
   #:quintic-curve/fast
   #:quintic-hermite
   #:quintic-hermite/derivative
   #:falloff-squared-c1
   #:falloff-squared-c2))

(defpackage #:umbra.color
  (:use
   #:shadow.glsl
   #:umbra.common)
  ;; color space conversion
  (:export
   #:rgb->grayscale
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
  (:export
   #:set-exposure
   #:set-saturation
   #:set-contrast
   #:set-brightness
   #:set-gamma
   #:color-filter
   #:tone-map/linear
   #:tone-map/reinhard
   #:tone-map/haarm-peter-duiker
   #:tone-map/hejl-burgess-dawson
   #:tone-map/uncharted2
   #:tone-map/aces))

(defpackage #:umbra.effects
  (:use #:shadow.glsl
        #:umbra.common)
  (:export
   #:window-rain))

(defpackage #:umbra.graphing
  (:use
   #:shadow.glsl
   #:umbra.common)
  (:export
   #:graph))

(defpackage #:umbra.hashing
  (:use
   #:shadow.glsl
   #:umbra.common)
  (:export
   #:blum-blum-shub
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

(defpackage #:umbra.noise
  (:local-nicknames
   (#:hash #:umbra.hashing)
   (#:shape #:umbra.shaping))
  (:use
   #:shadow.glsl
   #:umbra.common)
  (:export
   #:billow/cellular
   #:billow/perlin
   #:billow/simplex
   #:fbm/cellular
   #:fbm/perlin
   #:fbm/simplex
   #:perlin
   #:perlin/derivs
   #:perlin-surflet
   #:perlin-surflet/derivs
   #:perlin-improved
   #:cellular
   #:cellular/derivs
   #:cellular-fast
   #:polkadot
   #:polkadot-box
   #:hermite
   #:hermite/derivs
   #:ridges/cellular
   #:ridges/perlin
   #:ridges/simplex
   #:simplex-perlin
   #:simplex-perlin/derivs
   #:simplex-cellular
   #:simplex-polkadot
   #:value
   #:value/derivs
   #:value-perlin
   #:value-hermite
   #:cubist
   #:stars))

(defpackage #:umbra.sdf
  (:use
   #:shadow.glsl
   #:umbra.common)
  (:export
   #:difference
   #:difference/smooth
   #:dist/arc
   #:dist/box
   #:dist/circle
   #:dist/cross
   #:dist/egg
   #:dist/hexagon
   #:dist/line
   #:dist/parabola
   #:dist/pentagon
   #:dist/pie
   #:dist/rhombus
   #:dist/rounded-x
   #:dist/segment
   #:dist/semi-circle
   #:dist/star
   #:dist/star5
   #:dist/star6
   #:dist/trapezoid
   #:dist/triangle
   #:dist/uneven-capsule
   #:dist/vesica
   #:intersect
   #:intersect/smooth
   #:mask/fill
   #:mask/inner-border
   #:mask/outer-border
   #:shadow
   #:onion
   #:union
   #:union/smooth))

(defpackage #:umbra.sprite
  (:use
   #:shadow.glsl
   #:umbra.common)
  (:export
   #:sprite))
