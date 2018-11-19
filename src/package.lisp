(in-package :defpackage+-user-1)

(defpackage+ #:umbra
  (:inherit #:shadow.vari)
  (:export #:mvlet*))

(defpackage+ #:umbra.math
  (:use #:umbra)
  (:export #:+epsilon+
           #:+pi+
           #:+half-pi+
           #:log10
           #:saturate
           #:map-domain))

(defpackage+ #:umbra.graph
  (:use #:umbra)
  (:export #:graph))

(defpackage+ #:umbra.color
  (:use #:umbra #:umbra.math)
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
           #:tone-map/linear
           #:tone-map/reinhard
           #:tone-map/haarm-peter-duiker
           #:tone-map/hejl-burgess-dawson
           #:tone-map/uncharted2))

(defpackage+ #:umbra.shaping
  (:use #:umbra #:umbra.math)
  ;; penner
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
           #:bounce-in-out)
  ;; iq
  (:export #:almost-identity
           #:impulse
           #:cubic-pulse
           #:exponential-step
           #:gain
           #:parabola
           #:power-curve
           #:sinc-curve)
  ;; levin
  (:export #:exponential-emphasis
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
  (:export #:hermite-curve
           #:quintic-curve
           #:quintic-curve/interpolate-derivative
           #:quintic-curve/derivative
           #:quintic-curve/fast
           #:quintic-hermite
           #:quintic-hermite/derivative
           #:falloff-squared-c1
           #:falloff-squared-c2))

(defpackage+ #:umbra.hashing
  (:use #:umbra)
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

(defpackage+ #:umbra.noise
  (:use #:umbra #:umbra.math)
  (:export #:perlin
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
