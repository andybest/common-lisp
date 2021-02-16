(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers
  (:shadow #:+ #:- #:* #:/ #:abs #:expt #:max #:min)
  (:export
   #:+
   #:-
   #:*
   #:/
   #:abs
   #:billow
   #:blend
   #:clamp
   #:displace
   #:expt
   #:fractal
   #:invert
   #:max
   #:min
   #:power
   #:ridged
   #:ridged-multifractal
   #:rotate
   #:scale
   #:select
   #:strengthen
   #:translate
   #:turbulence
   #:uniform-scale))
