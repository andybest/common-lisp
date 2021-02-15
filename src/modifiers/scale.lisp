(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers.scale
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:scale
   #:uniform-scale))

(in-package #:coherent-noise.modifiers.scale)

(defstruct (scale
            (:include int::sampler)
            (:constructor %scale)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (source nil :type int::sampler)
  (x 1d0 :type u:f64)
  (y 1d0 :type u:f64)
  (z 1d0 :type u:f64)
  (w 1d0 :type u:f64))

(defun scale (source &key (x 1d0) (y 1d0) (z 1d0) (w 1d0))
  (%scale :rng (int::sampler-rng source)
          :source source
          :x x
          :y y
          :z z
          :w w))

(defun uniform-scale (source scalar)
  (%scale :rng (int::sampler-rng source)
          :source source
          :x scalar
          :y scalar
          :z scalar
          :w scalar))

(defmethod int::sample ((sampler scale) x &optional (y 0d0) (z 0d0) (w 0d0))
  (int::sample (source sampler)
               (/ x (x sampler))
               (/ y (y sampler))
               (/ z (z sampler))
               (/ w (w sampler))))
