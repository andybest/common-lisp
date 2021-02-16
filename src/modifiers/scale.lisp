(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers.scale
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:mod #:coherent-noise.modifiers)
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:coherent-noise.modifiers.scale)

(defstruct (scale
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source nil :type int:sampler)
  (x 1.0 :type u:f32)
  (y 1.0 :type u:f32)
  (z 1.0 :type u:f32)
  (w 1.0 :type u:f32))

(defun mod:scale (source &key (x 1.0) (y 1.0) (z 1.0) (w 1.0))
  (make-scale :rng (int::sampler-rng source)
              :source source
              :x (float x 1f0)
              :y (float y 1f0)
              :z (float z 1f0)
              :w (float w 1f0)))

(defun mod:uniform-scale (source scalar)
  (let ((scalar (float scalar 1f0)))
    (make-scale :rng (int::sampler-rng source)
                :source source
                :x scalar
                :y scalar
                :z scalar
                :w scalar)))

(defmethod int:sample ((sampler scale) x &optional (y 0d0) (z 0d0) (w 0d0))
  (int:sample (source sampler)
              (/ x (x sampler))
              (/ y (y sampler))
              (/ z (z sampler))
              (/ w (w sampler))))
