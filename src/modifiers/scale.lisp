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
  (x 1d0 :type u:f64)
  (y 1d0 :type u:f64)
  (z 1d0 :type u:f64)
  (w 1d0 :type u:f64))

(defun mod:scale (source &key (x 1.0) (y 1.0) (z 1.0) (w 1.0))
  (unless (typep source 'int:sampler)
    (error 'int:invalid-sampler-argument
           :sampler-type 'scale
           :argument 'source
           :value source))
  (unless (realp x)
    (error 'int:invalid-real-argument
           :sampler-type 'scale
           :argument :x
           :value x))
  (unless (realp y)
    (error 'int:invalid-real-argument
           :sampler-type 'scale
           :argument :y
           :value y))
  (unless (realp z)
    (error 'int:invalid-real-argument
           :sampler-type 'scale
           :argument :z
           :value z))
  (unless (realp w)
    (error 'int:invalid-real-argument
           :sampler-type 'scale
           :argument :w
           :value w))
  (make-scale :rng (int::sampler-rng source)
              :source source
              :x (float x 1d0)
              :y (float y 1d0)
              :z (float z 1d0)
              :w (float w 1d0)))

(defun mod:uniform-scale (source scalar)
  (unless (typep source 'int:sampler)
    (error 'int:invalid-sampler-argument
           :sampler-type 'uniform-scale
           :argument 'source
           :value source))
  (unless (realp scalar)
    (error 'int:invalid-real-argument
           :sampler-type 'scale
           :argument 'scalar
           :value scalar))
  (let ((scalar (float scalar 1d0)))
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
