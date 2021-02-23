(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers.translate
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:mod #:coherent-noise.modifiers)
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:coherent-noise.modifiers.translate)

(defstruct (translate
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source nil :type int:sampler)
  (x 0.0 :type u:f32)
  (y 0.0 :type u:f32)
  (z 0.0 :type u:f32)
  (w 0.0 :type u:f32))

(defun mod:translate (source &key (x 0.0) (y 0.0) (z 0.0) (w 0.0))
  (unless (typep source 'int:sampler)
    (error 'int:invalid-sampler-argument
           :sampler-type 'translate
           :argument 'source
           :value source))
  (unless (realp x)
    (error 'int:invalid-real-argument
           :sampler-type 'translate
           :argument :x
           :value x))
  (unless (realp y)
    (error 'int:invalid-real-argument
           :sampler-type 'translate
           :argument :y
           :value y))
  (unless (realp z)
    (error 'int:invalid-real-argument
           :sampler-type 'translate
           :argument :z
           :value z))
  (unless (realp w)
    (error 'int:invalid-real-argument
           :sampler-type 'translate
           :argument :w
           :value w))
  (make-translate :rng (int::sampler-rng source)
                  :source source
                  :x (float x 1f0)
                  :y (float y 1f0)
                  :z (float z 1f0)
                  :w (float w 1f0)))

(defmethod int:sample ((sampler translate) x &optional (y 0d0) (z 0d0) (w 0d0))
  (int:sample (source sampler)
              (+ x (x sampler))
              (+ y (y sampler))
              (+ z (z sampler))
              (+ w (w sampler))))
