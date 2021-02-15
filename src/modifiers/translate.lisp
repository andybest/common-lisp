(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers.translate
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:translate))

(in-package #:coherent-noise.modifiers.translate)

(defstruct (translate
            (:include int::sampler)
            (:constructor %translate)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (source nil :type int::sampler)
  (x 0d0 :type u:f64)
  (y 0d0 :type u:f64)
  (z 0d0 :type u:f64)
  (w 0d0 :type u:f64))

(defun translate (source &key (x 0d0) (y 0d0) (z 0d0) (w 0d0))
  (%translate :rng (int::sampler-rng source)
              :source source
              :x x
              :y y
              :z z
              :w w))

(defmethod int::sample ((sampler translate) x &optional (y 0d0) (z 0d0) (w 0d0))
  (int::sample (source sampler)
               (+ x (x sampler))
               (+ y (y sampler))
               (+ z (z sampler))
               (+ w (w sampler))))
