(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers.billow
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:billow))

(in-package #:coherent-noise.modifiers.billow)

(defstruct (billow
            (:include int::sampler)
            (:constructor %billow)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source nil :type int::sampler)
  (octaves 4 :type (integer 1 32))
  (frequency 1d0 :type u:f64)
  (gain 0.5d0 :type u:f64)
  (lacunarity 2d0 :type u:f64))

(defun billow (source &key (octaves 4) (frequency 1d0) (gain 0.5d0) (lacunarity 2d0))
  (%billow :rng (int::sampler-rng source)
           :source source
           :octaves octaves
           :frequency frequency
           :gain gain
           :lacunarity lacunarity))

(defmethod int::sample ((sampler billow) x &optional (y 0d0) (z 0d0) (w 0d0))
  (loop :with source = (source sampler)
        :with frequency = (frequency sampler)
        :with gain = (gain sampler)
        :with lacunarity = (lacunarity sampler)
        :repeat (octaves sampler)
        :for amplitude = 1.0 :then (* amplitude gain)
        :for fx = (* x frequency) :then (* fx lacunarity)
        :for fy = (* y frequency) :then (* fy lacunarity)
        :for fz = (* z frequency) :then (* fz lacunarity)
        :for fw = (* w frequency) :then (* fw lacunarity)
        :for sample = (1- (* (abs (int::sample source fx fy fz fw)) 2))
        :sum (* sample amplitude) :into value
        :finally (return (+ value 0.5))))
