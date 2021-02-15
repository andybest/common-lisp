(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers.ridged
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:ridged))

(in-package #:coherent-noise.modifiers.ridged)

(defstruct (ridged
            (:include int::sampler)
            (:constructor %ridged)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source nil :type int::sampler)
  (octaves 4 :type (integer 1 32))
  (frequency 1d0 :type u:f64)
  (gain 0.5d0 :type u:f64)
  (lacunarity 2d0 :type u:f64))

(defun ridged (source &key (octaves 4) (frequency 1d0) (gain 0.5d0) (lacunarity 2d0))
  (%ridged :rng (int::sampler-rng source)
           :source source
           :octaves octaves
           :frequency frequency
           :gain gain
           :lacunarity lacunarity))

(defmethod int::sample ((sampler ridged) x &optional (y 0d0) (z 0d0) (w 0d0))
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
        :for sample = (1+ (* (abs (int::sample source fx fy fz fw)) -2))
        :sum (* sample amplitude) :into value
        :finally (return (1- (* value 1.25)))))
