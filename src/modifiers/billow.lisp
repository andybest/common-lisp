(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers.billow
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:mod #:coherent-noise.modifiers)
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:coherent-noise.modifiers.billow)

(defstruct (billow
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source nil :type int:sampler)
  (octaves 4 :type (integer 1 32))
  (frequency 1f0 :type u:f32)
  (gain 0.5 :type u:f32)
  (lacunarity 2.0 :type u:f32))

(defun mod:billow (source &key (octaves 4) (frequency 1.0) (gain 0.5) (lacunarity 2.0))
  (make-billow :rng (int::sampler-rng source)
               :source source
               :octaves octaves
               :frequency (float frequency 1f0)
               :gain (float gain 1f0)
               :lacunarity (float lacunarity 1f0)))

(defmethod int:sample ((sampler billow) x &optional (y 0d0) (z 0d0) (w 0d0))
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
        :for sample = (1- (* (abs (int:sample source fx fy fz fw)) 2))
        :sum (* sample amplitude) :into value
        :finally (return (+ value 0.5))))
