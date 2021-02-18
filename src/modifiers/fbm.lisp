(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers.fbm
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:mod #:coherent-noise.modifiers)
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:coherent-noise.modifiers.fbm)

(defstruct (fbm
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source nil :type int:sampler)
  (scale 1.0 :type u:f32)
  (octaves 4 :type (integer 1 32))
  (frequency 1.0 :type u:f32)
  (lacunarity int::+default-lacunarity+ :type u:f32)
  (persistence 0.5 :type u:f32))

(defun mod:fbm (source &key (octaves 4) (frequency 1.0) (lacunarity int::+default-lacunarity+)
                         (persistence 0.5))
  (make-fbm :rng (int::sampler-rng source)
            :source source
            :scale (int::calculate-fractal-scaling-factor octaves persistence)
            :octaves octaves
            :frequency (float frequency 1f0)
            :lacunarity (float lacunarity 1f0)
            :persistence (float persistence 1f0)))

(defmethod int:sample ((sampler fbm) x &optional (y 0d0) (z 0d0) (w 0d0))
  (loop :with source = (source sampler)
        :with frequency = (frequency sampler)
        :with lacunarity = (lacunarity sampler)
        :with persistence = (persistence sampler)
        :repeat (octaves sampler)
        :for amplitude = 1.0 :then (* amplitude persistence)
        :for fx = (* x frequency) :then (* fx lacunarity)
        :for fy = (* y frequency) :then (* fy lacunarity)
        :for fz = (* z frequency) :then (* fz lacunarity)
        :for fw = (* w frequency) :then (* fw lacunarity)
        :for sample = (int:sample source fx fy fz fw)
        :sum (* sample amplitude) :into result
        :finally (return (/ result (scale sampler)))))
