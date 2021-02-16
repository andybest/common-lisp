(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers.ridged-multi
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:mod #:coherent-noise.modifiers)
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:coherent-noise.modifiers.ridged-multi)

(defstruct (ridged-multi
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source nil :type int:sampler)
  (octaves 4 :type (integer 1 32))
  (frequency 1.0 :type u:f32)
  (gain 2.0 :type u:f32)
  (lacunarity 2.0 :type u:f32)
  (offset 1.0 :type u:f32)
  (weights (make-array 0 :element-type 'u:f32) :type (simple-array u:f32 (*))))

(defun mod:ridged-multi (source &key (octaves 4) (frequency 1.0) (gain 2.0) (lacunarity 2.0)
                                  (exponent 1.0) (offset 1.0))
  (let ((weights (make-array octaves :element-type 'u:f32))
        (frequency (float frequency 1.0))
        (exponent (float exponent 1.0)))
    (loop :for i :below octaves
          :for frequency = 1.0 :then (* frequency lacunarity)
          :do (setf (aref weights i) (expt frequency (- exponent))))
    (make-ridged-multi :rng (int::sampler-rng source)
                       :source source
                       :octaves octaves
                       :frequency frequency
                       :gain (float gain 1f0)
                       :lacunarity (float lacunarity 1f0)
                       :offset (float offset 1f0)
                       :weights weights)))

(defmethod int:sample ((sampler ridged-multi) x &optional (y 0d0) (z 0d0) (w 0d0))
  (loop :with source = (source sampler)
        :with frequency = (frequency sampler)
        :with gain = (gain sampler)
        :with lacunarity = (lacunarity sampler)
        :with offset = (offset sampler)
        :with weights = (weights sampler)
        :for i :below (octaves sampler)
        :for fx = (* x frequency) :then (* fx lacunarity)
        :for fy = (* y frequency) :then (* fy lacunarity)
        :for fz = (* z frequency) :then (* fz lacunarity)
        :for fw = (* w frequency) :then (* fw lacunarity)
        :for weight = 1.0 :then (u:clamp (* sample gain) 0.0 1.0)
        :for sample = (* (expt (- offset (abs (int:sample source fx fy fz fw))) 2) weight)
        :sum (* sample (aref weights i)) :into value
        :finally (return (1- (* value 1.25)))))
