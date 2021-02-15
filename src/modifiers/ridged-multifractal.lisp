(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers.ridged-multifractal
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:ridged-multifractal))

(in-package #:coherent-noise.modifiers.ridged-multifractal)

(defstruct (ridged-multifractal
            (:include int::sampler)
            (:constructor %ridged-multifractal)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source nil :type int::sampler)
  (octaves 4 :type (integer 1 32))
  (frequency 1d0 :type u:f64)
  (gain 2d0 :type u:f64)
  (lacunarity 2d0 :type u:f64)
  (exponent 1d0 :type u:f64)
  (offset 1d0 :type u:f64)
  (weights (make-array 0 :element-type 'u:f64) :type (simple-array u:f64 (*))))

(defun ridged-multifractal (source &key (octaves 4) (frequency 1d0) (gain 2d0) (lacunarity 2d0)
                                     (exponent 1d0) (offset 1d0))
  (let ((weights (make-array octaves :element-type 'u:f64)))
    (loop :for i :below octaves
          :for frequency = 1d0 :then (* frequency lacunarity)
          :do (setf (aref weights i) (expt frequency (- exponent))))
    (%ridged-multifractal :rng (int::sampler-rng source)
                          :source source
                          :octaves octaves
                          :frequency frequency
                          :gain gain
                          :lacunarity lacunarity
                          :exponent exponent
                          :offset offset
                          :weights weights)))

(defmethod int::sample ((sampler ridged-multifractal) x &optional (y 0d0) (z 0d0) (w 0d0))
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
        :for sample = (* (expt (- offset (abs (int::sample source fx fy fz fw))) 2) weight)
        :sum (* sample (aref weights i)) :into value
        :finally (return (1- (* value 1.25)))))
