(in-package #:cl-user)

(defpackage #:coherent-noise.generators.hybrid-multifractal-3d
  (:local-nicknames
   (#:gen #:coherent-noise.generators)
   (#:int #:coherent-noise.internal)
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:coherent-noise.generators.hybrid-multifractal-3d)

(defstruct (hybrid-multifractal-3d
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (sources (vector) :type simple-vector)
  (scale 1.0 :type u:f32)
  (octaves 4 :type (integer 1 32))
  (frequency 1.0 :type u:f32)
  (lacunarity int::+default-lacunarity+ :type u:f32)
  (persistence 0.25 :type u:f32))

(defun get-scale-factor (octaves persistence)
  (loop :with persistence = persistence
        :with result = persistence
        :for i :from 1 :below octaves
        :for amplitude = persistence :then (* amplitude persistence)
        :for weight = result :then (max weight 1)
        :for sample = amplitude
        :do (setf weight (* weight sample))
            (incf result weight)
        :finally (return result)))

(defun gen:hybrid-multifractal-3d (&key seed (generator #'gen:perlin-3d) (octaves 4) (frequency 1.0)
                                     (lacunarity int::+default-lacunarity+) (persistence 0.25))
  (let ((rng (int::make-rng seed)))
    (make-hybrid-multifractal-3d :rng rng
                                 :sources (int::make-fractal-sources generator rng octaves)
                                 :scale (get-scale-factor octaves persistence)
                                 :octaves octaves
                                 :frequency (float frequency 1f0)
                                 :lacunarity (float lacunarity 1f0)
                                 :persistence (float persistence 1f0))))

(defmethod int:sample ((sampler hybrid-multifractal-3d) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (ignore w))
  (loop :with sources = (sources sampler)
        :with frequency = (frequency sampler)
        :with lacunarity = (lacunarity sampler)
        :with persistence = (persistence sampler)
        :with fx = (* x frequency)
        :with fy = (* y frequency)
        :with fz = (* z frequency)
        :with result = (* (int:sample (aref sources 0) fx fy fz) persistence)
        :for i :from 1 :below (octaves sampler)
        :for amplitude = persistence :then (* amplitude persistence)
        :for weight = result :then (max weight 1)
        :for lx = (* fx lacunarity)
        :for ly = (* fy lacunarity)
        :for lz = (* fz lacunarity)
        :for sample = (* (int:sample (aref sources i) lx ly lz) amplitude)
        :do (setf weight (* weight sample))
            (incf result weight)
        :finally (return (/ result (scale sampler)))))
