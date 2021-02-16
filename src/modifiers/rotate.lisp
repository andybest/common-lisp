(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers.rotate
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:mod #:coherent-noise.modifiers)
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:coherent-noise.modifiers.rotate)

(defstruct (rotate
            (:include int::sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source nil :type int::sampler)
  (rx1 0.0 :type u:f32)
  (rx2 0.0 :type u:f32)
  (rx3 0.0 :type u:f32)
  (ry1 0.0 :type u:f32)
  (ry2 0.0 :type u:f32)
  (ry3 0.0 :type u:f32)
  (rz1 0.0 :type u:f32)
  (rz2 0.0 :type u:f32)
  (rz3 0.0 :type u:f32))

(defun mod:rotate (source &key (x 0.0) (y 0.0) (z 0.0))
  (let ((cx (float (cos x) 1f0))
        (cy (float (cos y) 1f0))
        (cz (float (cos z) 1f0))
        (sx (float (sin x) 1f0))
        (sy (float (sin y) 1f0))
        (sz (float (sin z) 1f0)))
    (make-rotate :rng (int::sampler-rng source)
                 :source source
                 :rx1 (+ (* sx sy sz) (* cy cz))
                 :rx2 (- (* sx sy cz) (* cy sz))
                 :rx3 (* (- sy) cx)
                 :ry1 (* cy sz)
                 :ry2 (* cx cz)
                 :ry3 sx
                 :rz1 (- (* sy cz) (* cy sy sz))
                 :rz2 (- (* (- cy) sx cz) (* sy sz))
                 :rz3 (* cy cx))))

(defmethod int:sample ((sampler rotate) x &optional (y 0d0) (z 0d0) (w 0d0))
  (let ((x (+ (* x (rx1 sampler)) (* y (ry1 sampler)) (* z (rz1 sampler))))
        (y (+ (* x (rx2 sampler)) (* y (ry2 sampler)) (* z (rz2 sampler))))
        (z (+ (* x (rx3 sampler)) (* y (ry3 sampler)) (* z (rz3 sampler)))))
    (int:sample (source sampler) x y z w)))
