(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers.select
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow #:min #:max)
  (:export
   #:select))

(in-package #:coherent-noise.modifiers.select)

(defstruct (select
            (:include int::sampler)
            (:constructor %select)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (source1 nil :type int::sampler)
  (source2 nil :type int::sampler)
  (control nil :type int::sampler)
  (min -1d0 :type u:f64)
  (max 1d0 :type u:f64)
  (falloff 0d0 :type u:f64))

(defun select (source1 source2 control &key (min -1d0) (max 1d0) (falloff 0d0))
  (%select :rng (int::sampler-rng source1)
           :source1 source1
           :source2 source2
           :control control
           :min min
           :max max
           :falloff falloff))

(defmethod int::sample ((sampler select) x &optional (y 0d0) (z 0d0) (w 0d0))
  (let* ((min (min sampler))
         (max (max sampler))
         (falloff (cl:min (falloff sampler) (* (- max min) 0.5)))
         (sample1 (int::sample (source1 sampler) x y z w))
         (sample2 (int::sample (source2 sampler) x y z w))
         (control (int::sample (control sampler) x y z w)))
    (if (plusp falloff)
        (cond
          ((< control (- min falloff))
           sample1)
          ((< control (+ min falloff))
           (let* ((low (- min falloff))
                  (high (+ min falloff))
                  (alpha (int::interpolate/cubic (/ (- control low) (- high low)))))
             (u:lerp alpha sample1 sample2)))
          ((< control (- max falloff))
           sample2)
          ((< control (+ max falloff))
           (let* ((low (- max falloff))
                  (high (+ max falloff))
                  (alpha (int::interpolate/cubic (/ (- control low) (- high low)))))
             (u:lerp alpha sample2 sample1)))
          (t
           sample1))
        (if (or (< control min) (> control max))
            sample1
            sample2))))
