(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers.cache
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:mod #:coherent-noise.modifiers)
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:coherent-noise.modifiers.cache)

(defstruct (cache
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source nil :type int:sampler)
  (cached-p nil :type boolean)
  (value 0f0 :type u:f32)
  (x 0d0 :type u:f64)
  (y 0d0 :type u:f64)
  (z 0d0 :type u:f64)
  (w 0d0 :type u:f64))

(defun mod:cache (source)
  (make-cache :rng (int::sampler-rng source) :source source))

(defmethod int:sample ((sampler cache) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (optimize speed)
           (u:f64 x y z w))
  (unless (and (cached-p sampler)
               (= x (x sampler))
               (= y (y sampler))
               (= z (z sampler))
               (= w (w sampler)))
    (setf (cached-p sampler) t
          (x sampler) x
          (y sampler) y
          (z sampler) z
          (w sampler) w
          (value sampler) (int:sample (source sampler) x y z w)))
  (value sampler))
