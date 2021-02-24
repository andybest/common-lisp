(in-package #:cl-user)

(defpackage #:%coherent-noise.generators.simplex-1d
  (:local-nicknames
   (#:gen #:%coherent-noise.generators)
   (#:int #:%coherent-noise.internal)
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:%coherent-noise.generators.simplex-1d)

(u:define-constant +scale+ 0.395d0)

(defstruct (gen:simplex-1d
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (table nil :type (simple-array u:ub8 (512))))

(defun gen:simplex-1d (&key seed)
  (let ((rng (int::make-rng seed)))
    (make-simplex-1d :rng rng :table (int::perlin-permute rng))))

(defmethod int:sample ((sampler gen:simplex-1d) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (ignore y z w)
           (optimize speed)
           (int::f50 x y z w))
  (flet ((grad (hash x)
           (let* ((s (- 1 (* x x)))
                  (h (logand hash 15))
                  (grad (if (zerop (logand h 8))
                            (* (1+ (logand h 7)) x)
                            (* (- (1+ (logand h 7))) x))))
             (if (plusp s)
                 (* s s s s grad)
                 0d0))))
    (declare (inline grad))
    (let* ((table (table sampler))
           (i1 (floor x))
           (i2 (1+ i1))
           (x1 (- x i1))
           (x2 (1- x1)))
      (float (* (+ (grad (int::lookup table i1) x1)
                   (grad (int::lookup table i2) x2))
                +scale+)
             1f0))))
