(in-package #:cl-user)

(defpackage #:%coherent-noise.modifiers.displace
  (:local-nicknames
   (#:int #:%coherent-noise.internal)
   (#:mod #:%coherent-noise.modifiers)
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:%coherent-noise.modifiers.displace)

(defstruct (displace
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source nil :type int:sampler)
  (x nil :type (or int:sampler null))
  (y nil :type (or int:sampler null))
  (z nil :type (or int:sampler null))
  (w nil :type (or int:sampler null)))

(defun mod:displace (source &key x y z w)
  (unless (typep source 'int:sampler)
    (error 'int:invalid-sampler-argument
           :sampler-type 'displace
           :argument 'source
           :value source))
  (unless (typep x '(or int:sampler null))
    (error 'int:invalid-sampler-argument
           :sampler-type 'displace
           :argument :x
           :value x))
  (unless (typep y '(or int:sampler null))
    (error 'int:invalid-sampler-argument
           :sampler-type 'displace
           :argument :y
           :value y))
  (unless (typep z '(or int:sampler null))
    (error 'int:invalid-sampler-argument
           :sampler-type 'displace
           :argument :z
           :value z))
  (unless (typep w '(or int:sampler null))
    (error 'int:invalid-sampler-argument
           :sampler-type 'displace
           :argument :w
           :value w))
  (make-displace :rng (int::sampler-rng source) :source source :x x :y y :z z :w w))

(defmethod int:sample ((sampler displace) x &optional (y 0d0) (z 0d0) (w 0d0))
  (let* ((dx (if (x sampler) (int:sample (x sampler) x y z w) 0.0))
         (dy (if (y sampler) (int:sample (y sampler) x y z w) 0.0))
         (dz (if (z sampler) (int:sample (z sampler) x y z w) 0.0))
         (dw (if (w sampler) (int:sample (w sampler) x y z w) 0.0)))
    (int:sample (source sampler) (+ x dx) (+ y dy) (+ z dz) (+ w dw))))
