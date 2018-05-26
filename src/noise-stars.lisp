(in-package :umbra.noise)

;;;; Noise functions
;;;; Stars noise

(defun-gpu stars ((p :vec2)
                  (probability-threshold :float)
                  (max-dimness :float)
                  (radius :float)
                  (hash-fn (function (:vec2) :vec4)))
  (let* ((pi (floor p))
         (pf (- p pi))
         (hash (funcall hash-fn pi))
         (value (- 1 (* max-dimness (.z hash)))))
    (multf pf (vec2 radius))
    (decf pf (vec2 (1- radius)))
    (incf pf  (* (.xy hash) (- radius 2)))
    (if (< (.w hash) probability-threshold)
        (* (umbra.shaping:falloff-squared-c1 (min (dot pf pf) 1.0)) value)
        0.0)))

(defun-gpu stars ((p :vec2)
                  (probability-threshold :float)
                  (max-dimness :float)
                  (radius :float))
  (stars p probability-threshold max-dimness radius
         (lambda ((x :vec2)) (umbra.hashing:fast32/cell x))))
