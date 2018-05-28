(in-package :umbra.noise)

;;;; Noise functions
;;;; Stars noise

(defun-gpu stars ((point :vec2)
                  (probability-threshold :float)
                  (max-dimness :float)
                  (radius :float)
                  (hash-fn (function (:vec2) :vec4)))
  (let* ((cell (floor point))
         (vec (- point cell))
         (hash (funcall hash-fn cell))
         (value (- 1 (* max-dimness (.z hash)))))
    (multf vec (vec2 radius))
    (decf vec (vec2 (1- radius)))
    (incf vec (* (.xy hash) (- radius 2)))
    (if (< (.w hash) probability-threshold)
        (* (umbra.shaping:falloff-squared-c1 (min (dot vec vec) 1)) value)
        0.0)))

(defun-gpu stars ((point :vec2)
                  (probability-threshold :float)
                  (max-dimness :float)
                  (radius :float))
  (stars point probability-threshold max-dimness radius
         (lambda ((x :vec2)) (umbra.hashing:fast32/cell x))))
