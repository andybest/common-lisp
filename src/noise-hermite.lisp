(in-package :umbra.noise)

;;;; Noise functions
;;;; Hermite noise

(defun-gpu hermite ((p :vec2)
                    (hash-fn (function (:vec2) (:vec4 :vec4))))
  (let* ((pi (floor p))
         (pf (- p pi)))
    (multiple-value-bind (x y) (funcall hash-fn pi)
      (setf x (- x 0.49999)
            y (- y 0.49999))
      (let ((norm (inversesqrt (+ (* x x) (* y y)))))
        (multf x norm)
        (multf y norm)
        (let ((res (umbra.shaping:quintic-hermite (.y pf) (.xy x) (.zw x) (.xy y) (.zw y))))
          (map-domain
           (* (umbra.shaping:quintic-hermite (.x pf) (.x res) (.y res) (.z res) (.w res))
              2.2627418)
           -1 1 0 1))))))

(defun-gpu hermite ((p :vec2))
  (hermite p (lambda ((x :vec2)) (umbra.hashing:fast32/2-per-corner x))))
