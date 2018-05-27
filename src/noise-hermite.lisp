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

(defun-gpu hermite ((p :vec3)
                    (hash-fn (function (:vec3) (:vec4 :vec4 :vec4 :vec4 :vec4 :vec4))))
  (let* ((pi (floor p))
         (pf (- p pi)))
    (multiple-value-bind (x0 y0 z0 x1 y1 z1) (funcall hash-fn pi)
      (decf x0 (vec4 0.49999))
      (decf y0 (vec4 0.49999))
      (decf z0 (vec4 0.49999))
      (decf x1 (vec4 0.49999))
      (decf y1 (vec4 0.49999))
      (decf z1 (vec4 0.49999))
      (let ((norm0 (inversesqrt (+ (* x0 x0) (* y0 y0) (* z0 z0))))
            (norm1 (inversesqrt (+ (* x1 x1) (* y1 y1) (* z1 z1)))))
        (multf x0 norm0)
        (multf y0 norm0)
        (multf z0 norm0)
        (multf x1 norm1)
        (multf y1 norm1)
        (multf z1 norm1)
        (multiple-value-bind (ival-results igrad-results-x igrad-results-y)
            (umbra.shaping:quintic-hermite (.z pf) x0 x1 y0 y1 z0 z1)
          (let ((qh-results (umbra.shaping:quintic-hermite
                             (.y pf)
                             (vec4 (.xy ival-results) (.xy igrad-results-x))
                             (vec4 (.zw ival-results) (.zw igrad-results-x))
                             (vec4 (.xy igrad-results-y) 0 0)
                             (vec4 (.zw igrad-results-y) 0 0))))
            (map-domain
             (* (umbra.shaping:quintic-hermite
                 (.x pf) (.x qh-results) (.y qh-results) (.z qh-results) (.w qh-results))
                1.8475208)
             -1 1 0 1)))))))

(defun-gpu hermite ((p :vec3))
  (hermite p (lambda ((x :vec3)) (umbra.hashing:fast32/3-per-corner x))))
