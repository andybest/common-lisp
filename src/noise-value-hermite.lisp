(in-package :umbra.noise)

;;;; Noise functions
;;;; Value Hermite

(defun-gpu value-hermite ((p :vec2)
                          (value-scale :float)
                          (gradient-scale :float)
                          (normalization-value :float)
                          (hash-fn (function (:vec2) (:vec4 :vec4 :vec4))))
  (let* ((pi (floor p))
         (pf (- p pi)))
    (multiple-value-bind (x y z) (funcall hash-fn pi)
      (let* ((x (* (- z 0.5) value-scale))
             (y (* (- x 0.49999) gradient-scale))
             (z (* (- y 0.49999) gradient-scale))
             (qh-results (umbra.shaping:quintic-hermite
                          (.y pf)
                          (vec4 (.xy x) (.xy y))
                          (vec4 (.zw x) (.zw y))
                          (vec4 (.xy z) 0 0)
                          (vec4 (.zw z) 0 0))))
        (map-domain
         (* (umbra.shaping:quintic-hermite
             (.x pf) (.x qh-results) (.y qh-results) (.z qh-results) (.w qh-results))
            normalization-value)
         -1 1 0 1)))))

(defun-gpu value-hermite ((p :vec2)
                          (value-scale :float)
                          (gradient-scale :float)
                          (normalization-value :float))
  (value-hermite p value-scale gradient-scale normalization-value
                 (lambda ((x :vec2)) (umbra.hashing:fast32/3-per-corner x))))

(defun-gpu value-hermite ((p :vec3)
                          (value-scale :float)
                          (gradient-scale :float)
                          (normalization-value :float)
                          (hash-fn (function (:vec3) (:vec4 :vec4 :vec4 :vec4
                                                      :vec4 :vec4 :vec4 :vec4))))
  (let* ((pi (floor p))
         (pf (- p pi)))
    (multiple-value-bind (x0 y0 z0 w0 x1 y1 z1 w1) (funcall hash-fn pi)
      (let ((x0 (* (- x0 0.5) value-scale))
            (y0 (* (- y0 0.49999) gradient-scale))
            (z0 (* (- z0 0.49999) gradient-scale))
            (w0 (* (- w0 0.49999) gradient-scale))
            (x1 (* (- x1 0.5) value-scale))
            (y1 (* (- y1 0.49999) gradient-scale))
            (z1 (* (- z1 0.49999) gradient-scale))
            (w1 (* (- w1 0.49999) gradient-scale)))
        (multiple-value-bind (ival-results igrad-results-x igrad-results-y)
            (umbra.shaping:quintic-hermite (.z pf) x0 x1 y0 y1 z0 z1 w0 w1)
          (let ((qh-results (umbra.shaping:quintic-hermite
                             (.y pf)
                             (vec4 (.xy ival-results) (.xy igrad-results-x))
                             (vec4 (.zw ival-results) (.zw igrad-results-x))
                             (vec4 (.xy igrad-results-y) 0 0)
                             (vec4 (.zw igrad-results-y) 0 0))))
            (map-domain
             (* (umbra.shaping:quintic-hermite
                 (.x pf) (.x qh-results) (.y qh-results) (.z qh-results) (.w qh-results))
                normalization-value)
             -1 1 0 1)))))))

(defun-gpu value-hermite ((p :vec3)
                          (value-scale :float)
                          (gradient-scale :float)
                          (normalization-value :float))
  (value-hermite p value-scale gradient-scale normalization-value
                 (lambda ((x :vec3)) (umbra.hashing:fast32/4-per-corner x))))
