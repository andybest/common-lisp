(in-package :umbra.noise)

;;;; Noise functions
;;;; Value Hermite

(defun-gpu value-hermite ((point :vec2)
                          (value-scale :float)
                          (gradient-scale :float)
                          (normalization-value :float)
                          (hash-fn (function (:vec2) (:vec4 :vec4 :vec4))))
  (mvlet* ((cell (floor point))
           (vec (- point cell))
           (hash-x hash-y hash-z (funcall hash-fn cell))
           (hash-x (* (- hash-z 0.5) value-scale))
           (hash-y (* (- hash-x 0.5 +epsilon+) gradient-scale))
           (hash-z (* (- hash-y 0.5 +epsilon+) gradient-scale))
           (out (umbra.shaping:quintic-hermite
                 (.y vec)
                 (vec4 (.xy hash-x) (.xy hash-y))
                 (vec4 (.zw hash-x) (.zw hash-y))
                 (vec4 (.xy hash-z) 0 0)
                 (vec4 (.zw hash-z) 0 0)))
           (out (* (umbra.shaping:quintic-hermite (.x vec) (.x out) (.y out) (.z out) (.w out))
                   normalization-value)))
    (map-domain out -1 1 0 1)))

(defun-gpu value-hermite ((point :vec2)
                          (value-scale :float)
                          (gradient-scale :float)
                          (normalization-value :float))
  (value-hermite point value-scale gradient-scale normalization-value
                 (lambda ((x :vec2)) (umbra.hashing:fast32/3-per-corner x))))

(defun-gpu value-hermite ((point :vec3)
                          (value-scale :float)
                          (gradient-scale :float)
                          (normalization-value :float)
                          (hash-fn (function (:vec3) (:vec4 :vec4 :vec4 :vec4
                                                      :vec4 :vec4 :vec4 :vec4))))
  (mvlet* ((cell (floor point))
           (vec (- point cell))
           (hash-x0 hash-y0 hash-z0 hash-w0 hash-x1 hash-y1 hash-z1 hash-w1 (funcall hash-fn cell))
           (hash-x0 (* (- hash-x0 0.5) value-scale))
           (hash-y0 (* (- hash-y0 0.5 +epsilon+) gradient-scale))
           (hash-z0 (* (- hash-z0 0.5 +epsilon+) gradient-scale))
           (hash-w0 (* (- hash-w0 0.5 +epsilon+) gradient-scale))
           (hash-x1 (* (- hash-x1 0.5) value-scale))
           (hash-y1 (* (- hash-y1 0.5 +epsilon+) gradient-scale))
           (hash-z1 (* (- hash-z1 0.5 +epsilon+) gradient-scale))
           (hash-w1 (* (- hash-w1 0.5 +epsilon+) gradient-scale))
           (ival igrad-x igrad-y (umbra.shaping:quintic-hermite
                                  (.z vec) hash-x0 hash-x1 hash-y0 hash-y1 hash-z0 hash-z1 hash-w0
                                  hash-w1))
           (out (umbra.shaping:quintic-hermite
                 (.y vec)
                 (vec4 (.xy ival) (.xy igrad-x))
                 (vec4 (.zw ival) (.zw igrad-x))
                 (vec4 (.xy igrad-y) 0 0)
                 (vec4 (.zw igrad-y) 0 0)))
           (out (* (umbra.shaping:quintic-hermite (.x vec) (.x out) (.y out) (.z out) (.w out))
                   normalization-value)))
    (map-domain out -1 1 0 1)))

(defun-gpu value-hermite ((point :vec3)
                          (value-scale :float)
                          (gradient-scale :float)
                          (normalization-value :float))
  (value-hermite point value-scale gradient-scale normalization-value
                 (lambda ((x :vec3)) (umbra.hashing:fast32/4-per-corner x))))
