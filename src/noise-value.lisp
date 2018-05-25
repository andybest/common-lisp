(in-package :umbra.noise)

;;;; Noise functions
;;;; Value noise

(defun-gpu value-noise ((p :vec2)
                        (hash-fn (function (:vec2) :vec4)))
  (let* ((pi (floor p))
         (pf (- p pi))
         (hash (funcall hash-fn pi))
         (blend (umbra.shaping:quintic-curve pf))
         (blend2 (vec4 blend (- 1 blend))))
    (dot hash (* (.zxzx blend2) (.wwyy blend2)))))

(defun-gpu value-noise ((p :vec2))
  (value-noise p (lambda ((x :vec2)) (umbra.hashing:fast32 x))))

(defun-gpu value-noise ((p :vec3)
                        (hash-fn (function (:vec3) (:vec4 :vec4))))
  (let* ((pi (floor p))
         (pf (- p pi)))
    (multiple-value-bind (low-z high-z) (funcall hash-fn pi)
      (let* ((blend (umbra.shaping:quintic-curve pf))
             (res0 (mix low-z high-z (.z blend)))
             (blend2 (vec4 (.xy blend) (- 1 (.xy blend)))))
        (dot res0 (* (.zxzx blend2) (.wwyy blend2)))))))

(defun-gpu value-noise ((p :vec3))
  (value-noise p (lambda ((x :vec3)) (umbra.hashing:fast32 x))))

(defun-gpu value-noise ((p :vec4)
                        (hash-fn (function (:vec4) (:vec4 :vec4 :vec4 :vec4))))
  (let* ((pi (floor p))
         (pf (- p pi)))
    (multiple-value-bind (z0w0 z1w0 z0w1 z1w1) (funcall hash-fn pi)
      (let* ((blend (umbra.shaping:quintic-curve pf))
             (res0 (+ z0w0 (* (- z0w1 z0w0) (.w blend))))
             (res1 (+ z1w0 (* (- z1w1 z1w0) (.w blend)))))
        (incf res0 (* (- res1 res0) (.z blend)))
        (setf (.zw blend) (- 1 (.xy blend)))
        (dot res0 (* (.zxzx blend) (.wwyy blend)))))))

(defun-gpu value-noise ((p :vec4))
  (value-noise p (lambda ((x :vec4)) (umbra.hashing:fast32-2 x))))
