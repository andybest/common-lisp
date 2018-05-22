(in-package :umbra.hashing)

;;; Blum Blum Shub
;;; Marc Olano
;;; http://www.cs.umbc.edu/~olano/papers/mNoise.pdf

(defun-gpu blum-blum-shub/coord-prepare ((x :vec4))
  (- x (* (floor (* x (/ 61.0))) 61.0)))

(defun-gpu blum-blum-shub/permute-and-resolve ((x :vec4))
  (fract (* x x (/ 61.0))))

(defun-gpu blum-blum-shub/permute ((x :vec4))
  (* (blum-blum-shub/permute-and-resolve x) 61.0))

(defun-gpu blum-blum-shub ((grid-cell :vec2))
  (let* ((hash-coord (blum-blum-shub/coord-prepare (v4:make grid-cell (1+ grid-cell))))
         (hash (blum-blum-shub/permute (* (.xzxz hash-coord) 7.0))))
    (blum-blum-shub/permute-and-resolve (+ hash (.yyww hash-coord)))))

(defun-gpu blum-blum-shub/hq ((grid-cell :vec2))
  (let* ((hash-coord (blum-blum-shub/coord-prepare (v4:make grid-cell (1+ grid-cell))))
         (hash (blum-blum-shub/permute (* (.xzxz hash-coord) 7.0))))
    (blum-blum-shub/permute-and-resolve
     (+ (blum-blum-shub/permute (+ hash (.yyww hash-coord)))
        (.xzxz hash-coord)))))

;;; SGPP
;;; Stefan Gustavson and Ian McEwan
;;; http://github.com/ashima/webgl-noise
;;; http://www.itn.liu.se/~stegu/GLSL-cellular

(defun-gpu sgpp/coord-prepare ((x :vec4))
  (- x (* (floor (* x (/ 289.0))) 289.0)))

(defun-gpu sgpp/permute ((x :vec4))
  (* (fract (* x (+ (* (/ 34.0 289.0) x) (/ 289.0)))) 289.0))

(defun-gpu sgpp/resolve ((x :vec4))
  (let ((k (/ 7.0 288.0)))
    (fract (* x k))))

(defun-gpu sgpp ((grid-cell :vec2))
  (let ((hash-coord (sgpp/coord-prepare (v4:make grid-cell (1+ grid-cell)))))
    (sgpp/resolve
     (sgpp/permute
      (+ (sgpp/permute (.xzxz hash-coord))
         (.yyww hash-coord))))))

;;; FAST32
;;; Brian Sharpe
;;; https://briansharpe.wordpress.com/2011/11/15/a-fast-and-simple-32bit-floating-point-hash-function/

(defun-gpu fast32 ((grid-cell :vec2))
  (let ((offset (v2:make 26.0 161.0))
        (domain 71.0)
        (some-large-float 951.135664)
        (p (v4:make grid-cell (1+ grid-cell))))
    (setf p (- p (* (floor (* p (/ domain))) domain)))
    (incf p (.xyxy offset))
    (multf p p)
    (fract (* (.xzxz p) (.yyww p) (/ some-large-float)))))

;;; FAST32_2
;;; Brian Sharpe
;;; https://github.com/BrianSharpe/GPU-Noise-Lib/blob/master/gpu_noise_lib.glsl
(defun-gpu fast32-2 ((grid-cell :vec2))
  (let ((offset (v2:make 403.839172 377.242706))
        (domain 69.0)
        (some-large-float 32745.708984)
        (scale (v2:make 2.009842 1.372549))
        (p (v4:make grid-cell (1+ grid-cell))))
    (setf p (+ (* (- p (* (floor (* p (/ domain))) domain)) (.xyxy scale)) (.xyxy offset)))

    (multf p p)
    (fract (* (.xzxz p) (.yyww p) (/ some-large-float)))))
