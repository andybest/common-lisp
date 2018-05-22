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

(defun-gpu blum-blum-shub ((grid-cell :vec3))
  (decf (.xyz grid-cell) (* (floor (* (.xyz grid-cell) (/ 60.0))) 60.0))
  (let* ((grid-cell-inc1 (* (step grid-cell (v3:make 58.5)) (1+ grid-cell)))
         (p (blum-blum-shub/permute
             (+ (* (blum-blum-shub/permute
                    (.xyxy (v2:make (.x grid-cell) (.x grid-cell-inc1))))
                   1.0)
                (.xxyy (v2:make (.y grid-cell) (.y grid-cell-inc1))))))
         (low-z (blum-blum-shub/permute-and-resolve (+ p (.zzzz grid-cell))))
         (high-z (blum-blum-shub/permute-and-resolve (+ p (.zzzz grid-cell-inc1)))))
    (values low-z high-z)))

;;; Permutation polynomial
;;; Stefan Gustavson and Ian McEwan
;;; http://github.com/ashima/webgl-noise
;;; http://www.itn.liu.se/~stegu/GLSL-cellular

(defun-gpu permutation-polynomial/coord-prepare ((x :vec3))
  (- x (* (floor (* x (/ 289.0))) 289.0)))

(defun-gpu permutation-polynomial/coord-prepare ((x :vec4))
  (- x (* (floor (* x (/ 289.0))) 289.0)))

(defun-gpu permutation-polynomial/permute ((x :vec4))
  (* (fract (* x (+ (* (/ 34.0 289.0) x) (/ 289.0)))) 289.0))

(defun-gpu permutation-polynomial/resolve ((x :vec4))
  (fract (* x (/ 7.0 288.0))))

(defun-gpu permutation-polynomial ((grid-cell :vec2))
  (let ((hash-coord (permutation-polynomial/coord-prepare (v4:make grid-cell (1+ grid-cell)))))
    (permutation-polynomial/resolve
     (permutation-polynomial/permute
      (+ (permutation-polynomial/permute (.xzxz hash-coord))
         (.yyww hash-coord))))))

(defun-gpu permutation-polynomial ((grid-cell :vec3))
  (let* ((grid-cell (permutation-polynomial/coord-prepare grid-cell))
         (grid-cell-inc1 (* (step grid-cell (v3:make 287.5)) (1+ grid-cell)))
         (x (.xyxy (v2:make (.x grid-cell) (.x grid-cell-inc1))))
         (y (.xxyy (v2:make (.y grid-cell) (.y grid-cell-inc1))))
         (high-z (+ (permutation-polynomial/permute (+ (permutation-polynomial/permute x) y))))
         (low-z (permutation-polynomial/resolve
                 (permutation-polynomial/permute (+ high-z (.zzzz grid-cell))))))
    (setf high-z (permutation-polynomial/resolve
                  (permutation-polynomial/permute (+ high-z (.zzzz grid-cell-inc1)))))
    (values low-z high-z)))

;;; FAST32
;;; Brian Sharpe
;;; https://briansharpe.wordpress.com/2011/11/15/a-fast-and-simple-32bit-floating-point-hash-function/

(defun-gpu fast32 ((grid-cell :vec2))
  (let ((p (v4:make grid-cell (1+ grid-cell))))
    (setf p (- p (* (floor (* p (/ 71.0))) 71.0)))
    (incf p (.xyxy (v2:make 26.0 161.0)))
    (multf p p)
    (fract (* (.xzxz p) (.yyww p) (/ 951.135664)))))

(defun-gpu fast32 ((grid-cell :vec3))
  (decf (.xyz grid-cell) (* (floor (* (.xyz grid-cell) (/ 69.0))) 69.0))
  (let* ((grid-cell-inc1 (* (step grid-cell (v3:make 57.5)) (1+ grid-cell)))
         (p (+ (v4:make (.xy grid-cell) (.xy grid-cell-inc1))
               (.xyxy (v2:make 50.0 161.0))))
         (high-z (v4:make 0)))
    (multf p p)
    (setf p (* (.xzxz p) (.yyww p))
          (.xy high-z) (/ (+ 635.298681 (* (v2:make (.z grid-cell) (.z grid-cell-inc1)) 48.600388))))
    (values (fract (* p (.xxxx high-z)))
            (fract (* p (.yyyy high-z))))))

;;; FAST32_2
;;; Brian Sharpe
;;; https://github.com/BrianSharpe/GPU-Noise-Lib/blob/master/gpu_noise_lib.glsl

(defun-gpu fast32-2 ((grid-cell :vec2))
  (let ((p (v4:make grid-cell (1+ grid-cell))))
    (setf p (+ (* (- p (* (floor (* p (/ 69.0))) 69.0))
                  (.xyxy (v2:make 2.009842 1.372549)))
               (.xyxy (v2:make 403.839172 377.242706))))
    (multf p p)
    (fract (* (.xzxz p) (.yyww p) (/ 32745.708984)))))

(defun-gpu fast32-2 ((grid-cell :vec3))
  (decf grid-cell (* (floor (* grid-cell (/ 69.0))) 69.0))
  (let ((offset (v3:make 55.882355 63.167774 52.941177))
        (scale (v3:make 0.235142 0.205890 0.216449))
        (grid-cell-inc1 (* (step grid-cell (v3:make 67.5)) (1+ grid-cell)))
        (x (v4:make 0)))
    (setf grid-cell (+ (* grid-cell scale) offset)
          grid-cell-inc1 (+ (* grid-cell-inc1 scale) offset))
    (multf grid-cell grid-cell)
    (multf grid-cell-inc1 grid-cell-inc1)
    (setf x (* (v4:make (.x grid-cell) (.x grid-cell-inc1) (.x grid-cell) (.x grid-cell-inc1))
               (v4:make (.yy grid-cell) (.yy grid-cell-inc1))))
    (values (fract (* x (.zzzz grid-cell) (/ 69412.070313)))
            (fract (* x (.zzzz grid-cell-inc1) (/ 69412.070313))))))

(defun-gpu fast32-2 ((grid-cell :vec4))
  (decf grid-cell (* (floor (* grid-cell (/ 69.0))) 69.0))
  (let ((offset (v4:make 16.841230 18.774548 16.873274 13.664607))
        (scale (v4:make 0.102007 0.114473 0.139651 0.084550))
        (grid-cell-inc1 (* (step grid-cell (v4:make 67.5)) (1+ grid-cell)))
        (x (v4:make 0))
        (z (v4:make 0)))
    (setf grid-cell (+ (* grid-cell scale) offset)
          grid-cell-inc1 (+ (* grid-cell-inc1 scale) offset))
    (multf grid-cell grid-cell)
    (multf grid-cell-inc1 grid-cell-inc1)
    (setf x (* (v4:make (.x grid-cell) (.x grid-cell-inc1) (.x grid-cell) (.x grid-cell-inc1))
               (v4:make (.yy grid-cell) (.yy grid-cell-inc1)))
          z (* (v4:make (.z grid-cell) (.z grid-cell-inc1) (.z grid-cell) (.z grid-cell-inc1))
               (v4:make (.ww grid-cell) (.ww grid-cell-inc1))
               (/ 47165.636719)))
    (values (fract (* x (.xxxx z)))
            (fract (* x (.yyyy z)))
            (fract (* x (.zzzz z)))
            (fract (* x (.wwww z))))))
