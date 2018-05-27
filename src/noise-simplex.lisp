(in-package :umbra.noise)

;;;; Noise functions
;;;; Simplex noise utilities

(defconstant +simplex-2d/skew-factor+ (* 0.5 (1- (sqrt 3))))
(defconstant +simplex-2d/unskew-factor+ (/ (- 3 (sqrt 3)) 6))
(defconstant +simplex-2d/triangle-height+ (sqrt 0.5))
(defconstant +simplex-2d/inverse-triangle-height+ (sqrt (/ 0.5)))
(defconstant +simplex-2d/inverse-triangle-half-edge-length+ (/ (sqrt 0.75) (sqrt 0.125)))
(defconstant +simplex-2d/norm-factor+ (/ (* 0.4082483 (expt (- 0.5 (expt 0.4082483 2)) 4) 2)))

(defconstant +simplex-3d/skew-factor+ (/ 3.0))
(defconstant +simplex-3d/unskew-factor+ (/ 6.0))
(defconstant +simplex-3d/corner-pos+ 0.5)
(defconstant +simplex-3d/pyramid-height+ (sqrt 0.5))
(defconstant +simplex-3d/inverse-pyramid-height+ (sqrt (/ 0.5)))
(defconstant +simplex-3d/inverse-triangle-half-edge-length+ (/ 2 (sqrt 0.75)))
(defconstant +simplex-3d/norm-factor+ (/ (* 0.4330127 (expt (- 0.5 (expt 0.4330127 2)) 3) 2)))

(defun-gpu simplex/get-corner-vectors ((p :vec3))
  (let* ((p (* p +simplex-3d/pyramid-height+))
         (pi (floor (+ p (dot p (vec3 +simplex-3d/skew-factor+)))))
         (x0 (+ (- p pi) (dot pi (vec3 +simplex-3d/unskew-factor+))))
         (g (step (.yzx x0) (.xyz x0)))
         (l (- 1 g))
         (pi1 (min (.xyz g) (.zxy l)))
         (pi2 (max (.xyz g) (.zxy l)))
         (x1 (+ (- x0 pi1) +simplex-3d/unskew-factor+))
         (x2 (+ (- x0 pi2) +simplex-3d/skew-factor+))
         (x3 (- x0 +simplex-3d/corner-pos+))
         (v1234-x (vec4 (.x x0) (.x x1) (.x x2) (.x x3)))
         (v1234-y (vec4 (.y x0) (.y x1) (.y x2) (.y x3)))
         (v1234-z (vec4 (.z x0) (.z x1) (.z x2) (.z x3))))
    (values pi pi1 pi2 v1234-x v1234-y v1234-z)))

(defun-gpu simplex/get-surflet-weights ((v1234-x :vec4)
                                        (v1234-y :vec4)
                                        (v1234-z :vec4))
  (let ((surflet-weights (+ (* v1234-x v1234-x) (* v1234-y v1234-y) (* v1234-z v1234-z))))
    (setf surflet-weights (max (- 0.5 surflet-weights) 0.0))
    (* surflet-weights surflet-weights surflet-weights)))
