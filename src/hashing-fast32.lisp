(in-package :umbra.hashing)

;;;; Hashing functions
;;;; FAST32
;;;; Brian Sharpe
;;;; https://briansharpe.wordpress.com/2011/11/15/a-fast-and-simple-32bit-floating-point-hash-function/

(defun-gpu fast32 ((grid-cell :vec2))
  (let ((p (vec4 grid-cell (1+ grid-cell))))
    (decf p (* (floor (* p (/ 71.0))) 71.0))
    (incf p (vec4 26 161 26 161))
    (multf p p)
    (fract (* (.xzxz p) (.yyww p) (/ 951.135664)))))

(defun-gpu fast32/2-per-corner ((grid-cell :vec2))
  (let ((p (vec4 grid-cell (1+ grid-cell))))
    (decf p (* (floor (* p (/ 71.0))) 71.0))
    (incf p (vec4 26 161 26 161))
    (multf p p)
    (setf p (* (.xzxz p) (.yyww p)))
    (values (fract (* p (/ 951.135664)))
            (fract (* p (/ 642.949883))))))

(defun-gpu fast32/3-per-corner ((grid-cell :vec2))
  (let ((p (vec4 grid-cell (1+ grid-cell))))
    (decf p (* (floor (* p (/ 71.0))) 71.0))
    (incf p (vec4 26 161 26 161))
    (multf p p)
    (setf p (* (.xzxz p) (.yyww p)))
    (values (fract (* p (/ 951.135664)))
            (fract (* p (/ 642.949883)))
            (fract (* p (/ 803.202459))))))

(defun-gpu fast32/cell ((grid-cell :vec2))
  (let ((p (- grid-cell (* (floor (* grid-cell (/ 71.0))) 71.0))))
    (incf p (vec2 26 161))
    (multf p p)
    (fract (* (.x p) (.y p) (/ (vec4 "951.135664" "642.949883" "803.202459" "986.973274"))))))

(defun-gpu fast32 ((grid-cell :vec3))
  (decf grid-cell (* (floor (* grid-cell (/ 69.0))) 69.0))
  (let* ((grid-cell-inc1 (* (step grid-cell (vec3 67.5)) (1+ grid-cell)))
         (p (+ (vec4 (.xy grid-cell) (.xy grid-cell-inc1))
               (vec4 50 161 50 161)))
         (high-z (/ (+ "635.298681" (* (vec2 (.z grid-cell) (.z grid-cell-inc1)) "48.600388")))))
    (multf p p)
    (setf p (* (.xzxz p) (.yyww p)))
    (values (fract (* p (.x high-z)))
            (fract (* p (.y high-z))))))

(defun-gpu fast32 ((grid-cell :vec3)
                   (v1-mask :vec3)
                   (v2-mask :vec3))
  (decf grid-cell (* (floor (* grid-cell (/ 69.0))) 69.0))
  (let* ((grid-cell-inc1 (* (step grid-cell (vec3 67.5)) (1+ grid-cell)))
         (p (+ (vec4 (.xy grid-cell) (.xy grid-cell-inc1))
               (vec4 50 161 50 161)))
         ((v1xy-v2xy :vec4))
         ((v1z-v2z :vec2))
         ((mod-vals :vec4)))
    (multf p p)
    (setf v1xy-v2xy (mix (.xyxy p) (.zwzw p) (vec4 (.xy v1-mask) (.xy v2-mask)))
          p (* (vec4 (.x p) (.xy v1xy-v2xy) (.z p))
               (vec4 (.y p) (.yw v1xy-v2xy) (.w p))))
    (if (< (.z v1-mask) 0.5)
        (setf v1z-v2z (.zz grid-cell))
        (setf v1z-v2z (.zz grid-cell-inc1)))
    (setf mod-vals (/ (+ "635.298681"
                         (* (vec4 (.z grid-cell) v1z-v2z (.z grid-cell-inc1)) "48.500388"))))
    (fract (* p mod-vals))))

(defun-gpu fast32/3-per-corner ((grid-cell :vec3)
                                (v1-mask :vec3)
                                (v2-mask :vec3))
  (decf grid-cell (* (floor (* grid-cell (/ 69.0))) 69.0))
  (let* ((grid-cell-inc1 (* (step grid-cell (vec3 67.5)) (1+ grid-cell)))
         (p (+ (vec4 (.xy grid-cell) (.xy grid-cell-inc1))
               (vec4 50 161 50 161)))
         (floats (vec3 "635.298681" "682.357502" "668.926525"))
         (z-inc (vec3 "48.500388" "65.294118" "63.934599"))
         ((v1xy-v2xy :vec4))
         ((low-z-mods :vec3))
         ((high-z-mods :vec3)))
    (multf p p)
    (setf v1xy-v2xy (mix (.xyxy p) (.zwzw p) (vec4 (.xy v1-mask) (.xy v2-mask)))
          p (* (vec4 (.x p) (.xz v1xy-v2xy) (.z p))
               (vec4 (.y p) (.yw v1xy-v2xy) (.w p)))
          low-z-mods (/ (+ floats (* z-inc (.z grid-cell))))
          high-z-mods (/ (+ floats (* z-inc (.z grid-cell-inc1)))))
    (if (< (.z v1-mask) 0.5)
        (setf v1-mask low-z-mods)
        (setf v1-mask high-z-mods))
    (if (< (.z v2-mask) 0.5)
        (setf v2-mask low-z-mods)
        (setf v2-mask high-z-mods))
    (values (fract (* p (vec4 (.x low-z-mods) (.x v1-mask) (.x v2-mask) (.x high-z-mods))))
            (fract (* p (vec4 (.y low-z-mods) (.y v1-mask) (.y v2-mask) (.y high-z-mods))))
            (fract (* p (vec4 (.z low-z-mods) (.z v1-mask) (.z v2-mask) (.z high-z-mods)))))))

(defun-gpu fast32/3-per-corner ((grid-cell :vec3))
  (decf grid-cell (* (floor (* grid-cell (/ 69.0))) 69.0))
  (let* ((grid-cell-inc1 (* (step grid-cell (vec3 67.5)) (1+ grid-cell)))
         (p (+ (vec4 (.xy grid-cell) (.xy grid-cell-inc1))
               (vec4 50 161 50 161)))
         (floats (vec3 "635.298681" "682.357502" "668.926525"))
         (z-inc (vec3 "48.500388" "65.294118" "63.934599"))
         (low-z-mod (/ (+ floats (* (.z grid-cell) z-inc))))
         (high-z-mod (/ (+ floats (* (.z grid-cell-inc1) z-inc)))))
    (multf p p)
    (setf p (* (.xzxz p) (.yyww p)))
    (values (fract (* p (.x low-z-mod)))
            (fract (* p (.y low-z-mod)))
            (fract (* p (.z low-z-mod)))
            (fract (* p (.x high-z-mod)))
            (fract (* p (.y high-z-mod)))
            (fract (* p (.z high-z-mod))))))

(defun-gpu fast32/4-per-corner ((grid-cell :vec3))
  (decf grid-cell (* (floor (* grid-cell (/ 69.0))) 69.0))
  (let* ((grid-cell-inc1 (* (step grid-cell (vec3 67.5)) (1+ grid-cell)))
         (p (+ (vec4 (.xy grid-cell) (.xy grid-cell-inc1))
               (vec4 50 161 50 161)))
         (floats (vec4 "635.298681" "682.357502" "668.926525" "588.255119"))
         (z-inc (vec4 "48.500388" "65.294118" "63.934599" "63.279683"))
         (low-z-3 (/ (+ floats (* (.z grid-cell) z-inc))))
         (high-z-3 (/ (+ floats (* (.z grid-cell-inc1) z-inc)))))
    (multf p p)
    (setf p (* (.xzxz p) (.yyww p)))
    (values (fract (* p (.x low-z-3)))
            (fract (* p (.y low-z-3)))
            (fract (* p (.z low-z-3)))
            (fract (* p (.w low-z-3)))
            (fract (* p (.x high-z-3)))
            (fract (* p (.y high-z-3)))
            (fract (* p (.z high-z-3)))
            (fract (* p (.w high-z-3))))))

(defun-gpu fast32/cell ((grid-cell :vec3))
  (let ((floats (vec4 "635.298681" "682.357502" "668.926525" "588.255119"))
        (z-inc (vec4 "48.500388" "65.294118" "63.934599" "63.279683")))
    (decf grid-cell (* (floor (* grid-cell (/ 71.0))) 71.0))
    (incf (.xy grid-cell) (vec2 50 161))
    (multf (.xy grid-cell) (.xy grid-cell))
    (fract (* (.x grid-cell)
              (.y grid-cell)
              (/ (+ floats (* z-inc (.z grid-cell))))))))
