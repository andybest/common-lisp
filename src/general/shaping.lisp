(in-package #:net.mfiano.lisp.origin)

(defun linear (x)
  x)

(defun sine-out (x)
  (sin (* com:pi 0.5 x)))

(defun sine-in (x)
  (1+ (sin (* (1- x) 0.5 com:pi))))

(defun sine-in-out (x)
  (* 0.5 (- 1 (cos (* x com:pi)))))

(defun quadratic-out (x)
  (- (* x (- x 2))))

(defun quadratic-in (x)
  (* x x))

(defun quadratic-in-out (x)
  (if (< x 0.5)
      (* x x 2)
      (1- (+ (* x x -2) (* x 4.0)))))

(defun  cubic-out (x)
  (let ((f (1- x)))
    (1+ (expt f 3))))

(defun cubic-in (x)
  (expt x 3))

(defun cubic-in-out (x)
  (if (< x 0.5)
      (* (expt x 3) 4)
      (1+ (* 0.5 (expt (- (* x 2) 2) 3)))))

(defun quartic-out (x)
  (1+ (* (expt (1- x) 3) (- 1 x))))

(defun quartic-in (x)
  (expt x 4))

(defun quartic-in-out (x)
  (if (< x 0.5)
      (* (expt x 4) 8)
      (1+ (* -8 (expt (1- x) 4)))))

(defun quintic-out (x)
  (1+ (expt (1- x) 5)))

(defun quintic-in (x)
  (expt x 5))

(defun quintic-in-out (x)
  (if (< x 0.5)
      (* (expt x 5) 16)
      (1+ (* 0.5 (expt (- (* x 2) 2) 5)))))

(defun exponential-out (x)
  (if (= x 1)
      x
      (- 1 (expt 2 (* x -10)))))

(defun exponential-in (x)
  (if (zerop x)
      x
      (expt 2 (* 10 (1- x)))))

(defun exponential-in-out (x)
  (cond
    ((or (zerop x) (= x 1))
     x)
    ((< x 0.5)
     (* 0.5 (expt 2 (- (* x 20) 10))))
    (t
     (1+ (* -0.5 (expt 2 (+ (* x -20) 10)))))))

(defun circular-out (x)
  (sqrt (* (- 2 x) x)))

(defun circular-in (x)
  (- 1 (sqrt (- 1 (* x x)))))

(defun circular-in-out (x)
  (if (< x 0.5)
      (* 0.5 (- 1 (sqrt (- 1 (* 4 x x)))))
      (* 0.5 (1+ (sqrt (- (* (- (* 2 x) 3) (1- (* 2 x)))))))))

(defun back-out (x)
  (let ((v (- 1 x)))
    (- 1 (- (expt v 3) (* v (sin (* v com:pi)))))))

(defun back-in (x)
  (- (expt x 2) (* x (sin (* x com:pi)))))

(defun back-in-out (x)
  (if (< x 0.5)
      (let ((v (* x 2)))
        (* 0.5 (- (expt v 3) (* v (sin (* v com:pi))))))
      (let ((v (- 2 (* x 2))))
        (+ (* 0.5 (- 1 (- (expt v 3) (* v (sin (* v com:pi)))))) 0.5))))

(defun elastic-out (x)
  (1+ (* (sin (* -13 com:pi 0.5 (1+ x))) (expt 2 (* -10 x)))))

(defun elastic-in (x)
  (* (sin (* 13 com:pi 0.5 x)) (expt 2 (* 10 (1- x)))))

(defun elastic-in-out (x)
  (if (< x 0.5)
      (let ((v (* x 2)))
        (* 0.5 (sin (* 13 com:pi 0.5 v)) (expt 2 (* 10 (1- v)))))
      (let ((v (1- (* x 2))))
        (* 0.5 (+ (* (sin (* -13 com:pi 0.5 (1+ v))) (expt 2 (* -10 v))) 2)))))

(defun bounce-out (x)
  (cond
    ((< x 0.36363637)
     (/ (* x x 121) 16))
    ((< x 0.72727275)
     (+ (- (* 9.075 x x) (* 9.9 x)) 3.4))
    ((< x 0.9)
     (+ (- (* 12.066482 x x) (* 19.635458 x)) 8.898061))
    (t
     (+ (- (* 10.8 x x) (* 20.52 x)) 10.72))))

(defun bounce-in (x)
  (- 1 (bounce-out (- 1 x))))

(defun bounce-in-out (x)
  (let ((v (* x 2)))
    (if (< x 0.5)
        (* 0.5 (bounce-in v))
        (+ 0.5 (* 0.5 (bounce-out (1- v)))))))

(defun hermite-curve (x)
  (* x x (- 3 (* 2 x))))

(defun quintic-curve (x)
  (* (expt x 3) (+ (* x (- (* x 6) 15)) 10)))
