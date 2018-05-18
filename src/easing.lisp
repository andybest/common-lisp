(in-package :umbra.easing)

;;;; Robert Penner's Easing functions
;;;; Specify the rate of change for a parameter over time.
;;;; http://www.robertpenner.com/easing

(defun-gpu linear ((x :float))
  x)

(defun-gpu sine-out ((x :float))
  (sin (* +half-pi+ x)))

(defun-gpu sine-in ((x :float))
  (1+ (sin (* (1- x) +half-pi+))))

(defun-gpu sine-in-out ((x :float))
  (* 0.5 (- 1 (cos (* x +pi+)))))

(defun-gpu quadratic-out ((x :float))
  (- (* x (- x 2))))

(defun-gpu quadratic-in ((x :float))
  (* x x))

(defun-gpu quadratic-in-out ((x :float))
  (if (< x 0.5)
      (* x x 2)
      (1- (+ (* x x -2) (* x 4)))))

(defun-gpu cubic-out ((x :float))
  (let ((f (1- x)))
    (1+ (* f f f))))

(defun-gpu cubic-in ((x :float))
  (* x x x))

(defun-gpu cubic-in-out ((x :float))
  (if (< x 0.5)
      (* x x x 4)
      (let ((f (- (* x 2) 2)))
        (1+ (* 0.5 f f f)))))

(defun-gpu quartic-out ((x :float))
  (let ((f (1- x)))
    (1+ (* f f f (- 1 x)))))

(defun-gpu quartic-in ((x :float))
  (* x x x x))

(defun-gpu quartic-in-out ((x :float))
  (if (< x 0.5)
      (* x x x x 8)
      (let ((f (1- x)))
        (1+ (* -8 f f f f)))))

(defun-gpu quintic-out ((x :float))
  (let ((f (- x 1)))
    (1+ (* f f f f f))))

(defun-gpu quintic-in ((x :float))
  (* x x x x x))

(defun-gpu quintic-in-out ((x :float))
  (if (< x 0.5)
      (* x x x x x 16)
      (let ((f (- (* x 2) 2)))
        (1+ (* 0.5 f f f f f)))))

(defun-gpu exponential-out ((x :float))
  (if (= x 1)
      x
      (let ((f (* x -10)))
        (- 1 (expt 2 (* x -10))))))

(defun-gpu exponential-in ((x :float))
  (if (zerop x)
      x
      (expt 2 (* 10 (1- x)))))

(defun-gpu exponential-in-out ((x :float))
  (cond
    ((or (zerop x) (= x 1))
     x)
    ((< x 0.5)
     (* 0.5 (expt 2 (- (* x 20) 10))))
    (t
     (1+ (* -0.5 (expt 2 (+ (* x -20) 10)))))))

(defun-gpu circular-out ((x :float))
  (sqrt (* (- 2 x) x)))

(defun-gpu circular-in ((x :float))
  (- 1 (sqrt (- 1 (* x x)))))

(defun-gpu circular-in-out ((x :float))
  (if (< x 0.5)
      (* 0.5 (- 1 (sqrt (- 1 (* 4 x x)))))
      (* 0.5 (1+ (sqrt (- (* (- (* 2 x) 3) (1- (* 2 x)))))))))

(defun-gpu back-out ((x :float))
  (let ((f (- 1 x)))
    (- 1 (- (* f f f) (* f (sin (* f +pi+)))))))

(defun-gpu back-in ((x :float))
  (- (* x x x) (* x (sin (* x +pi+)))))

(defun-gpu back-in-out ((x :float))
  (if (< x 0.5)
      (let ((f (* x 2)))
        (* 0.5 (- (* f f f) (* f (sin (* f +pi+))))))
      (let ((f (- 2 (* x 2))))
        (+ (* 0.5 (- 1 (- (* f f f) (* f (sin (* f +pi+)))))) 0.5))))

(defun-gpu elastic-out ((x :float))
  (1+ (* (sin (* -13 +half-pi+ (1+ x))) (expt 2 (* -10 x)))))

(defun-gpu elastic-in ((x :float))
  (* (sin (* 13 +half-pi+ x)) (expt 2 (* 10 (1- x)))))

(defun-gpu elastic-in-out ((x :float))
  (if (< x 0.5)
      (let ((v (* x 2)))
        (* 0.5 (sin (* 13 +half-pi+ v)) (expt 2 (* 10 (1- v)))))
      (let ((v (1- (* x 2))))
        (* 0.5 (+ (* (sin (* -13 +half-pi+ (1+ v))) (expt 2 (* -10 v))) 2)))))

(defun-gpu bounce-out ((x :float))
  (cond
    ((< x (/ 4 11.0))
     (/ (* x x 121) 16.0))
    ((< x (/ 8 11.0))
     (+ (- (* (/ 363 40.0) x x) (* 9.9 x)) 3.4))
    ((< x 0.9)
     (+ (- (* (/ 4356 361.0) x x) (* (/ 35442 1805.0) x)) (/ 16061 1805.0)))
    (t
     (+ (- (* 10.8 x x) (* 20.52 x)) 10.72))))

(defun-gpu bounce-in ((x :float))
  (- 1 (bounce-out (- 1 x))))

(defun-gpu bounce-in-out ((x :float))
  (let ((f (* x 2)))
    (if (< x 0.5)
        (* 0.5 (bounce-in f))
        (+ 0.5 (* 0.5 (bounce-out (1- f)))))))
