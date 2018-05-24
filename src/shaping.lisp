(in-package :umbra.shaping)

;;;; Shaping functions
;;;; Various functions to modify a signal or interpolate a value non-linearly.

;;; Easing functions
;;; Robert Penner http://www.robertpenner.com/easing

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

;;; iq shaping functions
;;; Inigo Quilez http://www.iquilezles.org/www/articles/functions/functions.htm

(defun-gpu almost-identity ((x :float)
                            (threshold :float)
                            (min :float))
  (if (> x threshold)
      x
      (let ((a (- (* 2.0 min) threshold))
            (b (- (* 2.0 threshold) (* 3.0 min)))
            (v (/ x threshold)))
        (+ (* (+ (* a v) b) v v) min))))

(defun-gpu impulse ((x :float) (k :float))
  (let ((h (* x k)))
    (* h (exp (- 1 h)))))

(defun-gpu cubic-pulse ((x :float)
                        (center :float)
                        (width :float))
  (let ((x (abs (- x center))))
    (if (> x width)
        0.0
        (progn
          (divf x width)
          (- 1.0 (* x x (- 3.0 (* 2.0 x))))))))

(defun-gpu exponential-step ((x :float)
                             (exponent :float)
                             (sharpness :float))
  (exp (* (- sharpness) (expt x exponent))))

(defun-gpu gain ((x :float)
                 (k :float))
  (let* ((v (if (< x 0.5) x (- 1.0 x)))
         (a (* 0.5 (expt (* 2 v) k))))
    (if (< x 0.5)
        a
        (- 1.0 a))))

(defun-gpu parabola ((x :float)
                     (k :float))
  (expt (* 4.0 x (- 1.0 x)) k))


(defun-gpu power-curve ((x :float)
                        (a :float)
                        (b :float))
  (let ((k (/ (expt (+ a b) (+ a b))
              (* (expt a a) (expt b b)))))
    (* k (expt x a) (expt (- 1.0 x) b))))

(defun-gpu sinc-curve ((x :float)
                       (k :float))
  (let ((a (* umbra.math:+pi+ (1- (* k x)))))
    (/ (sin a) a)))

;;; Exponential shaping functions
;;; Golan Levin http://www.flong.com/texts/code/shapers_exp/

(defun-gpu exponential-emphasis ((x :float)
                                 (a :float))
  (let ((a (max +epsilon+ (min (- 1 +epsilon+) a))))
    (if (< a 0.5)
        (expt x (* 2 a))
        (expt x (/ (- 1 (* 2 (- a 0.5))))))))

(defun-gpu double-exponential-seat ((x :float)
                                    (a :float))
  (let ((a (min (- 1 +epsilon+) (max +epsilon+ a))))
    (if (<= x 0.5)
        (/ (expt (* 2 x) (- 1 a)) 2)
        (- 1 (/ (expt (* 2 (- 1 x)) (- 1 a)) 2)))))

(defun-gpu double-exponential-sigmoid ((x :float)
                                       (a :float))
  (let ((a (- 1 (min (- 1 +epsilon+) (max +epsilon+ a)))))
    (if (<= x 0.5)
        (/ (expt (* 2 x) (/ a)) 2)
        (- 1 (/ (expt (* 2 (- 1 x)) (/ a)) 2)))))

(defun-gpu logistic-sigmoid ((x :float)
                             (a :float))
  (let* ((a (1- (/ (- 1 (max +epsilon+ (min (- 1 +epsilon+) a))))))
         (a2 (/ (1+ (exp (- (* (- x 0.5) a 2.0))))))
         (b (/ (1+ (exp a))))
         (c (/ (1+ (exp (- a))))))
    (/ (- a2 b) (- c b))))

;;; Circular & elliptical shaping functions
;;; Golan Levin http://www.flong.com/texts/code/shapers_circ/

(defun-gpu double-circle-seat ((x :float)
                               (a :float))
  (let* ((a (max 0 (min 1 a)))
         (b (* (- x a) (- x a)))
         (c (* (- 1 a) (- 1 a))))
    (if (<= x a)
        (sqrt (- (* a a) b))
        (- 1 (sqrt (- c b))))))

(defun-gpu double-circle-sigmoid ((x :float)
                                  (a :float))
  (let* ((a (max 0 (min 1 a)))
         (b (* (- 1 a) (- 1 a)))
         (c (* (1- x) (1- x))))
    (if (<= x a)
        (- a (sqrt (- (* a a) (* x x))))
        (+ a (sqrt (- b c))))))

(defun-gpu double-elliptical-seat ((x :float)
                                   (a :float)
                                   (b :float))
  (let* ((a (max +epsilon+ (min (- 1 +epsilon+) a)))
         (b (max +epsilon+ (min (- 1 +epsilon+) b)))
         (c (* (- x a) (- x a)))
         (d (* (- 1 a) (- 1 a))))
    (if (<= x a)
        (* (/ b a) (sqrt (- (* a a) c)))
        (- 1 (* (/ (- 1 b) (- 1 a)) (sqrt (- d c)))))))

(defun-gpu double-elliptical-sigmoid ((x :float)
                                      (a :float)
                                      (b :float))
  (let* ((a (max +epsilon+ (min (- 1 +epsilon+) a)))
         (b (max +epsilon+ (min (- 1 +epsilon+) b)))
         (c (* (- 1 a) (- 1 a)))
         (d (* (1- x) (1- x))))
    (if (<= x a)
        (* b (- 1 (/ (sqrt (- (* a a) (* x x))) a)))
        (+ b (* (/ (- 1 b) (- 1 a)) (sqrt (- c d)))))))

;;; Polynomial shaping functions
;;; Golan Levin http://www.flong.com/texts/code/shapers_poly/

(defun-gpu blinn-wyvill-raised-inverted-cosine ((x :float))
  (let* ((x2 (* x x))
         (x4 (* x² x²))
         (x6 (* x⁴ x²)))
    (+ (- (* (/ 4 9.0) x6) (* (/ 17 9.0) x4))
       (* (/ 22 9.0) x2))))

(defun-gpu double-cubic-seat ((x :float)
                              (a :float)
                              (b :float))
  (let ((a (min (- 1 +epsilon+) (max +epsilon+ a)))
        (b (min 1 (max 0 b))))
    (if (<= x a)
        (- b (* b (expt (- 1 (/ x a)) 3.0)))
        (+ b (* (- 1 b) (expt (/ (- x a) (- 1 a)) 3.0))))))

(defun-gpu double-cubic-seat/linear-blend ((x :float)
                                           (a :float)
                                           (b :float))
  (let ((a (min (- 1 +epsilon+) (max +epsilon+ a)))
        (b (- 1 (min 1 (max 0 b)))))
    (if (<= x a)
        (+ (* b x) (* (- 1 b) a (- 1 (expt (- 1 (/ x a)) 3.0))))
        (+ (* b x) (* (- 1 b) (+ a (* (- 1 a) (expt (/ (- x a) (- 1 a)) 3.0))))))))

(defun-gpu double-odd-polynomial-seat ((x :float)
                                       (a :float)
                                       (b :float)
                                       (n :uint))
  (let ((a (min (- 1 +epsilon+) (max +epsilon+ a)))
        (b (min 1 (max 0 b)))
        (p (1+ (* 2 n))))
    (if (<= x a)
        (- b (* b (expt (- 1 (/ x a)) p)))
        (+ b (* (- 1 b) (expt (/ (- x a) (- 1 a)) p))))))

(defun-gpu quadratic-point ((x :float)
                            (a :float)
                            (b :float))
  (let* ((a (min (- 1 +epsilon+) (max +epsilon+ a)))
         (b (min 1 (max 0 b)))
         (a2 (- (/ (- 1 b) (- 1 a)) (/ b a)))
         (b2 (/ (- (* a2 a a) b) a))
         (y (- (* a2 x x) (* b2 x))))
    (min 1 (max 0 y))))
