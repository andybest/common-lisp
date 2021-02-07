(in-package #:coherent-noise/internal)

(deftype ub8-512 () '(simple-array u:ub8 (512)))
(deftype f50 () '(double-float #.(- (expt 2d0 50)) #.(expt 2d0 50)))

(u:define-constant +prime-x+ 501125321)

(u:define-constant +prime-y+ 1136930381)

(u:define-constant +prime-z+ 1720413743)

(defmacro lookup (table &body (first . rest))
  (if rest
      `(aref ,table (logand (+ ,first (lookup ,table ,@rest)) 255))
      `(aref ,table (logand ,first 255))))

(declaim (inline interpolate/cubic))
(defun interpolate/cubic (x)
  (* x x (- 3.0 (* x 2.0))))

(declaim (inline interpolate/quintic))
(defun interpolate/quintic (x)
  (* x x x (+ (* x (- (* x 6.0) 15.0)) 10.0)))

(defun make-rng (seed)
  (unless (stringp seed)
    (error 'invalid-seed :seed seed))
  (let ((rng (rng:make-generator seed)))
    (values rng (nth-value 1 (rng:get-seed rng)))))
