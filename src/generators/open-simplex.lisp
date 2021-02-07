(in-package #:coherent-noise/internal)

;;; 2D OpenSimplex

(u:define-constant +open-simplex-2d/stretch+ (/ (1- (/ (sqrt 3d0))) 2))

(u:define-constant +open-simplex-2d/squish+ (/ (1- (sqrt 3d0)) 2))

(u:define-constant +open-simplex-2d/scale+ (/ 47d0))

(u:define-constant +open-simplex-2d/gradients+
    (let ((data '(5 2 2 5 -5 2 -2 5 5 -2 2 -5 -5 -2 -2 -5)))
      (make-array 16 :element-type 'fixnum :initial-contents data))
  :test #'equalp)

(declaim (inline %make-open-simplex-2d-state))
(defstruct (open-simplex-2d-state
            (:constructor %make-open-simplex-2d-state)
            (:conc-name oss2d-)
            (:predicate nil)
            (:copier nil))
  (table +perlin/permutation+ :type ub8-512)
  (stretch-offset 0d0 :type u:f64)
  (xsb 0 :type fixnum)
  (ysb 0 :type fixnum)
  (dx0 0d0 :type u:f64)
  (dy0 0d0 :type u:f64)
  (dx1 0d0 :type u:f64)
  (dy1 0d0 :type u:f64)
  (dx2 0d0 :type u:f64)
  (dy2 0d0 :type u:f64)
  (dx-ext 0d0 :type u:f64)
  (dy-ext 0d0 :type u:f64)
  (xsv-ext 0 :type fixnum)
  (ysv-ext 0 :type fixnum)
  (xins 0d0 :type u:f64)
  (yins 0d0 :type u:f64)
  (ins 0d0 :type u:f64)
  (value 0d0 :type u:f64))

(declaim (inline make-open-simplex-2d-state))
(defun make-open-simplex-2d-state (table x y)
  (let* ((stretch-offset (* (+ x y) +open-simplex-2d/stretch+))
         (xs (+ x stretch-offset))
         (ys (+ y stretch-offset))
         (xsb (floor xs))
         (ysb (floor ys))
         (squish-offset (* (+ xsb ysb) +open-simplex-2d/squish+))
         (dx0 (- x (+ xsb squish-offset)))
         (dy0 (- y (+ ysb squish-offset)))
         (xins (- xs xsb))
         (yins (- ys ysb)))
    (declare (f50 xs ys))
    (%make-open-simplex-2d-state :table table
                                 :xsb xsb
                                 :ysb ysb
                                 :dx0 dx0
                                 :dy0 dy0
                                 :dx1 (- dx0 1 +open-simplex-2d/squish+)
                                 :dy1 (- dy0 +open-simplex-2d/squish+)
                                 :dx2 (- dx0 +open-simplex-2d/squish+)
                                 :dy2 (- dy0 1 +open-simplex-2d/squish+)
                                 :xins xins
                                 :yins yins
                                 :ins (+ xins yins))))

(declaim (inline open-simplex-2d/extrapolate))
(defun open-simplex-2d/extrapolate (table xsb ysb dx dy)
  (let ((index (logand (lookup table ysb xsb) 14)))
    (+ (* (aref +open-simplex-2d/gradients+ index) dx)
       (* (aref +open-simplex-2d/gradients+ (1+ index)) dy))))

(declaim (inline open-simplex-2d/contribute))
(defun open-simplex-2d/contribute (state dx dy xsb ysb)
  (let ((a (- 2 (* dx dx) (* dy dy))))
    (when (plusp a)
      (incf (oss2d-value state)
            (* (expt a 4) (open-simplex-2d/extrapolate (oss2d-table state) xsb ysb dx dy))))
    (values)))

(declaim (inline open-simplex-2d/contribute1))
(defun open-simplex-2d/contribute1 (state)
  (let ((xsb (oss2d-xsb state))
        (ysb (oss2d-ysb state)))
    (open-simplex-2d/contribute state
                                (oss2d-dx1 state)
                                (oss2d-dy1 state)
                                (1+ xsb) ysb)
    (open-simplex-2d/contribute state
                                (oss2d-dx2 state)
                                (oss2d-dy2 state)
                                xsb
                                (1+ ysb))))

(declaim (inline open-simplex-2d/contribute2))
(defun open-simplex-2d/contribute2 (state)
  (open-simplex-2d/contribute state
                              (oss2d-dx0 state)
                              (oss2d-dy0 state)
                              (oss2d-xsb state)
                              (oss2d-ysb state))
  (open-simplex-2d/contribute state
                              (oss2d-dx-ext state)
                              (oss2d-dy-ext state)
                              (oss2d-xsv-ext state)
                              (oss2d-ysv-ext state)))

(declaim (inline open-simplex-2d/in1))
(defun open-simplex-2d/in1 (state)
  (let ((xins (oss2d-xins state))
        (yins (oss2d-yins state))
        (zins (- 1 (oss2d-ins state)))
        (xsb (oss2d-xsb state))
        (ysb (oss2d-ysb state))
        (dx0 (oss2d-dx0 state))
        (dy0 (oss2d-dy0 state))
        (sq2 #.(* +open-simplex-2d/squish+ 2)))
    (if (or (> zins xins) (> zins yins))
        (if (> xins yins)
            (psetf (oss2d-xsv-ext state) (1+ xsb)
                   (oss2d-ysv-ext state) (1- ysb)
                   (oss2d-dx-ext state) (1- dx0)
                   (oss2d-dy-ext state) (1+ dy0))
            (psetf (oss2d-xsv-ext state) (1- xsb)
                   (oss2d-ysv-ext state) (1+ ysb)
                   (oss2d-dx-ext state) (1+ dx0)
                   (oss2d-dy-ext state) (1- dy0)))
        (psetf (oss2d-xsv-ext state) (1+ xsb)
               (oss2d-ysv-ext state) (1+ ysb)
               (oss2d-dx-ext state) (- dx0 1 sq2)
               (oss2d-dy-ext state) (- dy0 1 sq2)))
    (values)))

(declaim (inline open-simplex-2d/in2))
(defun open-simplex-2d/in2 (state)
  (let ((xins (oss2d-xins state))
        (yins (oss2d-yins state))
        (zins (- 2 (oss2d-ins state)))
        (xsb (oss2d-xsb state))
        (ysb (oss2d-ysb state))
        (dx0 (oss2d-dx0 state))
        (dy0 (oss2d-dy0 state))
        (sq2 #.(* +open-simplex-2d/squish+ 2)))
    (if (or (< zins xins) (< zins yins))
        (if (> xins yins)
            (psetf (oss2d-xsv-ext state) (+ xsb 2)
                   (oss2d-ysv-ext state) ysb
                   (oss2d-dx-ext state) (- dx0 2 sq2)
                   (oss2d-dy-ext state) (- dy0 sq2))
            (psetf (oss2d-xsv-ext state) xsb
                   (oss2d-ysv-ext state) (+ ysb 2)
                   (oss2d-dx-ext state) (- dx0 sq2)
                   (oss2d-dy-ext state) (- dy0 2 sq2)))
        (psetf (oss2d-dx-ext state) dx0
               (oss2d-dy-ext state) dy0
               (oss2d-xsv-ext state) xsb
               (oss2d-ysv-ext state) ysb))
    (incf (oss2d-xsb state))
    (incf (oss2d-ysb state))
    (psetf (oss2d-dx0 state) (- dx0 1 sq2)
           (oss2d-dy0 state) (- dy0 1 sq2))
    (values)))

(declaim (inline %open-simplex-2d))
(defun %open-simplex-2d (table x y)
  (declare (optimize speed)
           (u:f64 x y))
  (let ((state (make-open-simplex-2d-state table x y)))
    (open-simplex-2d/contribute1 state)
    (if (<= (oss2d-ins state) 1)
        (open-simplex-2d/in1 state)
        (open-simplex-2d/in2 state))
    (open-simplex-2d/contribute2 state)
    (float (* (oss2d-value state) +open-simplex-2d/scale+) 1f0)))

;;; 3D OpenSimplex

(u:define-constant +open-simplex-3d/stretch+ (/ -6d0))

(u:define-constant +open-simplex-3d/squish+ (/ 3d0))

(u:define-constant +open-simplex-3d/scale+ (/ 103d0))

(u:define-constant +open-simplex-3d/permutation+
    (let ((data '(21 48 51 57 54 45 33 39 27 69 0 15 6 51 21 27 60 36 21 18 63 66 24 9 39 0 63 30 69
                  66 18 12 21 0 54 9 0 6 15 42 66 36 9 33 63 33 33 24 27 27 27 48 63 15 24 45 18 60
                  15 48 9 0 60 21 6 63 69 42 57 0 9 66 15 6 42 45 33 45 39 6 36 57 39 42 12 27 60 51
                  21 66 15 48 12 18 69 18 51 3 45 51 3 0 24 3 51 12 36 57 48 51 54 3 24 12 45 30 60
                  60 45 42 60 12 39 18 15 54 9 48 12 3 30 30 12 9 15 30 42 9 66 18 45 30 39 60 45 42
                  33 33 69 48 30 51 42 63 12 54 21 45 6 63 69 24 24 6 60 30 57 66 15 27 15 33 69 57
                  12 27 27 66 45 39 57 6 36 42 21 51 24 48 30 51 48 24 6 18 3 36 33 30 6 3 66 54 0 36
                  69 33 54 3 27 9 3 57 27 42 69 33 3 0 66 21 39 21 30 39 48 36 36 24 57 3 6 63 21 12
                  18 42 54 60 39 63 18 54 57 15 0 0 9 63 24 9 18 54 69 39 36 36)))
      (make-array 256 :element-type 'u:ub8 :initial-contents data))
  :test #'equalp)

(u:define-constant +open-simplex-3d/gradients+
    (let ((data '(-11 4 4 -4 11 4 -4 4 11 11 4 4 4 11 4 4 4 11 -11 -4 4 -4 -11 4 -4 -4 11 11 -4 4 4
                  -11 4 4 -4 11 -11 4 -4 -4 11 -4 -4 4 -11 11 4 -4 4 11 -4 4 4 -11 -11 -4 -4 -4 -11
                  -4 -4 -4 -11 11 -4 -4 4 11 -4 4 -4 -11)))
      (make-array 72 :element-type 'fixnum :initial-contents data))
  :test #'equalp)

(declaim (inline %make-open-simplex-3d-state))
(defstruct (open-simplex-3d-state
            (:constructor %make-open-simplex-3d-state)
            (:conc-name oss3d-)
            (:predicate nil)
            (:copier nil))
  (table +open-simplex-3d/permutation+ :type (simple-array u:ub8 (256)))
  (stretch-offset 0d0 :type u:f64)
  (xsb 0 :type fixnum)
  (ysb 0 :type fixnum)
  (zsb 0 :type fixnum)
  (dx0 0d0 :type u:f64)
  (dy0 0d0 :type u:f64)
  (dz0 0d0 :type u:f64)
  (dx1 0d0 :type u:f64)
  (dy1 0d0 :type u:f64)
  (dz1 0d0 :type u:f64)
  (dx2 0d0 :type u:f64)
  (dy2 0d0 :type u:f64)
  (dz2 0d0 :type u:f64)
  (dx3 0d0 :type u:f64)
  (dy3 0d0 :type u:f64)
  (dz3 0d0 :type u:f64)
  (dx4 0d0 :type u:f64)
  (dy4 0d0 :type u:f64)
  (dz4 0d0 :type u:f64)
  (dx5 0d0 :type u:f64)
  (dy5 0d0 :type u:f64)
  (dz5 0d0 :type u:f64)
  (dx6 0d0 :type u:f64)
  (dy6 0d0 :type u:f64)
  (dz6 0d0 :type u:f64)
  (dx-ext0 0d0 :type u:f64)
  (dy-ext0 0d0 :type u:f64)
  (dz-ext0 0d0 :type u:f64)
  (dx-ext1 0d0 :type u:f64)
  (dy-ext1 0d0 :type u:f64)
  (dz-ext1 0d0 :type u:f64)
  (xsv-ext0 0 :type fixnum)
  (ysv-ext0 0 :type fixnum)
  (zsv-ext0 0 :type fixnum)
  (xsv-ext1 0 :type fixnum)
  (ysv-ext1 0 :type fixnum)
  (zsv-ext1 0 :type fixnum)
  (xins 0d0 :type u:f64)
  (yins 0d0 :type u:f64)
  (zins 0d0 :type u:f64)
  (ins 0d0 :type u:f64)
  (value 0d0 :type u:f64))

(declaim (inline make-open-simplex-3d-state))
(defun make-open-simplex-3d-state (table x y z)
  (let* ((stretch-offset (* (+ x y z) +open-simplex-3d/stretch+))
         (xs (+ x stretch-offset))
         (ys (+ y stretch-offset))
         (zs (+ z stretch-offset))
         (xsb (floor xs))
         (ysb (floor ys))
         (zsb (floor zs))
         (squish-offset (* (+ xsb ysb zsb) +open-simplex-3d/squish+))
         (dx0 (- x (+ xsb squish-offset)))
         (dy0 (- y (+ ysb squish-offset)))
         (dz0 (- z (+ zsb squish-offset)))
         (xins (- xs xsb))
         (yins (- ys ysb))
         (zins (- zs zsb)))
    (declare (f50 xs ys zs))
    (%make-open-simplex-3d-state :table table
                                 :xsb xsb
                                 :ysb ysb
                                 :zsb zsb
                                 :dx0 dx0
                                 :dy0 dy0
                                 :dz0 dz0
                                 :xins xins
                                 :yins yins
                                 :zins zins
                                 :ins (+ xins yins zins))))

(declaim (inline open-simplex-3d/extrapolate))
(defun open-simplex-3d/extrapolate (table xsb ysb zsb dx dy dz)
  (let ((index (lookup table zsb ysb xsb)))
    (+ (* (aref +open-simplex-3d/gradients+ index) dx)
       (* (aref +open-simplex-3d/gradients+ (1+ index)) dy)
       (* (aref +open-simplex-3d/gradients+ (+ index 2)) dz))))

(declaim (inline open-simplex-3d/contribute))
(defun open-simplex-3d/contribute (state dx dy dz xsb ysb zsb)
  (let ((a (- 2 (* dx dx) (* dy dy) (* dz dz))))
    (when (plusp a)
      (incf (oss3d-value state)
            (* (expt a 4)
               (open-simplex-3d/extrapolate (oss3d-table state) xsb ysb zsb dx dy dz))))
    (values)))

(defun open-simplex-3d/contribute1 (state)
  (let ((xsb (oss3d-xsb state))
        (ysb (oss3d-ysb state))
        (zsb (oss3d-zsb state)))
    (open-simplex-3d/contribute state
                                (oss3d-dx0 state)
                                (oss3d-dy0 state)
                                (oss3d-dz0 state)
                                xsb
                                ysb
                                zsb)
    (open-simplex-3d/contribute state
                                (oss3d-dx1 state)
                                (oss3d-dy1 state)
                                (oss3d-dz1 state)
                                (1+ xsb)
                                ysb
                                zsb)
    (open-simplex-3d/contribute state
                                (oss3d-dx2 state)
                                (oss3d-dy2 state)
                                (oss3d-dz2 state)
                                xsb
                                (1+ ysb)
                                zsb)
    (open-simplex-3d/contribute state
                                (oss3d-dx3 state)
                                (oss3d-dy3 state)
                                (oss3d-dz3 state)
                                xsb
                                ysb
                                (1+ zsb))))

(defun open-simplex-3d/contribute2 (state)
  (let ((xsb (oss3d-xsb state))
        (ysb (oss3d-ysb state))
        (zsb (oss3d-zsb state)))
    (open-simplex-3d/contribute state
                                (oss3d-dx3 state)
                                (oss3d-dy3 state)
                                (oss3d-dz3 state)
                                (1+ xsb)
                                (1+ ysb)
                                zsb)
    (open-simplex-3d/contribute state
                                (oss3d-dx2 state)
                                (oss3d-dy2 state)
                                (oss3d-dz2 state)
                                (1+ xsb)
                                ysb
                                (1+ zsb))
    (open-simplex-3d/contribute state
                                (oss3d-dx1 state)
                                (oss3d-dy1 state)
                                (oss3d-dz1 state)
                                xsb
                                (1+ ysb)
                                (1+ zsb))
    (open-simplex-3d/contribute state
                                (oss3d-dx0 state)
                                (oss3d-dy0 state)
                                (oss3d-dz0 state)
                                (1+ xsb)
                                (1+ ysb)
                                (1+ zsb))))

(defun open-simplex-3d/contribute3 (state)
  (let ((xsb (oss3d-xsb state))
        (ysb (oss3d-ysb state))
        (zsb (oss3d-zsb state)))
    (open-simplex-3d/contribute state
                                (oss3d-dx1 state)
                                (oss3d-dy1 state)
                                (oss3d-dz1 state)
                                (1+ xsb)
                                ysb
                                zsb)
    (open-simplex-3d/contribute state
                                (oss3d-dx2 state)
                                (oss3d-dy2 state)
                                (oss3d-dz2 state)
                                xsb
                                (1+ ysb)
                                zsb)
    (open-simplex-3d/contribute state
                                (oss3d-dx3 state)
                                (oss3d-dy3 state)
                                (oss3d-dz3 state)
                                xsb
                                ysb
                                (1+ zsb))
    (open-simplex-3d/contribute state
                                (oss3d-dx4 state)
                                (oss3d-dy4 state)
                                (oss3d-dz4 state)
                                (1+ xsb)
                                (1+ ysb)
                                zsb)
    (open-simplex-3d/contribute state
                                (oss3d-dx5 state)
                                (oss3d-dy5 state)
                                (oss3d-dz5 state)
                                (1+ xsb)
                                ysb
                                (1+ zsb))
    (open-simplex-3d/contribute state
                                (oss3d-dx6 state)
                                (oss3d-dy6 state)
                                (oss3d-dz6 state)
                                xsb
                                (1+ ysb)
                                (1+ zsb))))

(defun open-simplex-3d/contribute4 (state)
  (open-simplex-3d/contribute state
                              (oss3d-dx-ext0 state)
                              (oss3d-dy-ext0 state)
                              (oss3d-dz-ext0 state)
                              (oss3d-xsv-ext0 state)
                              (oss3d-ysv-ext0 state)
                              (oss3d-zsv-ext0 state))
  (open-simplex-3d/contribute state
                              (oss3d-dx-ext1 state)
                              (oss3d-dy-ext1 state)
                              (oss3d-dz-ext1 state)
                              (oss3d-xsv-ext1 state)
                              (oss3d-ysv-ext1 state)
                              (oss3d-zsv-ext1 state)))

(defun open-simplex-3d/in1 (state)
  (let* ((point-a 1)
         (point-b 2)
         (score-a (oss3d-xins state))
         (score-b (oss3d-yins state))
         (zins (oss3d-zins state))
         (wins (- 1 (oss3d-ins state)))
         (xsb (oss3d-xsb state))
         (ysb (oss3d-ysb state))
         (zsb (oss3d-zsb state))
         (dx0 (oss3d-dx0 state))
         (dy0 (oss3d-dy0 state))
         (dz0 (oss3d-dz0 state))
         (sq +open-simplex-3d/squish+)
         (sq2 #.(* +open-simplex-3d/squish+ 2)))
    (cond
      ((and (>= score-a score-b)
            (> zins score-b))
       (psetf score-b zins
              point-b 4))
      ((and (< score-a score-b)
            (> zins score-a))
       (psetf score-a zins
              point-a 4)))
    (if (or (> wins score-a)
            (> wins score-b))
        (let ((c (if (> score-b score-a) point-b point-a)))
          (if (zerop (logand c 1))
              (psetf (oss3d-xsv-ext0 state) (1- xsb)
                     (oss3d-xsv-ext1 state) xsb
                     (oss3d-dx-ext0 state) (1+ dx0)
                     (oss3d-dx-ext1 state) dx0)
              (let ((xsv (1+ xsb))
                    (dx (1- dx0)))
                (psetf (oss3d-xsv-ext0 state) xsv
                       (oss3d-xsv-ext1 state) xsv
                       (oss3d-dx-ext0 state) dx
                       (oss3d-dx-ext1 state) dx)))
          (cond
            ((zerop (logand c 2))
             (psetf (oss3d-ysv-ext0 state) ysb
                    (oss3d-ysv-ext1 state) ysb
                    (oss3d-dy-ext0 state) dy0
                    (oss3d-dy-ext1 state) dy0)
             (cond
               ((zerop (logand c 1))
                (decf (oss3d-ysv-ext1 state))
                (incf (oss3d-dy-ext1 state)))
               (t
                (decf (oss3d-ysv-ext0 state))
                (incf (oss3d-dy-ext0 state)))))
            (t
             (let ((ysv (1+ ysb))
                   (dy (1- dy0)))
               (psetf (oss3d-ysv-ext0 state) ysv
                      (oss3d-ysv-ext1 state) ysv
                      (oss3d-dy-ext0 state) dy
                      (oss3d-dy-ext1 state) dy))))
          (if (zerop (logand c 4))
              (psetf (oss3d-zsv-ext0 state) zsb
                     (oss3d-zsv-ext1 state) (1- zsb)
                     (oss3d-dz-ext0 state) dz0
                     (oss3d-dz-ext1 state) (1+ dz0))
              (let ((zsv (1+ zsb))
                    (dz (1- dz0)))
                (psetf (oss3d-zsv-ext0 state) zsv
                       (oss3d-zsv-ext1 state) zsv
                       (oss3d-dz-ext0 state) dz
                       (oss3d-dz-ext1 state) dz))))
        (let ((c (logand (logior point-a point-b) 255)))
          (if (zerop (logand c 1))
              (psetf (oss3d-xsv-ext0 state) xsb
                     (oss3d-xsv-ext1 state) (1- xsb)
                     (oss3d-dx-ext0 state) (- dx0 sq2)
                     (oss3d-dx-ext1 state) (- (1+ dx0) sq))
              (let ((xsv (1+ xsb)))
                (psetf (oss3d-xsv-ext0 state) xsv
                       (oss3d-xsv-ext1 state) xsv
                       (oss3d-dx-ext0 state) (- dx0 1 sq2)
                       (oss3d-dx-ext1 state) (- dx0 1 sq))))
          (if (zerop (logand c 2))
              (psetf (oss3d-ysv-ext0 state) ysb
                     (oss3d-ysv-ext1 state) (1- ysb)
                     (oss3d-dy-ext0 state) (- dy0 sq2)
                     (oss3d-dy-ext1 state) (- (1+ dy0) sq))
              (let ((ysv (1+ ysb)))
                (psetf (oss3d-ysv-ext0 state) ysv
                       (oss3d-ysv-ext1 state) ysv
                       (oss3d-dy-ext0 state) (- dy0 1 sq2)
                       (oss3d-dy-ext1 state) (- dy0 1 sq))))
          (if (zerop (logand c 4))
              (psetf (oss3d-zsv-ext0 state) zsb
                     (oss3d-zsv-ext1 state) (1- zsb)
                     (oss3d-dz-ext0 state) (- dz0 sq2)
                     (oss3d-dz-ext1 state) (- (1+ dz0) sq))
              (let ((zsv (1+ zsb)))
                (psetf (oss3d-zsv-ext0 state) zsv
                       (oss3d-zsv-ext1 state) zsv
                       (oss3d-dz-ext0 state) (- dz0 1 sq2)
                       (oss3d-dz-ext1 state) (- dz0 1 sq))))))
    (let ((dy1 (- dy0 sq))
          (dz1 (- dz0 sq))
          (dx2 (- dx0 sq)))
      (psetf (oss3d-dx1 state) (- dx0 1 sq)
             (oss3d-dy1 state) dy1
             (oss3d-dz1 state) dz1
             (oss3d-dx2 state) dx2
             (oss3d-dy2 state) (- dy0 1 sq)
             (oss3d-dz2 state) dz1
             (oss3d-dx3 state) dx2
             (oss3d-dy3 state) dy1
             (oss3d-dz3 state) (- dz0 1 sq)))
    (values)))

(defun open-simplex-3d/in2 (state)
  (let* ((point-a 6)
         (point-b 5)
         (score-a (oss3d-xins state))
         (score-b (oss3d-yins state))
         (zins (oss3d-zins state))
         (wins (- 3 (oss3d-ins state)))
         (xsb (oss3d-xsb state))
         (ysb (oss3d-ysb state))
         (zsb (oss3d-zsb state))
         (dx0 (oss3d-dx0 state))
         (dy0 (oss3d-dy0 state))
         (dz0 (oss3d-dz0 state))
         (sq +open-simplex-3d/squish+)
         (sq2 #.(* +open-simplex-3d/squish+ 2))
         (sq3 #.(* +open-simplex-3d/squish+ 3)))
    (cond
      ((and (<= score-a score-b)
            (< zins score-b))
       (psetf score-b zins
              point-b 3))
      ((and (> score-a score-b)
            (< zins score-a))
       (psetf score-a zins
              point-a 3)))
    (if (or (< wins score-a)
            (< wins score-b))
        (let ((c (if (< score-b score-a) point-b point-a)))
          (if (not (zerop (logand c 1)))
              (psetf (oss3d-xsv-ext0 state) (+ xsb 2)
                     (oss3d-xsv-ext1 state) (1+ xsb)
                     (oss3d-dx-ext0 state) (- dx0 2 sq3)
                     (oss3d-dx-ext1 state) (- dx0 1 sq3))
              (let ((dx (- dx0 sq3)))
                (psetf (oss3d-xsv-ext0 state) xsb
                       (oss3d-xsv-ext1 state) xsb
                       (oss3d-dx-ext0 state) dx
                       (oss3d-dx-ext1 state) dx)))
          (cond
            ((not (zerop (logand c 2)))
             (let ((ysv (1+ ysb))
                   (dy (- dy0 1 sq3)))
               (psetf (oss3d-ysv-ext0 state) ysv
                      (oss3d-ysv-ext1 state) ysv
                      (oss3d-dy-ext0 state) dy
                      (oss3d-dy-ext1 state) dy))
             (cond
               ((not (zerop (logand c 1)))
                (incf (oss3d-ysv-ext1 state))
                (decf (oss3d-dy-ext1 state)))
               (t
                (incf (oss3d-ysv-ext0 state))
                (decf (oss3d-dy-ext0 state)))))
            (t
             (let ((dy (- dy0 sq3)))
               (psetf (oss3d-ysv-ext0 state) ysb
                      (oss3d-ysv-ext1 state) ysb
                      (oss3d-dy-ext0 state) dy
                      (oss3d-dy-ext1 state) dy))))
          (if (not (zerop (logand c 4)))
              (psetf (oss3d-zsv-ext0 state) (1+ zsb)
                     (oss3d-zsv-ext1 state) (+ zsb 2)
                     (oss3d-dz-ext0 state) (- dz0 1 sq3)
                     (oss3d-dz-ext1 state) (- dz0 2 sq3))
              (let ((dz (- dz0 sq3)))
                (psetf (oss3d-zsv-ext0 state) zsb
                       (oss3d-zsv-ext1 state) zsb
                       (oss3d-dz-ext0 state) dz
                       (oss3d-dz-ext1 state) dz))))
        (let ((c (logand (logand point-a point-b) 255)))
          (if (not (zerop (logand c 1)))
              (psetf (oss3d-xsv-ext0 state) (1+ xsb)
                     (oss3d-xsv-ext1 state) (+ xsb 2)
                     (oss3d-dx-ext0 state) (- dx0 1 sq)
                     (oss3d-dx-ext1 state) (- dx0 2 sq2))
              (psetf (oss3d-xsv-ext0 state) xsb
                     (oss3d-xsv-ext1 state) xsb
                     (oss3d-dx-ext0 state) (- dx0 sq)
                     (oss3d-dx-ext1 state) (- dx0 sq2)))
          (if (not (zerop (logand c 2)))
              (psetf (oss3d-ysv-ext0 state) (1+ ysb)
                     (oss3d-ysv-ext1 state) (+ ysb 2)
                     (oss3d-dy-ext0 state) (- dy0 1 sq)
                     (oss3d-dy-ext1 state) (- dy0 2 sq2))
              (psetf (oss3d-ysv-ext0 state) ysb
                     (oss3d-ysv-ext1 state) ysb
                     (oss3d-dy-ext0 state) (- dy0 sq)
                     (oss3d-dy-ext1 state) (- dy0 sq2)))
          (if (not (zerop (logand c 4)))
              (psetf (oss3d-zsv-ext0 state) (1+ zsb)
                     (oss3d-zsv-ext1 state) (+ zsb 2)
                     (oss3d-dz-ext0 state) (- dz0 1 sq)
                     (oss3d-dz-ext1 state) (- dz0 2 sq2))
              (psetf (oss3d-zsv-ext0 state) zsb
                     (oss3d-zsv-ext1 state) zsb
                     (oss3d-dz-ext0 state) (- dz0 sq)
                     (oss3d-dz-ext1 state) (- dz0 sq2)))))
    (let ((dz2 (- dz0 1 sq2))
          (dx3 (- dx0 1 sq2))
          (dy3 (- dy0 1 sq2)))
      (psetf (oss3d-dx3 state) dx3
             (oss3d-dy3 state) dy3
             (oss3d-dz3 state) (- dz0 sq2)
             (oss3d-dx2 state) dx3
             (oss3d-dy2 state) (- dy0 sq2)
             (oss3d-dz2 state) dz2
             (oss3d-dx1 state) (- dx0 sq2)
             (oss3d-dy1 state) dy3
             (oss3d-dz1 state) dz2
             (oss3d-dx0 state) (- dx0 1 sq3)
             (oss3d-dy0 state) (- dy0 1 sq3)
             (oss3d-dz0 state) (- dz0 1 sq3)))
    (values)))

(defun open-simplex-3d/in3 (state)
  (let* ((point-a 0)
         (point-b 0)
         (score-a 0d0)
         (score-b 0d0)
         (a-farthest-p nil)
         (b-farthest-p nil)
         (xins (oss3d-xins state))
         (yins (oss3d-yins state))
         (zins (oss3d-zins state))
         (p1 (+ xins yins))
         (p2 (+ xins zins))
         (p3 (+ yins zins))
         (xsb (oss3d-xsb state))
         (ysb (oss3d-ysb state))
         (zsb (oss3d-zsb state))
         (dx0 (oss3d-dx0 state))
         (dy0 (oss3d-dy0 state))
         (dz0 (oss3d-dz0 state))
         (sq +open-simplex-3d/squish+)
         (sq2 #.(* +open-simplex-3d/squish+ 2))
         (sq3 #.(* +open-simplex-3d/squish+ 3)))
    (if (> p1 1)
        (psetf score-a (1- p1)
               point-a 3
               a-farthest-p t)
        (psetf score-a (- 1 p1)
               point-a 4
               a-farthest-p nil))
    (if (> p2 1)
        (psetf score-b (1- p2)
               point-b 5
               b-farthest-p t)
        (psetf score-b (- 1 p2)
               point-b 2
               b-farthest-p nil))
    (if (> p3 1)
        (let ((score (1- p3)))
          (cond
            ((and (<= score-a score-b)
                  (< score-a score))
             (psetf score-a score
                    point-a 6
                    a-farthest-p t))
            ((and (> score-a score-b)
                  (< score-b score))
             (psetf score-b score
                    point-b 6
                    b-farthest-p t))))
        (let ((score (- 1 p3)))
          (cond
            ((and (<= score-a score-b)
                  (< score-a score))
             (psetf score-a score
                    point-a 1
                    a-farthest-p nil))
            ((and (> score-a score-b)
                  (< score-b score))
             (psetf score-b score
                    point-b 1
                    b-farthest-p nil)))))
    (if (eq a-farthest-p b-farthest-p)
        (if a-farthest-p
            (let ((c (logand point-a point-b)))
              (psetf (oss3d-dx-ext0 state) (- dx0 1 sq3)
                     (oss3d-dy-ext0 state) (- dy0 1 sq3)
                     (oss3d-dz-ext0 state) (- dz0 1 sq3)
                     (oss3d-xsv-ext0 state) (1+ xsb)
                     (oss3d-ysv-ext0 state) (1+ ysb)
                     (oss3d-zsv-ext0 state) (1+ zsb))
              (cond
                ((not (zerop (logand c 1)))
                 (psetf (oss3d-dx-ext1 state) (- dx0 2 sq2)
                        (oss3d-dy-ext1 state) (- dy0 sq2)
                        (oss3d-dz-ext1 state) (- dz0 sq2)
                        (oss3d-xsv-ext1 state) (+ xsb 2)
                        (oss3d-ysv-ext1 state) ysb
                        (oss3d-zsv-ext1 state) zsb))
                ((not (zerop (logand c 2)))
                 (psetf (oss3d-dx-ext1 state) (- dx0 sq2)
                        (oss3d-dy-ext1 state) (- dy0 2 sq2)
                        (oss3d-dz-ext1 state) (- dz0 sq2)
                        (oss3d-xsv-ext1 state) xsb
                        (oss3d-ysv-ext1 state) (+ ysb 2)
                        (oss3d-zsv-ext1 state) zsb))
                (t
                 (psetf (oss3d-dx-ext1 state) (- dx0 sq2)
                        (oss3d-dy-ext1 state) (- dy0 sq2)
                        (oss3d-dz-ext1 state) (- dz0 2 sq2)
                        (oss3d-xsv-ext1 state) xsb
                        (oss3d-ysv-ext1 state) ysb
                        (oss3d-zsv-ext1 state) (+ zsb 2)))))
            (let ((c (logior point-a point-b)))
              (psetf (oss3d-dx-ext0 state) dx0
                     (oss3d-dy-ext0 state) dy0
                     (oss3d-dz-ext0 state) dz0
                     (oss3d-xsv-ext0 state) xsb
                     (oss3d-ysv-ext0 state) ysb
                     (oss3d-zsv-ext0 state) zsb)
              (cond
                ((zerop (logand c 1))
                 (psetf (oss3d-dx-ext1 state) (- (1+ dx0) sq)
                        (oss3d-dy-ext1 state) (- dy0 1 sq)
                        (oss3d-dz-ext1 state) (- dz0 1 sq)
                        (oss3d-xsv-ext1 state) (1- xsb)
                        (oss3d-ysv-ext1 state) (1+ ysb)
                        (oss3d-zsv-ext1 state) (1+ zsb)))
                ((zerop (logand c 2))
                 (psetf (oss3d-dx-ext1 state) (- dx0 1 sq)
                        (oss3d-dy-ext1 state) (- (1+ dy0) sq)
                        (oss3d-dz-ext1 state) (- dz0 1 sq)
                        (oss3d-xsv-ext1 state) (1+ xsb)
                        (oss3d-ysv-ext1 state) (1- ysb)
                        (oss3d-zsv-ext1 state) (1+ zsb)))
                (t
                 (psetf (oss3d-dx-ext1 state) (- dx0 1 sq)
                        (oss3d-dy-ext1 state) (- dy0 1 sq)
                        (oss3d-dz-ext1 state) (- (1+ dz0) sq)
                        (oss3d-xsv-ext1 state) (1+ xsb)
                        (oss3d-ysv-ext1 state) (1+ ysb)
                        (oss3d-zsv-ext1 state) (1- zsb))))))
        (let ((c1 (if a-farthest-p point-a point-b))
              (c2 (if a-farthest-p point-b point-a)))
          (cond
            ((zerop (logand c1 1))
             (psetf (oss3d-dx-ext0 state) (- (1+ dx0) sq)
                    (oss3d-dy-ext0 state) (- dy0 1 sq)
                    (oss3d-dz-ext0 state) (- dz0 1 sq)
                    (oss3d-xsv-ext0 state) (1- xsb)
                    (oss3d-ysv-ext0 state) (1+ ysb)
                    (oss3d-zsv-ext0 state) (1+ zsb)))
            ((zerop (logand c1 2))
             (psetf (oss3d-dx-ext0 state) (- dx0 1 sq)
                    (oss3d-dy-ext0 state) (- (1+ dy0) sq)
                    (oss3d-dz-ext0 state) (- dz0 1 sq)
                    (oss3d-xsv-ext0 state) (1+ xsb)
                    (oss3d-ysv-ext0 state) (1- ysb)
                    (oss3d-zsv-ext0 state) (1+ zsb)))
            (t
             (psetf (oss3d-dx-ext0 state) (- dx0 1 sq)
                    (oss3d-dy-ext0 state) (- dy0 1 sq)
                    (oss3d-dz-ext0 state) (- (1+ dz0) sq)
                    (oss3d-xsv-ext0 state) (1+ xsb)
                    (oss3d-ysv-ext0 state) (1+ ysb)
                    (oss3d-zsv-ext0 state) (1- zsb))))
          (psetf (oss3d-dx-ext1 state) (- dx0 sq2)
                 (oss3d-dy-ext1 state) (- dy0 sq2)
                 (oss3d-dz-ext1 state) (- dz0 sq2)
                 (oss3d-xsv-ext1 state) xsb
                 (oss3d-ysv-ext1 state) ysb
                 (oss3d-zsv-ext1 state) zsb)
          (cond
            ((not (zerop (logand c2 1)))
             (decf (oss3d-dx-ext1 state) 2)
             (incf (oss3d-xsv-ext1 state) 2))
            ((not (zerop (logand c2 2)))
             (decf (oss3d-dy-ext1 state) 2)
             (incf (oss3d-ysv-ext1 state) 2))
            (t
             (decf (oss3d-dz-ext1 state) 2)
             (incf (oss3d-zsv-ext1 state) 2)))))
    (let ((dy1 (- dy0 sq))
          (dz1 (- dz0 sq))
          (dx2 (- dx0 sq))
          (dx4 (- dx0 1 sq2))
          (dy4 (- dy0 1 sq2))
          (dz5 (- dz0 1 sq2)))
      (psetf (oss3d-dx1 state) (- dx0 1 sq)
             (oss3d-dy1 state) dy1
             (oss3d-dz1 state) dz1
             (oss3d-dx2 state) dx2
             (oss3d-dy2 state) (- dy0 1 sq)
             (oss3d-dz2 state) dz1
             (oss3d-dx3 state) dx2
             (oss3d-dy3 state) dy1
             (oss3d-dz3 state) (- dz0 1 sq)
             (oss3d-dx4 state) dx4
             (oss3d-dy4 state) dy4
             (oss3d-dz4 state) (- dz0 sq2)
             (oss3d-dx5 state) dx4
             (oss3d-dy5 state) (- dy0 sq2)
             (oss3d-dz5 state) dz5
             (oss3d-dx6 state) (- dx0 sq2)
             (oss3d-dy6 state) dy4
             (oss3d-dz6 state) dz5))
    (values)))

(declaim (inline %open-simplex-3d))
(defun %open-simplex-3d (table x y z)
  (declare (optimize speed)
           (u:f64 x y z))
  (let ((state (make-open-simplex-3d-state table x y z)))
    (cond
      ((<= (oss3d-ins state) 1)
       (open-simplex-3d/in1 state)
       (open-simplex-3d/contribute1 state))
      ((>= (oss3d-ins state) 2)
       (open-simplex-3d/in2 state)
       (open-simplex-3d/contribute2 state))
      (t
       (open-simplex-3d/in3 state)
       (open-simplex-3d/contribute3 state)))
    (open-simplex-3d/contribute4 state)
    (float (* (oss3d-value state) +open-simplex-3d/scale+) 1f0)))

;;; 4D OpenSimplex

(u:define-constant +open-simplex-4d/stretch+ (/ (1- (/ (sqrt 5d0))) 4))

(u:define-constant +open-simplex-4d/squish+ (/ (1- (sqrt 5d0)) 4))

(u:define-constant +open-simplex-4d/scale+ (/ 30d0))

(u:define-constant +open-simplex-4d/gradients+
    (let ((data '(3 1 1 1 1 3 1 1 1 1 3 1 1 1 1 3 -3 1 1 1 -1 3 1 1 -1 1 3 1 -1 1 1 3 3 -1 1 1 1 -3 1
                  1 1 -1 3 1 1 -1 1 3 -3 -1 1 1 -1 -3 1 1 -1 -1 3 1 -1 -1 1 3 3 1 -1 1 1 3 -1 1 1 1
                  -3 1 1 1 -1 3 -3 1 1 1 -1 3 -1 1 -1 1 -3 1 -1 1 -1 3 3 -1 -1 1 1 -3 -1 1 1 -1 -3 1
                  1 -1 -1 3 -3 -1 -1 1 -1 -3 -1 1 -1 -1 -3 1 -1 -1 -1 3 3 1 1 -1 1 3 1 -1 1 1 3 -1 1
                  1 1 -3 -3 1 1 -1 -1 3 1 -1 -1 1 3 -1 -1 1 1 -3 3 -1 1 -1 1 -3 1 -1 1 -1 3 -1 1 -1 1
                  -3 -3 -1 1 -1 -1 3 1 -1 -1 -1 3 -1 -1 -1 1 -3 3 1 -1 -1 1 3 -1 -1 1 1 -3 -1 1 1 -1
                  -3 -3 1 -1 -1 -1 3 -1 -1 -1 1 -3 -1 -1 1 -1 -3 3 -1 -1 -1 1 -3 -1 -1 1 -1 -3 -1 1
                  -1 -1 -3 -3 -1 -1 -1 -1 -3 -1 -1 -1 -1 3 -1 -1 -1 -1 -3)))
      (make-array 256 :element-type 'fixnum :initial-contents data))
  :test #'equalp)

(declaim (inline %make-open-simplex-4d-state))
(defstruct (open-simplex-4d-state
            (:constructor %make-open-simplex-4d-state)
            (:conc-name oss4d-)
            (:predicate nil)
            (:copier nil))
  (table +perlin/permutation+ :type ub8-512)
  (stretch-offset 0d0 :type u:f64)
  (xsb 0 :type fixnum)
  (ysb 0 :type fixnum)
  (zsb 0 :type fixnum)
  (wsb 0 :type fixnum)
  (dx0 0d0 :type u:f64)
  (dy0 0d0 :type u:f64)
  (dz0 0d0 :type u:f64)
  (dw0 0d0 :type u:f64)
  (dx1 0d0 :type u:f64)
  (dy1 0d0 :type u:f64)
  (dz1 0d0 :type u:f64)
  (dw1 0d0 :type u:f64)
  (dx2 0d0 :type u:f64)
  (dy2 0d0 :type u:f64)
  (dz2 0d0 :type u:f64)
  (dw2 0d0 :type u:f64)
  (dx3 0d0 :type u:f64)
  (dy3 0d0 :type u:f64)
  (dz3 0d0 :type u:f64)
  (dw3 0d0 :type u:f64)
  (dx4 0d0 :type u:f64)
  (dy4 0d0 :type u:f64)
  (dz4 0d0 :type u:f64)
  (dw4 0d0 :type u:f64)
  (dx5 0d0 :type u:f64)
  (dy5 0d0 :type u:f64)
  (dz5 0d0 :type u:f64)
  (dw5 0d0 :type u:f64)
  (dx6 0d0 :type u:f64)
  (dy6 0d0 :type u:f64)
  (dz6 0d0 :type u:f64)
  (dw6 0d0 :type u:f64)
  (dx7 0d0 :type u:f64)
  (dy7 0d0 :type u:f64)
  (dz7 0d0 :type u:f64)
  (dw7 0d0 :type u:f64)
  (dx8 0d0 :type u:f64)
  (dy8 0d0 :type u:f64)
  (dz8 0d0 :type u:f64)
  (dw8 0d0 :type u:f64)
  (dx9 0d0 :type u:f64)
  (dy9 0d0 :type u:f64)
  (dz9 0d0 :type u:f64)
  (dw9 0d0 :type u:f64)
  (dx10 0d0 :type u:f64)
  (dy10 0d0 :type u:f64)
  (dz10 0d0 :type u:f64)
  (dw10 0d0 :type u:f64)
  (dx-ext0 0d0 :type u:f64)
  (dy-ext0 0d0 :type u:f64)
  (dz-ext0 0d0 :type u:f64)
  (dw-ext0 0d0 :type u:f64)
  (dx-ext1 0d0 :type u:f64)
  (dy-ext1 0d0 :type u:f64)
  (dz-ext1 0d0 :type u:f64)
  (dw-ext1 0d0 :type u:f64)
  (dx-ext2 0d0 :type u:f64)
  (dy-ext2 0d0 :type u:f64)
  (dz-ext2 0d0 :type u:f64)
  (dw-ext2 0d0 :type u:f64)
  (xsv-ext0 0 :type fixnum)
  (ysv-ext0 0 :type fixnum)
  (zsv-ext0 0 :type fixnum)
  (wsv-ext0 0 :type fixnum)
  (xsv-ext1 0 :type fixnum)
  (ysv-ext1 0 :type fixnum)
  (zsv-ext1 0 :type fixnum)
  (wsv-ext1 0 :type fixnum)
  (xsv-ext2 0 :type fixnum)
  (ysv-ext2 0 :type fixnum)
  (zsv-ext2 0 :type fixnum)
  (wsv-ext2 0 :type fixnum)
  (xins 0d0 :type u:f64)
  (yins 0d0 :type u:f64)
  (zins 0d0 :type u:f64)
  (wins 0d0 :type u:f64)
  (ins 0d0 :type u:f64)
  (value 0d0 :type u:f64))

(declaim (inline make-open-simplex-4d-state))
(defun make-open-simplex-4d-state (table x y z w)
  (let* ((stretch-offset (* (+ x y z w) +open-simplex-4d/stretch+))
         (xs (+ x stretch-offset))
         (ys (+ y stretch-offset))
         (zs (+ z stretch-offset))
         (ws (+ w stretch-offset))
         (xsb (floor xs))
         (ysb (floor ys))
         (zsb (floor zs))
         (wsb (floor ws))
         (squish-offset (* (+ xsb ysb zsb wsb) +open-simplex-4d/squish+))
         (dx0 (- x (+ xsb squish-offset)))
         (dy0 (- y (+ ysb squish-offset)))
         (dz0 (- z (+ zsb squish-offset)))
         (dw0 (- w (+ wsb squish-offset)))
         (xins (- xs xsb))
         (yins (- ys ysb))
         (zins (- zs zsb))
         (wins (- ws wsb)))
    (declare (f50 xs ys zs ws))
    (%make-open-simplex-4d-state :table table
                                 :xsb xsb
                                 :ysb ysb
                                 :zsb zsb
                                 :wsb wsb
                                 :dx0 dx0
                                 :dy0 dy0
                                 :dz0 dz0
                                 :dw0 dw0
                                 :xins xins
                                 :yins yins
                                 :zins zins
                                 :wins wins
                                 :ins (+ xins yins zins wins))))

(declaim (inline open-simplex-4d/contribute))
(defun open-simplex-4d/contribute (state dx dy dz dw xsb ysb zsb wsb)
  (let ((index (logand (lookup (oss4d-table state) wsb zsb ysb xsb) 252))
        (a (- 2 (* dx dx) (* dy dy) (* dz dz) (* dw dw))))
    (when (plusp a)
      (incf (oss4d-value state)
            (* (expt a 4)
               (+ (* (aref +open-simplex-4d/gradients+ index) dx)
                  (* (aref +open-simplex-4d/gradients+ (1+ index)) dy)
                  (* (aref +open-simplex-4d/gradients+ (+ index 2)) dz)
                  (* (aref +open-simplex-4d/gradients+ (+ index 3)) dw)))))
    (values)))

(defun open-simplex-4d/contribute1 (state)
  (let ((xsb (oss4d-xsb state))
        (ysb (oss4d-ysb state))
        (zsb (oss4d-zsb state))
        (wsb (oss4d-wsb state)))
    (open-simplex-4d/contribute state
                                (oss4d-dx0 state)
                                (oss4d-dy0 state)
                                (oss4d-dz0 state)
                                (oss4d-dw0 state)
                                xsb
                                ysb
                                zsb
                                wsb)
    (open-simplex-4d/contribute state
                                (oss4d-dx1 state)
                                (oss4d-dy1 state)
                                (oss4d-dz1 state)
                                (oss4d-dw1 state)
                                (1+ xsb)
                                ysb
                                zsb
                                wsb)
    (open-simplex-4d/contribute state
                                (oss4d-dx2 state)
                                (oss4d-dy2 state)
                                (oss4d-dz2 state)
                                (oss4d-dw2 state)
                                xsb
                                (1+ ysb)
                                zsb
                                wsb)
    (open-simplex-4d/contribute state
                                (oss4d-dx3 state)
                                (oss4d-dy3 state)
                                (oss4d-dz3 state)
                                (oss4d-dw3 state)
                                xsb
                                ysb
                                (1+ zsb)
                                wsb)
    (open-simplex-4d/contribute state
                                (oss4d-dx4 state)
                                (oss4d-dy4 state)
                                (oss4d-dz4 state)
                                (oss4d-dw4 state)
                                xsb
                                ysb
                                zsb
                                (1+ wsb))))

(defun open-simplex-4d/contribute2 (state)
  (let ((xsb (oss4d-xsb state))
        (ysb (oss4d-ysb state))
        (zsb (oss4d-zsb state))
        (wsb (oss4d-wsb state)))
    (open-simplex-4d/contribute state
                                (oss4d-dx4 state)
                                (oss4d-dy4 state)
                                (oss4d-dz4 state)
                                (oss4d-dw4 state)
                                (1+ xsb)
                                (1+ ysb)
                                (1+ zsb)
                                wsb)
    (open-simplex-4d/contribute state
                                (oss4d-dx3 state)
                                (oss4d-dy3 state)
                                (oss4d-dz3 state)
                                (oss4d-dw3 state)
                                (1+ xsb)
                                (1+ ysb)
                                zsb
                                (1+ wsb))
    (open-simplex-4d/contribute state
                                (oss4d-dx2 state)
                                (oss4d-dy2 state)
                                (oss4d-dz2 state)
                                (oss4d-dw2 state)
                                (1+ xsb)
                                ysb
                                (1+ zsb)
                                (1+ wsb))
    (open-simplex-4d/contribute state
                                (oss4d-dx1 state)
                                (oss4d-dy1 state)
                                (oss4d-dz1 state)
                                (oss4d-dw1 state)
                                xsb
                                (1+ ysb)
                                (1+ zsb)
                                (1+ wsb))
    (open-simplex-4d/contribute state
                                (oss4d-dx0 state)
                                (oss4d-dy0 state)
                                (oss4d-dz0 state)
                                (oss4d-dw0 state)
                                (1+ xsb)
                                (1+ ysb)
                                (1+ zsb)
                                (1+ wsb))))

(defun open-simplex-4d/contribute3 (state)
  (let ((xsb (oss4d-xsb state))
        (ysb (oss4d-ysb state))
        (zsb (oss4d-zsb state))
        (wsb (oss4d-wsb state)))
    (open-simplex-4d/contribute state
                                (oss4d-dx1 state)
                                (oss4d-dy1 state)
                                (oss4d-dz1 state)
                                (oss4d-dw1 state)
                                (1+ xsb)
                                ysb
                                zsb
                                wsb)
    (open-simplex-4d/contribute state
                                (oss4d-dx2 state)
                                (oss4d-dy2 state)
                                (oss4d-dz2 state)
                                (oss4d-dw2 state)
                                xsb
                                (1+ ysb)
                                zsb
                                wsb)
    (open-simplex-4d/contribute state
                                (oss4d-dx3 state)
                                (oss4d-dy3 state)
                                (oss4d-dz3 state)
                                (oss4d-dw3 state)
                                xsb
                                ysb
                                (1+ zsb)
                                wsb)
    (open-simplex-4d/contribute state
                                (oss4d-dx4 state)
                                (oss4d-dy4 state)
                                (oss4d-dz4 state)
                                (oss4d-dw4 state)
                                xsb
                                ysb
                                zsb
                                (1+ wsb))
    (open-simplex-4d/contribute state
                                (oss4d-dx5 state)
                                (oss4d-dy5 state)
                                (oss4d-dz5 state)
                                (oss4d-dw5 state)
                                (1+ xsb)
                                (1+ ysb)
                                zsb
                                wsb)
    (open-simplex-4d/contribute state
                                (oss4d-dx6 state)
                                (oss4d-dy6 state)
                                (oss4d-dz6 state)
                                (oss4d-dw6 state)
                                (1+ xsb)
                                ysb
                                (1+ zsb)
                                wsb)
    (open-simplex-4d/contribute state
                                (oss4d-dx7 state)
                                (oss4d-dy7 state)
                                (oss4d-dz7 state)
                                (oss4d-dw7 state)
                                (1+ xsb)
                                ysb
                                zsb
                                (1+ wsb))
    (open-simplex-4d/contribute state
                                (oss4d-dx8 state)
                                (oss4d-dy8 state)
                                (oss4d-dz8 state)
                                (oss4d-dw8 state)
                                xsb
                                (1+ ysb)
                                (1+ zsb)
                                wsb)
    (open-simplex-4d/contribute state
                                (oss4d-dx9 state)
                                (oss4d-dy9 state)
                                (oss4d-dz9 state)
                                (oss4d-dw9 state)
                                xsb
                                (1+ ysb)
                                zsb
                                (1+ wsb))
    (open-simplex-4d/contribute state
                                (oss4d-dx10 state)
                                (oss4d-dy10 state)
                                (oss4d-dz10 state)
                                (oss4d-dw10 state)
                                xsb
                                ysb
                                (1+ zsb)
                                (1+ wsb))))

(defun open-simplex-4d/contribute4 (state)
  (let ((xsb (oss4d-xsb state))
        (ysb (oss4d-ysb state))
        (zsb (oss4d-zsb state))
        (wsb (oss4d-wsb state)))
    (open-simplex-4d/contribute state
                                (oss4d-dx4 state)
                                (oss4d-dy4 state)
                                (oss4d-dz4 state)
                                (oss4d-dw4 state)
                                (1+ xsb)
                                (1+ ysb)
                                (1+ zsb)
                                wsb)
    (open-simplex-4d/contribute state
                                (oss4d-dx3 state)
                                (oss4d-dy3 state)
                                (oss4d-dz3 state)
                                (oss4d-dw3 state)
                                (1+ xsb)
                                (1+ ysb)
                                zsb
                                (1+ wsb))
    (open-simplex-4d/contribute state
                                (oss4d-dx2 state)
                                (oss4d-dy2 state)
                                (oss4d-dz2 state)
                                (oss4d-dw2 state)
                                (1+ xsb)
                                ysb
                                (1+ zsb)
                                (1+ wsb))
    (open-simplex-4d/contribute state
                                (oss4d-dx1 state)
                                (oss4d-dy1 state)
                                (oss4d-dz1 state)
                                (oss4d-dw1 state)
                                xsb
                                (1+ ysb)
                                (1+ zsb)
                                (1+ wsb))
    (open-simplex-4d/contribute state
                                (oss4d-dx5 state)
                                (oss4d-dy5 state)
                                (oss4d-dz5 state)
                                (oss4d-dw5 state)
                                (1+ xsb)
                                (1+ ysb)
                                zsb
                                wsb)
    (open-simplex-4d/contribute state
                                (oss4d-dx6 state)
                                (oss4d-dy6 state)
                                (oss4d-dz6 state)
                                (oss4d-dw6 state)
                                (1+ xsb)
                                ysb
                                (1+ zsb)
                                wsb)
    (open-simplex-4d/contribute state
                                (oss4d-dx7 state)
                                (oss4d-dy7 state)
                                (oss4d-dz7 state)
                                (oss4d-dw7 state)
                                (1+ xsb)
                                ysb
                                zsb
                                (1+ wsb))
    (open-simplex-4d/contribute state
                                (oss4d-dx8 state)
                                (oss4d-dy8 state)
                                (oss4d-dz8 state)
                                (oss4d-dw8 state)
                                xsb
                                (1+ ysb)
                                (1+ zsb)
                                wsb)
    (open-simplex-4d/contribute state
                                (oss4d-dx9 state)
                                (oss4d-dy9 state)
                                (oss4d-dz9 state)
                                (oss4d-dw9 state)
                                xsb
                                (1+ ysb)
                                zsb
                                (1+ wsb))
    (open-simplex-4d/contribute state
                                (oss4d-dx10 state)
                                (oss4d-dy10 state)
                                (oss4d-dz10 state)
                                (oss4d-dw10 state)
                                xsb
                                ysb
                                (1+ zsb)
                                (1+ wsb))))

(defun open-simplex-4d/contribute5 (state)
  (open-simplex-4d/contribute state
                              (oss4d-dx-ext0 state)
                              (oss4d-dy-ext0 state)
                              (oss4d-dz-ext0 state)
                              (oss4d-dw-ext0 state)
                              (oss4d-xsv-ext0 state)
                              (oss4d-ysv-ext0 state)
                              (oss4d-zsv-ext0 state)
                              (oss4d-wsv-ext0 state))
  (open-simplex-4d/contribute state
                              (oss4d-dx-ext1 state)
                              (oss4d-dy-ext1 state)
                              (oss4d-dz-ext1 state)
                              (oss4d-dw-ext1 state)
                              (oss4d-xsv-ext1 state)
                              (oss4d-ysv-ext1 state)
                              (oss4d-zsv-ext1 state)
                              (oss4d-wsv-ext1 state))
  (open-simplex-4d/contribute state
                              (oss4d-dx-ext2 state)
                              (oss4d-dy-ext2 state)
                              (oss4d-dz-ext2 state)
                              (oss4d-dw-ext2 state)
                              (oss4d-xsv-ext2 state)
                              (oss4d-ysv-ext2 state)
                              (oss4d-zsv-ext2 state)
                              (oss4d-wsv-ext2 state)))

(defun open-simplex-4d/in1 (state)
  (let* ((point-a 1)
         (point-b 2)
         (score-a (oss4d-xins state))
         (score-b (oss4d-yins state))
         (zins (oss4d-zins state))
         (wins (oss4d-wins state))
         (uins (- 1 (oss4d-ins state)))
         (xsb (oss4d-xsb state))
         (ysb (oss4d-ysb state))
         (zsb (oss4d-zsb state))
         (wsb (oss4d-wsb state))
         (dx0 (oss4d-dx0 state))
         (dy0 (oss4d-dy0 state))
         (dz0 (oss4d-dz0 state))
         (dw0 (oss4d-dw0 state))
         (sq +open-simplex-4d/squish+)
         (sq2 #.(* +open-simplex-4d/squish+ 2)))
    (cond
      ((and (>= score-a score-b)
            (> zins score-b))
       (psetf score-b zins
              point-b 4))
      ((and (< score-a score-b)
            (> zins score-a))
       (psetf score-a zins
              point-a 4)))
    (cond
      ((and (>= score-a score-b)
            (> wins score-b))
       (psetf score-b wins
              point-b 8))
      ((and (< score-a score-b)
            (> wins score-a))
       (psetf score-a wins
              point-a 8)))
    (if (or (> uins score-a)
            (> uins score-b))
        (let ((c (if (> score-b score-a) point-b point-a)))
          (if (zerop (logand c 1))
              (psetf (oss4d-xsv-ext0 state) (1- xsb)
                     (oss4d-xsv-ext1 state) xsb
                     (oss4d-xsv-ext2 state) xsb
                     (oss4d-dx-ext0 state) (1+ dx0)
                     (oss4d-dx-ext1 state) dx0
                     (oss4d-dx-ext2 state) dx0)
              (let ((xsv (1+ xsb))
                    (dx (1- dx0)))
                (psetf (oss4d-xsv-ext0 state) xsv
                       (oss4d-xsv-ext1 state) xsv
                       (oss4d-xsv-ext2 state) xsv
                       (oss4d-dx-ext0 state) dx
                       (oss4d-dx-ext1 state) dx
                       (oss4d-dx-ext2 state) dx)))
          (cond
            ((zerop (logand c 2))
             (psetf (oss4d-ysv-ext0 state) ysb
                    (oss4d-ysv-ext1 state) ysb
                    (oss4d-ysv-ext2 state) ysb
                    (oss4d-dy-ext0 state) dy0
                    (oss4d-dy-ext1 state) dy0
                    (oss4d-dy-ext2 state) dy0)
             (cond
               ((= (logand c 1) 1)
                (decf (oss4d-ysv-ext0 state))
                (incf (oss4d-dy-ext0 state)))
               (t
                (decf (oss4d-ysv-ext1 state))
                (incf (oss4d-dy-ext1 state)))))
            (t
             (let ((ysv (1+ ysb))
                   (dy (1- dy0)))
               (psetf (oss4d-ysv-ext0 state) ysv
                      (oss4d-ysv-ext1 state) ysv
                      (oss4d-ysv-ext2 state) ysv
                      (oss4d-dy-ext0 state) dy
                      (oss4d-dy-ext1 state) dy
                      (oss4d-dy-ext2 state) dy))))
          (cond
            ((zerop (logand c 4))
             (psetf (oss4d-zsv-ext0 state) zsb
                    (oss4d-zsv-ext1 state) zsb
                    (oss4d-zsv-ext2 state) zsb
                    (oss4d-dz-ext0 state) dz0
                    (oss4d-dz-ext1 state) dz0
                    (oss4d-dz-ext2 state) dz0)
             (cond
               ((not (zerop (logand c 3)))
                (unless (= (logand c 3) 3)
                  (decf (oss4d-zsv-ext1 state))
                  (incf (oss4d-dz-ext1 state))))
               (t
                (decf (oss4d-zsv-ext2 state))
                (incf (oss4d-dz-ext2 state)))))
            (t
             (let ((zsv (1+ zsb))
                   (dz (1- dz0)))
               (psetf (oss4d-zsv-ext0 state) zsv
                      (oss4d-zsv-ext1 state) zsv
                      (oss4d-zsv-ext2 state) zsv
                      (oss4d-dz-ext0 state) dz
                      (oss4d-dz-ext1 state) dz
                      (oss4d-dz-ext2 state) dz))))
          (if (zerop (logand c 8))
              (psetf (oss4d-wsv-ext0 state) wsb
                     (oss4d-wsv-ext1 state) wsb
                     (oss4d-wsv-ext2 state) (1- wsb)
                     (oss4d-dw-ext0 state) dw0
                     (oss4d-dw-ext1 state) dw0
                     (oss4d-dw-ext2 state) (1+ dw0))
              (let ((wsv (1+ wsb))
                    (dw (1- dw0)))
                (psetf (oss4d-wsv-ext0 state) wsv
                       (oss4d-wsv-ext1 state) wsv
                       (oss4d-wsv-ext2 state) wsv
                       (oss4d-dw-ext0 state) dw
                       (oss4d-dw-ext1 state) dw
                       (oss4d-dw-ext2 state) dw))))
        (let ((c (logior point-a point-b)))
          (if (zerop (logand c 1))
              (psetf (oss4d-xsv-ext0 state) xsb
                     (oss4d-xsv-ext1 state) (1- xsb)
                     (oss4d-xsv-ext2 state) xsb
                     (oss4d-dx-ext0 state) (- dx0 sq2)
                     (oss4d-dx-ext1 state) (- (1+ dx0) sq)
                     (oss4d-dx-ext2 state) (- dx0 sq))
              (let ((xsv (1+ xsb))
                    (dx (- dx0 1 sq)))
                (psetf (oss4d-xsv-ext0 state) xsv
                       (oss4d-xsv-ext1 state) xsv
                       (oss4d-xsv-ext2 state) xsv
                       (oss4d-dx-ext0 state) (- dx0 1 sq2)
                       (oss4d-dx-ext1 state) dx
                       (oss4d-dx-ext2 state) dx)))
          (cond
            ((zerop (logand c 2))
             (let ((dy (- dy0 sq)))
               (psetf (oss4d-ysv-ext0 state) ysb
                      (oss4d-ysv-ext1 state) ysb
                      (oss4d-ysv-ext2 state) ysb
                      (oss4d-dy-ext0 state) (- dy0 sq2)
                      (oss4d-dy-ext1 state) dy
                      (oss4d-dy-ext2 state) dy))
             (cond
               ((= (logand c 1) 1)
                (decf (oss4d-ysv-ext1 state))
                (incf (oss4d-dy-ext1 state)))
               (t
                (decf (oss4d-ysv-ext2 state))
                (incf (oss4d-dy-ext2 state)))))
            (t
             (let ((ysv (1+ ysb))
                   (dy (- dy0 1 sq)))
               (psetf (oss4d-ysv-ext0 state) ysv
                      (oss4d-ysv-ext1 state) ysv
                      (oss4d-ysv-ext2 state) ysv
                      (oss4d-dy-ext0 state) (- dy0 1 sq2)
                      (oss4d-dy-ext1 state) dy
                      (oss4d-dy-ext2 state) dy))))
          (cond
            ((zerop (logand c 4))
             (let ((dz (- dz0 sq)))
               (psetf (oss4d-zsv-ext0 state) zsb
                      (oss4d-zsv-ext1 state) zsb
                      (oss4d-zsv-ext2 state) zsb
                      (oss4d-dz-ext0 state) (- dz0 sq2)
                      (oss4d-dz-ext1 state) dz
                      (oss4d-dz-ext2 state) dz))
             (cond
               ((= (logand c 3) 3)
                (decf (oss4d-zsv-ext1 state))
                (incf (oss4d-dz-ext1 state)))
               (t
                (decf (oss4d-zsv-ext2 state))
                (incf (oss4d-dz-ext2 state)))))
            (t
             (let ((zsv (1+ zsb))
                   (dz (- dz0 1 sq)))
               (psetf (oss4d-zsv-ext0 state) zsv
                      (oss4d-zsv-ext1 state) zsv
                      (oss4d-zsv-ext2 state) zsv
                      (oss4d-dz-ext0 state) (- dz0 1 sq2)
                      (oss4d-dz-ext1 state) dz
                      (oss4d-dz-ext2 state) dz))))
          (cond
            ((zerop (logand c 8))
             (psetf (oss4d-wsv-ext0 state) wsb
                    (oss4d-wsv-ext1 state) wsb
                    (oss4d-wsv-ext2 state) (1- wsb)
                    (oss4d-dw-ext0 state) (- dw0 sq2)
                    (oss4d-dw-ext1 state) (- dw0 sq)
                    (oss4d-dw-ext2 state) (- (1+ dw0) sq)))
            (t
             (let ((wsv (1+ wsb))
                   (dw (- dw0 1 sq)))
               (psetf (oss4d-wsv-ext0 state) wsv
                      (oss4d-wsv-ext1 state) wsv
                      (oss4d-wsv-ext2 state) wsv
                      (oss4d-dw-ext0 state) (- dw0 1 sq2)
                      (oss4d-dw-ext1 state) dw
                      (oss4d-dw-ext2 state) dw))))))
    (let ((dy1 (- dy0 sq))
          (dz1 (- dz0 sq))
          (dw1 (- dw0 sq))
          (dx2 (- dx0 sq)))
      (psetf (oss4d-dx1 state) (- dx0 1 sq)
             (oss4d-dy1 state) dy1
             (oss4d-dz1 state) dz1
             (oss4d-dw1 state) dw1
             (oss4d-dx2 state) dx2
             (oss4d-dy2 state) (- dy0 1 sq)
             (oss4d-dz2 state) dz1
             (oss4d-dw2 state) dw1
             (oss4d-dx3 state) dx2
             (oss4d-dy3 state) dy1
             (oss4d-dz3 state) (- dz0 1 sq)
             (oss4d-dw3 state) dw1
             (oss4d-dx4 state) dx2
             (oss4d-dy4 state) dy1
             (oss4d-dz4 state) dz1
             (oss4d-dw4 state) (- dw0 1 sq)))
    (values)))

(defun open-simplex-4d/in2 (state)
  (let* ((point-a 14)
         (point-b 13)
         (score-a (oss4d-xins state))
         (score-b (oss4d-yins state))
         (zins (oss4d-zins state))
         (wins (oss4d-wins state))
         (uins (- 4 (oss4d-ins state)))
         (xsb (oss4d-xsb state))
         (ysb (oss4d-ysb state))
         (zsb (oss4d-zsb state))
         (wsb (oss4d-wsb state))
         (dx0 (oss4d-dx0 state))
         (dy0 (oss4d-dy0 state))
         (dz0 (oss4d-dz0 state))
         (dw0 (oss4d-dw0 state))
         (sq2 #.(* +open-simplex-4d/squish+ 2))
         (sq3 #.(* +open-simplex-4d/squish+ 3))
         (sq4 #.(* +open-simplex-4d/squish+ 4)))
    (cond
      ((and (<= score-a score-b)
            (< zins score-b))
       (psetf score-b zins
              point-b 11))
      ((and (> score-a score-b)
            (< zins score-a))
       (psetf score-a zins
              point-a 11)))
    (cond
      ((and (<= score-a score-b)
            (< wins score-b))
       (psetf score-b wins
              point-b 7))
      ((and (> score-a score-b)
            (< wins score-a))
       (psetf score-a wins
              point-a 7)))
    (if (or (< uins score-a)
            (< uins score-b))
        (let ((c (if (< score-b score-a) point-b point-a)))
          (if (not (zerop (logand c 1)))
              (let ((xsv (1+ xsb))
                    (dx (- dx0 1 sq4)))
                (psetf (oss4d-xsv-ext0 state) (+ xsb 2)
                       (oss4d-xsv-ext1 state) xsv
                       (oss4d-xsv-ext2 state) xsv
                       (oss4d-dx-ext0 state) (- dx0 2 sq4)
                       (oss4d-dx-ext1 state) dx
                       (oss4d-dx-ext2 state) dx))
              (let ((dx (- dx0 sq4)))
                (psetf (oss4d-xsv-ext0 state) xsb
                       (oss4d-xsv-ext1 state) xsb
                       (oss4d-xsv-ext2 state) xsb
                       (oss4d-dx-ext0 state) dx
                       (oss4d-dx-ext1 state) dx
                       (oss4d-dx-ext2 state) dx)))
          (cond
            ((not (zerop (logand c 2)))
             (let ((ysv (1+ ysb))
                   (dy (- dy0 1 sq4)))
               (psetf (oss4d-ysv-ext0 state) ysv
                      (oss4d-ysv-ext1 state) ysv
                      (oss4d-ysv-ext2 state) ysv
                      (oss4d-dy-ext0 state) dy
                      (oss4d-dy-ext1 state) dy
                      (oss4d-dy-ext2 state) dy))
             (cond
               ((not (zerop (logand c 1)))
                (incf (oss4d-ysv-ext1 state))
                (decf (oss4d-dy-ext1 state)))
               (t
                (incf (oss4d-ysv-ext0 state))
                (decf (oss4d-dy-ext0 state)))))
            (t
             (let ((dy (- dy0 sq4)))
               (psetf (oss4d-ysv-ext0 state) ysb
                      (oss4d-ysv-ext1 state) ysb
                      (oss4d-ysv-ext2 state) ysb
                      (oss4d-dy-ext0 state) dy
                      (oss4d-dy-ext1 state) dy
                      (oss4d-dy-ext2 state) dy))))
          (cond
            ((not (zerop (logand c 4)))
             (let ((zsv (1+ zsb))
                   (dz (- dz0 1 sq4)))
               (psetf (oss4d-zsv-ext0 state) zsv
                      (oss4d-zsv-ext1 state) zsv
                      (oss4d-zsv-ext2 state) zsv
                      (oss4d-dz-ext0 state) dz
                      (oss4d-dz-ext1 state) dz
                      (oss4d-dz-ext2 state) dz))
             (cond
               ((not (= (logand c 3) 3))
                (unless (zerop (logand c 3))
                  (incf (oss4d-zsv-ext1 state))
                  (decf (oss4d-dz-ext1 state))))
               (t
                (incf (oss4d-zsv-ext2 state))
                (decf (oss4d-dz-ext2 state)))))
            (t
             (let ((dz (- dz0 sq4)))
               (psetf (oss4d-zsv-ext0 state) zsb
                      (oss4d-zsv-ext1 state) zsb
                      (oss4d-zsv-ext2 state) zsb
                      (oss4d-dz-ext0 state) dz
                      (oss4d-dz-ext1 state) dz
                      (oss4d-dz-ext2 state) dz))))
          (if (not (zerop (logand c 8)))
              (let ((wsv (1+ wsb))
                    (dw (- dw0 1 sq4)))
                (psetf (oss4d-wsv-ext0 state) wsv
                       (oss4d-wsv-ext1 state) wsv
                       (oss4d-wsv-ext2 state) (+ wsb 2)
                       (oss4d-dw-ext0 state) dw
                       (oss4d-dw-ext1 state) dw
                       (oss4d-dw-ext2 state) (- dw0 2 sq4)))
              (let ((dw (- dw0 sq4)))
                (psetf (oss4d-wsv-ext0 state) wsb
                       (oss4d-wsv-ext1 state) wsb
                       (oss4d-wsv-ext2 state) wsb
                       (oss4d-dw-ext0 state) dw
                       (oss4d-dw-ext1 state) dw
                       (oss4d-dw-ext2 state) dw))))
        (let ((c (logand point-a point-b)))
          (if (not (zerop (logand c 1)))
              (let ((xsv (1+ xsb)))
                (psetf (oss4d-xsv-ext0 state) xsv
                       (oss4d-xsv-ext1 state) (+ xsb 2)
                       (oss4d-xsv-ext2 state) xsv
                       (oss4d-dx-ext0 state) (- dx0 1 sq2)
                       (oss4d-dx-ext1 state) (- dx0 2 sq3)
                       (oss4d-dx-ext2 state) (- dx0 1 sq3)))
              (let ((dx (- dx0 sq3)))
                (psetf (oss4d-xsv-ext0 state) xsb
                       (oss4d-xsv-ext1 state) xsb
                       (oss4d-xsv-ext2 state) xsb
                       (oss4d-dx-ext0 state) (- dx0 sq2)
                       (oss4d-dx-ext1 state) dx
                       (oss4d-dx-ext2 state) dx)))
          (cond
            ((not (zerop (logand c 2)))
             (let ((ysv (1+ ysb))
                   (dy (- dy0 1 sq3)))
               (psetf (oss4d-ysv-ext0 state) ysv
                      (oss4d-ysv-ext1 state) ysv
                      (oss4d-ysv-ext2 state) ysv
                      (oss4d-dy-ext0 state) (- dy0 1 sq2)
                      (oss4d-dy-ext1 state) dy
                      (oss4d-dy-ext2 state) dy))
             (cond
               ((not (zerop (logand c 1)))
                (incf (oss4d-ysv-ext2 state))
                (decf (oss4d-dy-ext2 state)))
               (t
                (incf (oss4d-ysv-ext1 state))
                (decf (oss4d-dy-ext1 state)))))
            (t
             (let ((dy (- dy0 sq3)))
               (psetf (oss4d-ysv-ext0 state) ysb
                      (oss4d-ysv-ext1 state) ysb
                      (oss4d-ysv-ext2 state) ysb
                      (oss4d-dy-ext0 state) (- dy0 sq2)
                      (oss4d-dy-ext1 state) dy
                      (oss4d-dy-ext2 state) dy))))
          (cond
            ((not (zerop (logand c 4)))
             (let ((zsv (1+ zsb))
                   (dz (- dz0 1 sq3)))
               (psetf (oss4d-zsv-ext0 state) zsv
                      (oss4d-zsv-ext1 state) zsv
                      (oss4d-zsv-ext2 state) zsv
                      (oss4d-dz-ext0 state) (- dz0 1 sq2)
                      (oss4d-dz-ext1 state) dz
                      (oss4d-dz-ext2 state) dz))
             (cond
               ((not (zerop (logand c 3)))
                (incf (oss4d-zsv-ext2 state))
                (decf (oss4d-dz-ext2 state)))
               (t
                (incf (oss4d-zsv-ext1 state))
                (decf (oss4d-dz-ext1 state)))))
            (t
             (let ((dz (- dz0 sq3)))
               (psetf (oss4d-zsv-ext0 state) zsb
                      (oss4d-zsv-ext1 state) zsb
                      (oss4d-zsv-ext2 state) zsb
                      (oss4d-dz-ext0 state) (- dz0 sq2)
                      (oss4d-dz-ext1 state) dz
                      (oss4d-dz-ext2 state) dz))))
          (cond
            ((not (zerop (logand c 8)))
             (let  ((wsv (1+ wsb)))
               (psetf (oss4d-wsv-ext0 state) wsv
                      (oss4d-wsv-ext1 state) wsv
                      (oss4d-wsv-ext2 state) (+ wsb 2)
                      (oss4d-dw-ext0 state) (- dw0 1 sq2)
                      (oss4d-dw-ext1 state) (- dw0 1 sq3)
                      (oss4d-dw-ext2 state) (- dw0 2 sq3))))
            (t
             (let ((dw (- dw0 sq3)))
               (psetf (oss4d-wsv-ext0 state) wsb
                      (oss4d-wsv-ext1 state) wsb
                      (oss4d-wsv-ext2 state) wsb
                      (oss4d-dw-ext0 state) (- dw0 sq2)
                      (oss4d-dw-ext1 state) dw
                      (oss4d-dw-ext2 state) dw))))))
    (let ((dw3 (- dw0 1 sq3))
          (dx4 (- dx0 1 sq3))
          (dy4 (- dy0 1 sq3))
          (dz4 (- dz0 1 sq3)))
      (psetf (oss4d-dx4 state) dx4
             (oss4d-dy4 state) dy4
             (oss4d-dz4 state) dz4
             (oss4d-dw4 state) (- dw0 sq3)
             (oss4d-dx3 state) dx4
             (oss4d-dy3 state) dy4
             (oss4d-dz3 state) (- dz0 sq3)
             (oss4d-dw3 state) dw3
             (oss4d-dx2 state) dx4
             (oss4d-dy2 state) (- dy0 sq3)
             (oss4d-dz2 state) dz4
             (oss4d-dw2 state) dw3
             (oss4d-dx1 state) (- dx0 sq3)
             (oss4d-dy1 state) dy4
             (oss4d-dz1 state) dz4
             (oss4d-dw1 state) dw3
             (oss4d-dx0 state) (- dx0 1 sq4)
             (oss4d-dy0 state) (- dy0 1 sq4)
             (oss4d-dz0 state) (- dz0 1 sq4)
             (oss4d-dw0 state) (- dw0 1 sq4)))
    (values)))

(defun open-simplex-4d/in3 (state)
  (let* ((point-a 0)
         (point-b 0)
         (score-a 0d0)
         (score-b 0d0)
         (a-bigger-p t)
         (b-bigger-p t)
         (xins (oss4d-xins state))
         (yins (oss4d-yins state))
         (zins (oss4d-zins state))
         (wins (oss4d-wins state))
         (ins (oss4d-ins state))
         (xsb (oss4d-xsb state))
         (ysb (oss4d-ysb state))
         (zsb (oss4d-zsb state))
         (wsb (oss4d-wsb state))
         (dx0 (oss4d-dx0 state))
         (dy0 (oss4d-dy0 state))
         (dz0 (oss4d-dz0 state))
         (dw0 (oss4d-dw0 state))
         (sq +open-simplex-4d/squish+)
         (sq2 #.(* +open-simplex-4d/squish+ 2))
         (sq3 #.(* +open-simplex-4d/squish+ 3)))
    (if (> (+ xins yins) (+ zins wins))
        (psetf score-a (+ xins yins)
               point-a 3)
        (psetf score-a (+ zins wins)
               point-a 12))
    (if (> (+ xins zins) (+ yins wins))
        (psetf score-b (+ xins zins)
               point-b 5)
        (psetf score-b (+ yins wins)
               point-b 10))
    (if (> (+ xins wins) (+ yins zins))
        (let ((score (+ xins wins)))
          (cond
            ((and (>= score-a score-b)
                  (> score score-b))
             (psetf score-b score
                    point-b 9))
            ((and (< score-a score-b)
                  (> score score-a))
             (psetf score-a score
                    point-a 9))))
        (let ((score (+ yins zins)))
          (cond
            ((and (>= score-a score-b)
                  (> score score-b))
             (psetf score-b score
                    point-b 6))
            ((and (< score-a score-b)
                  (> score score-a))
             (psetf score-a score
                    point-a 6)))))
    (let ((p1 (+ (- 2 ins) xins)))
      (cond
        ((and (>= score-a score-b)
              (> p1 score-b))
         (psetf score-b p1
                point-b 1
                b-bigger-p nil))
        ((and (< score-a score-b)
              (> p1 score-a))
         (psetf score-a p1
                point-a 1
                a-bigger-p nil))))
    (let ((p2 (+ (- 2 ins) yins)))
      (cond
        ((and (>= score-a score-b)
              (> p2 score-b))
         (psetf score-b p2
                point-b 2
                b-bigger-p nil))
        ((and (< score-a score-b)
              (> p2 score-a))
         (psetf score-a p2
                point-a 2
                a-bigger-p nil))))
    (let ((p3 (+ (- 2 ins) zins)))
      (cond
        ((and (>= score-a score-b)
              (> p3 score-b))
         (psetf score-b p3
                point-b 4
                b-bigger-p nil))
        ((and (< score-a score-b)
              (> p3 score-a))
         (psetf score-a p3
                point-a 4
                a-bigger-p nil))))
    (let ((p4 (+ (- 2 ins) wins)))
      (cond
        ((and (>= score-a score-b)
              (> p4 score-b))
         (psetf score-b p4
                point-b 8
                b-bigger-p nil))
        ((and (< score-a score-b)
              (> p4 score-a))
         (psetf score-a p4
                point-a 8
                a-bigger-p nil))))
    (if (eq a-bigger-p b-bigger-p)
        (if a-bigger-p
            (let ((c1 (logior point-a point-b))
                  (c2 (logand point-a point-b)))
              (if (zerop (logand c1 1))
                  (psetf (oss4d-xsv-ext0 state) xsb
                         (oss4d-xsv-ext1 state) (1- xsb)
                         (oss4d-dx-ext0 state) (- dx0 sq3)
                         (oss4d-dx-ext1 state) (- (1+ dx0) sq2))
                  (let ((xsv (1+ xsb)))
                    (psetf (oss4d-xsv-ext0 state) xsv
                           (oss4d-xsv-ext1 state) xsv
                           (oss4d-dx-ext0 state) (- dx0 1 sq3)
                           (oss4d-dx-ext1 state) (- dx0 1 sq2))))
              (if (zerop (logand c1 2))
                  (psetf (oss4d-ysv-ext0 state) ysb
                         (oss4d-ysv-ext1 state) (1- ysb)
                         (oss4d-dy-ext0 state) (- dy0 sq3)
                         (oss4d-dy-ext1 state) (- (1+ dy0) sq2))
                  (let ((ysv (1+ ysb)))
                    (psetf (oss4d-ysv-ext0 state) ysv
                           (oss4d-ysv-ext1 state) ysv
                           (oss4d-dy-ext0 state) (- dy0 1 sq3)
                           (oss4d-dy-ext1 state) (- dy0 1 sq2))))
              (if (zerop (logand c1 4))
                  (psetf (oss4d-zsv-ext0 state) zsb
                         (oss4d-zsv-ext1 state) (1- zsb)
                         (oss4d-dz-ext0 state) (- dz0 sq3)
                         (oss4d-dz-ext1 state) (- (1+ dz0) sq2))
                  (let ((zsv (1+ zsb)))
                    (psetf (oss4d-zsv-ext0 state) zsv
                           (oss4d-zsv-ext1 state) zsv
                           (oss4d-dz-ext0 state) (- dz0 1 sq3)
                           (oss4d-dz-ext1 state) (- dz0 1 sq2))))
              (if (zerop (logand c1 8))
                  (psetf (oss4d-wsv-ext0 state) wsb
                         (oss4d-wsv-ext1 state) (1- wsb)
                         (oss4d-dw-ext0 state) (- dw0 sq3)
                         (oss4d-dw-ext1 state) (- (1+ dw0) sq2))
                  (let ((wsv (1+ wsb)))
                    (psetf (oss4d-wsv-ext0 state) wsv
                           (oss4d-wsv-ext1 state) wsv
                           (oss4d-dw-ext0 state) (- dw0 1 sq3)
                           (oss4d-dw-ext1 state) (- dw0 1 sq2))))
              (psetf (oss4d-xsv-ext2 state) xsb
                     (oss4d-ysv-ext2 state) ysb
                     (oss4d-zsv-ext2 state) zsb
                     (oss4d-wsv-ext2 state) wsb
                     (oss4d-dx-ext2 state) (- dx0 sq2)
                     (oss4d-dy-ext2 state) (- dy0 sq2)
                     (oss4d-dz-ext2 state) (- dz0 sq2)
                     (oss4d-dw-ext2 state) (- dw0 sq2))
              (cond
                ((not (zerop (logand c2 1)))
                 (incf (oss4d-xsv-ext2 state) 2)
                 (decf (oss4d-dx-ext2 state) 2))
                ((not (zerop (logand c2 2)))
                 (incf (oss4d-ysv-ext2 state) 2)
                 (decf (oss4d-dy-ext2 state) 2))
                ((not (zerop (logand c2 4)))
                 (incf (oss4d-zsv-ext2 state) 2)
                 (decf (oss4d-dz-ext2 state) 2))
                (t
                 (incf (oss4d-wsv-ext2 state) 2)
                 (decf (oss4d-dw-ext2 state) 2))))
            (let ((c (logior point-a point-b)))
              (psetf (oss4d-xsv-ext2 state) xsb
                     (oss4d-ysv-ext2 state) ysb
                     (oss4d-zsv-ext2 state) zsb
                     (oss4d-wsv-ext2 state) wsb
                     (oss4d-dx-ext2 state) dx0
                     (oss4d-dy-ext2 state) dy0
                     (oss4d-dz-ext2 state) dz0
                     (oss4d-dw-ext2 state) dw0)
              (if (zerop (logand c 1))
                  (psetf (oss4d-xsv-ext0 state) (1- xsb)
                         (oss4d-xsv-ext1 state) xsb
                         (oss4d-dx-ext0 state) (- (1+ dx0) sq)
                         (oss4d-dx-ext1 state) (- dx0 sq))
                  (let ((xsv (1+ xsb))
                        (dx (- dx0 1 sq)))
                    (psetf (oss4d-xsv-ext0 state) xsv
                           (oss4d-xsv-ext1 state) xsv
                           (oss4d-dx-ext0 state) dx
                           (oss4d-dx-ext1 state) dx)))
              (cond
                ((zerop (logand c 2))
                 (let ((dy (- dy0 sq)))
                   (psetf (oss4d-ysv-ext0 state) ysb
                          (oss4d-ysv-ext1 state) ysb
                          (oss4d-dy-ext0 state) dy
                          (oss4d-dy-ext1 state) dy))
                 (cond
                   ((= (logand c 1) 1)
                    (decf (oss4d-ysv-ext0 state))
                    (incf (oss4d-dy-ext0 state)))
                   (t
                    (decf (oss4d-ysv-ext1 state))
                    (incf (oss4d-dy-ext1 state)))))
                (t
                 (let ((ysv (1+ ysb))
                       (dy (- dy0 1 sq)))
                   (psetf (oss4d-ysv-ext0 state) ysv
                          (oss4d-ysv-ext1 state) ysv
                          (oss4d-dy-ext0 state) dy
                          (oss4d-dy-ext1 state) dy))))
              (cond
                ((zerop (logand c 4))
                 (let ((dz (- dz0 sq)))
                   (psetf (oss4d-zsv-ext0 state) zsb
                          (oss4d-zsv-ext1 state) zsb
                          (oss4d-dz-ext0 state) dz
                          (oss4d-dz-ext1 state) dz))
                 (cond
                   ((= (logand c 3) 3)
                    (decf (oss4d-zsv-ext0 state))
                    (incf (oss4d-dz-ext0 state)))
                   (t
                    (decf (oss4d-zsv-ext1 state))
                    (incf (oss4d-dz-ext1 state)))))
                (t
                 (let ((zsv (1+ zsb))
                       (dz (- dz0 1 sq)))
                   (psetf (oss4d-zsv-ext0 state) zsv
                          (oss4d-zsv-ext1 state) zsv
                          (oss4d-dz-ext0 state) dz
                          (oss4d-dz-ext1 state) dz))))
              (cond
                ((zerop (logand c 8))
                 (psetf (oss4d-wsv-ext0 state) wsb
                        (oss4d-wsv-ext1 state) (1- wsb)
                        (oss4d-dw-ext0 state) (- dw0 sq)
                        (oss4d-dw-ext1 state) (- (1+ dw0) sq)))
                (t
                 (let ((wsv (1+ wsb))
                       (dw (- dw0 1 sq)))
                   (psetf (oss4d-wsv-ext0 state) wsv
                          (oss4d-wsv-ext1 state) wsv
                          (oss4d-dw-ext0 state) dw
                          (oss4d-dw-ext1 state) dw))))))
        (let ((c1 (if a-bigger-p point-a point-b))
              (c2 (if a-bigger-p point-b point-a)))
          (if (zerop (logand c1 1))
              (psetf (oss4d-xsv-ext0 state) (1- xsb)
                     (oss4d-xsv-ext1 state) xsb
                     (oss4d-dx-ext0 state) (- (1+ dx0) sq)
                     (oss4d-dx-ext1 state) (- dx0 sq))
              (let ((xsv (1+ xsb))
                    (dx (- dx0 1 sq)))
                (psetf (oss4d-xsv-ext0 state) xsv
                       (oss4d-xsv-ext1 state) xsv
                       (oss4d-dx-ext0 state) dx
                       (oss4d-dx-ext1 state) dx)))
          (cond
            ((zerop (logand c1 2))
             (let ((dy (- dy0 sq)))
               (psetf (oss4d-ysv-ext0 state) ysb
                      (oss4d-ysv-ext1 state) ysb
                      (oss4d-dy-ext0 state) dy
                      (oss4d-dy-ext1 state) dy))
             (cond
               ((= (logand c1 1) 1)
                (decf (oss4d-ysv-ext0 state))
                (incf (oss4d-dy-ext0 state)))
               (t
                (decf (oss4d-ysv-ext1 state))
                (incf (oss4d-dy-ext1 state)))))
            (t
             (let ((ysv (1+ ysb))
                   (dy (- dy0 1 sq)))
               (psetf (oss4d-ysv-ext0 state) ysv
                      (oss4d-ysv-ext1 state) ysv
                      (oss4d-dy-ext0 state) dy
                      (oss4d-dy-ext1 state) dy))))
          (cond
            ((zerop (logand c1 4))
             (let ((dz (- dz0 sq)))
               (psetf (oss4d-zsv-ext0 state) zsb
                      (oss4d-zsv-ext1 state) zsb
                      (oss4d-dz-ext0 state) dz
                      (oss4d-dz-ext1 state) dz))
             (cond
               ((= (logand c1 3) 3)
                (decf (oss4d-zsv-ext0 state))
                (incf (oss4d-dz-ext0 state)))
               (t
                (decf (oss4d-zsv-ext1 state))
                (incf (oss4d-dz-ext1 state)))))
            (t
             (let ((zsv (1+ zsb))
                   (dz (- dz0 1 sq)))
               (psetf (oss4d-zsv-ext0 state) zsv
                      (oss4d-zsv-ext1 state) zsv
                      (oss4d-dz-ext0 state) dz
                      (oss4d-dz-ext1 state) dz))))
          (if (zerop (logand c1 8))
              (psetf (oss4d-wsv-ext0 state) wsb
                     (oss4d-wsv-ext1 state) (1- wsb)
                     (oss4d-dw-ext0 state) (- dw0 sq)
                     (oss4d-dw-ext1 state) (- (1+ dw0) sq))
              (let ((wsv (1+ wsb))
                    (dw (- dw0 1 sq)))
                (psetf (oss4d-wsv-ext0 state) wsv
                       (oss4d-wsv-ext1 state) wsv
                       (oss4d-dw-ext0 state) dw
                       (oss4d-dw-ext1 state) dw)))
          (psetf (oss4d-xsv-ext2 state) xsb
                 (oss4d-ysv-ext2 state) ysb
                 (oss4d-zsv-ext2 state) zsb
                 (oss4d-wsv-ext2 state) wsb
                 (oss4d-dx-ext2 state) (- dx0 sq2)
                 (oss4d-dy-ext2 state) (- dy0 sq2)
                 (oss4d-dz-ext2 state) (- dz0 sq2)
                 (oss4d-dw-ext2 state) (- dw0 sq2))
          (cond
            ((not (zerop (logand c2 1)))
             (incf (oss4d-xsv-ext2 state) 2)
             (decf (oss4d-dx-ext2 state) 2))
            ((not (zerop (logand c2 2)))
             (incf (oss4d-ysv-ext2 state) 2)
             (decf (oss4d-dy-ext2 state) 2))
            ((not (zerop (logand c2 4)))
             (incf (oss4d-zsv-ext2 state) 2)
             (decf (oss4d-dz-ext2 state) 2))
            (t
             (incf (oss4d-wsv-ext2 state) 2)
             (decf (oss4d-dw-ext2 state) 2)))))
    (let ((dy1 (- dy0 sq))
          (dz1 (- dz0 sq))
          (dw1 (- dw0 sq))
          (dx2 (- dx0 sq))
          (dx5 (- dx0 1 sq2))
          (dy5 (- dy0 1 sq2))
          (dz5 (- dz0 sq2))
          (dw5 (- dw0 sq2))
          (dy6 (- dy0 sq2))
          (dz6 (- dz0 1 sq2))
          (dw7 (- dw0 1 sq2))
          (dx8 (- dx0 sq2)))
      (psetf (oss4d-dx1 state) (- dx0 1 sq)
             (oss4d-dy1 state) dy1
             (oss4d-dz1 state) dz1
             (oss4d-dw1 state) dw1
             (oss4d-dx2 state) dx2
             (oss4d-dy2 state) (- dy0 1 sq)
             (oss4d-dz2 state) dz1
             (oss4d-dw2 state) dw1
             (oss4d-dx3 state) dx2
             (oss4d-dy3 state) dy1
             (oss4d-dz3 state) (- dz0 1 sq)
             (oss4d-dw3 state) dw1
             (oss4d-dx4 state) dx2
             (oss4d-dy4 state) dy1
             (oss4d-dz4 state) dz1
             (oss4d-dw4 state) (- dw0 1 sq)
             (oss4d-dx5 state) dx5
             (oss4d-dy5 state) dy5
             (oss4d-dz5 state) dz5
             (oss4d-dw5 state) dw5
             (oss4d-dx6 state) dx5
             (oss4d-dy6 state) dy6
             (oss4d-dz6 state) dz6
             (oss4d-dw6 state) dw5
             (oss4d-dx7 state) dx5
             (oss4d-dy7 state) dy6
             (oss4d-dz7 state) dz5
             (oss4d-dw7 state) dw7
             (oss4d-dx8 state) dx8
             (oss4d-dy8 state) dy5
             (oss4d-dz8 state) dz6
             (oss4d-dw8 state) dw5
             (oss4d-dx9 state) dx8
             (oss4d-dy9 state) dy5
             (oss4d-dz9 state) dz5
             (oss4d-dw9 state) dw7
             (oss4d-dx10 state) dx8
             (oss4d-dy10 state) dy6
             (oss4d-dz10 state) dz6
             (oss4d-dw10 state) dw7))
    (values)))

(defun open-simplex-4d/in4 (state)
  (let* ((point-a 0)
         (point-b 0)
         (score-a 0d0)
         (score-b 0d0)
         (a-bigger-p t)
         (b-bigger-p t)
         (xins (oss4d-xins state))
         (yins (oss4d-yins state))
         (zins (oss4d-zins state))
         (wins (oss4d-wins state))
         (ins (oss4d-ins state))
         (xsb (oss4d-xsb state))
         (ysb (oss4d-ysb state))
         (zsb (oss4d-zsb state))
         (wsb (oss4d-wsb state))
         (dx0 (oss4d-dx0 state))
         (dy0 (oss4d-dy0 state))
         (dz0 (oss4d-dz0 state))
         (dw0 (oss4d-dw0 state))
         (sq +open-simplex-4d/squish+)
         (sq2 #.(* +open-simplex-4d/squish+ 2))
         (sq3 #.(* +open-simplex-4d/squish+ 3))
         (sq4 #.(* +open-simplex-4d/squish+ 4)))
    (if (< (+ xins yins) (+ zins wins))
        (psetf score-a (+ xins yins)
               point-a 12)
        (psetf score-a (+ zins wins)
               point-a 3))
    (if (< (+ xins zins) (+ yins wins))
        (psetf score-b (+ xins zins)
               point-b 10)
        (psetf score-b (+ yins wins)
               point-b 5))
    (if (< (+ xins wins) (+ yins zins))
        (let ((score (+ xins wins)))
          (cond
            ((and (<= score-a score-b)
                  (< score score-b))
             (psetf score-b score
                    point-b 6))
            ((and (> score-a score-b)
                  (< score score-a))
             (psetf score-a score
                    point-a 6))))
        (let ((score (+ yins zins)))
          (cond
            ((and (<= score-a score-b)
                  (< score score-b))
             (psetf score-b score
                    point-b 9))
            ((and (> score-a score-b)
                  (< score score-a))
             (psetf score-a score
                    point-a 9)))))
    (let ((p1 (+ (- 3 ins) xins)))
      (cond
        ((and (<= score-a score-b)
              (< p1 score-b))
         (psetf score-b p1
                point-b 14
                b-bigger-p nil))
        ((and (> score-a score-b)
              (< p1 score-a))
         (psetf score-a p1
                point-a 14
                a-bigger-p nil))))
    (let ((p2 (+ (- 3 ins) yins)))
      (cond
        ((and (<= score-a score-b)
              (< p2 score-b))
         (psetf score-b p2
                point-b 13
                b-bigger-p nil))
        ((and (> score-a score-b)
              (< p2 score-a))
         (psetf score-a p2
                point-a 13
                a-bigger-p nil))))
    (let ((p3 (+ (- 3 ins) zins)))
      (cond
        ((and (<= score-a score-b)
              (< p3 score-b))
         (psetf score-b p3
                point-b 11
                b-bigger-p nil))
        ((and (> score-a score-b)
              (< p3 score-a))
         (psetf score-a p3
                point-a 11
                a-bigger-p nil))))
    (let ((p4 (+ (- 3 ins) wins)))
      (cond
        ((and (<= score-a score-b)
              (< p4 score-b))
         (psetf score-b p4
                point-b 7
                b-bigger-p nil))
        ((and (> score-a score-b)
              (< p4 score-a))
         (psetf score-a p4
                point-a 7
                a-bigger-p nil))))
    (if (eq a-bigger-p b-bigger-p)
        (if a-bigger-p
            (let ((c1 (logand point-a point-b))
                  (c2 (logior point-a point-b)))
              (psetf (oss4d-xsv-ext0 state) xsb
                     (oss4d-xsv-ext1 state) xsb
                     (oss4d-ysv-ext0 state) ysb
                     (oss4d-ysv-ext1 state) ysb
                     (oss4d-zsv-ext0 state) zsb
                     (oss4d-zsv-ext1 state) zsb
                     (oss4d-wsv-ext0 state) wsb
                     (oss4d-wsv-ext1 state) wsb
                     (oss4d-dx-ext0 state) (- dx0 sq)
                     (oss4d-dy-ext0 state) (- dy0 sq)
                     (oss4d-dz-ext0 state) (- dz0 sq)
                     (oss4d-dw-ext0 state) (- dw0 sq)
                     (oss4d-dx-ext1 state) (- dx0 sq2)
                     (oss4d-dy-ext1 state) (- dy0 sq2)
                     (oss4d-dz-ext1 state) (- dz0 sq2)
                     (oss4d-dw-ext1 state) (- dw0 sq2))
              (cond
                ((not (zerop (logand c1 1)))
                 (incf (oss4d-xsv-ext0 state))
                 (decf (oss4d-dx-ext0 state))
                 (incf (oss4d-xsv-ext1 state) 2)
                 (decf (oss4d-dx-ext1 state) 2))
                ((not (zerop (logand c1 2)))
                 (incf (oss4d-ysv-ext0 state))
                 (decf (oss4d-dy-ext0 state))
                 (incf (oss4d-ysv-ext1 state) 2)
                 (decf (oss4d-dy-ext1 state) 2))
                ((not (zerop (logand c1 4)))
                 (incf (oss4d-zsv-ext0 state))
                 (decf (oss4d-dz-ext0 state))
                 (incf (oss4d-zsv-ext1 state) 2)
                 (decf (oss4d-dz-ext1 state) 2))
                (t
                 (incf (oss4d-wsv-ext0 state))
                 (decf (oss4d-dw-ext0 state))
                 (incf (oss4d-wsv-ext1 state) 2)
                 (decf (oss4d-dw-ext1 state) 2)))
              (psetf (oss4d-xsv-ext2 state) (1+ xsb)
                     (oss4d-ysv-ext2 state) (1+ ysb)
                     (oss4d-zsv-ext2 state) (1+ zsb)
                     (oss4d-wsv-ext2 state) (1+ wsb)
                     (oss4d-dx-ext2 state) (- dx0 1 sq2)
                     (oss4d-dy-ext2 state) (- dy0 1 sq2)
                     (oss4d-dz-ext2 state) (- dz0 1 sq2)
                     (oss4d-dw-ext2 state) (- dw0 1 sq2))
              (cond
                ((zerop (logand c2 1))
                 (decf (oss4d-xsv-ext2 state) 2)
                 (incf (oss4d-dx-ext2 state) 2))
                ((zerop (logand c2 2))
                 (decf (oss4d-ysv-ext2 state) 2)
                 (incf (oss4d-dy-ext2 state) 2))
                ((zerop (logand c2 4))
                 (decf (oss4d-zsv-ext2 state) 2)
                 (incf (oss4d-dz-ext2 state) 2))
                (t
                 (decf (oss4d-wsv-ext2 state) 2)
                 (incf (oss4d-dw-ext2 state) 2))))
            (let ((c (logand point-a point-b)))
              (psetf (oss4d-xsv-ext2 state) (1+ xsb)
                     (oss4d-ysv-ext2 state) (1+ ysb)
                     (oss4d-zsv-ext2 state) (1+ zsb)
                     (oss4d-wsv-ext2 state) (1+ wsb)
                     (oss4d-dx-ext2 state) (- dx0 1 sq4)
                     (oss4d-dy-ext2 state) (- dy0 1 sq4)
                     (oss4d-dz-ext2 state) (- dz0 1 sq4)
                     (oss4d-dw-ext2 state) (- dw0 1 sq4))
              (if (not (zerop (logand c 1)))
                  (psetf (oss4d-xsv-ext0 state) (+ xsb 2)
                         (oss4d-xsv-ext1 state) (1+ xsb)
                         (oss4d-dx-ext0 state) (- dx0 2 sq3)
                         (oss4d-dx-ext1 state) (- dx0 1 sq3))
                  (let ((dx (- dx0 sq3)))
                    (psetf (oss4d-xsv-ext0 state) xsb
                           (oss4d-xsv-ext1 state) xsb
                           (oss4d-dx-ext0 state) dx
                           (oss4d-dx-ext1 state) dx)))
              (cond
                ((not (zerop (logand c 2)))
                 (let ((ysv (1+ ysb))
                       (dy (- dy0 1 sq3)))
                   (psetf (oss4d-ysv-ext0 state) ysv
                          (oss4d-ysv-ext1 state) ysv
                          (oss4d-dy-ext0 state) dy
                          (oss4d-dy-ext1 state) dy))
                 (cond
                   ((zerop (logand c 1))
                    (incf (oss4d-ysv-ext0 state))
                    (decf (oss4d-dy-ext0 state)))
                   (t
                    (incf (oss4d-ysv-ext1 state))
                    (decf (oss4d-dy-ext1 state)))))
                (t
                 (let ((dy (- dy0 sq3)))
                   (psetf (oss4d-ysv-ext0 state) ysb
                          (oss4d-ysv-ext1 state) ysb
                          (oss4d-dy-ext0 state) dy
                          (oss4d-dy-ext1 state) dy))))
              (cond
                ((not (zerop (logand c 4)))
                 (let ((zsv (1+ zsb))
                       (dz (- dz0 1 sq3)))
                   (psetf (oss4d-zsv-ext0 state) zsv
                          (oss4d-zsv-ext1 state) zsv
                          (oss4d-dz-ext0 state) dz
                          (oss4d-dz-ext1 state) dz))
                 (cond
                   ((zerop (logand c 3))
                    (incf (oss4d-zsv-ext0 state))
                    (decf (oss4d-dz-ext0 state)))
                   (t
                    (incf (oss4d-zsv-ext1 state))
                    (decf (oss4d-dz-ext1 state)))))
                (t
                 (let ((dz (- dz0 sq3)))
                   (psetf (oss4d-zsv-ext0 state) zsb
                          (oss4d-zsv-ext1 state) zsb
                          (oss4d-dz-ext0 state) dz
                          (oss4d-dz-ext1 state) dz))))
              (cond
                ((not (zerop (logand c 8)))
                 (psetf (oss4d-wsv-ext0 state) (1+ wsb)
                        (oss4d-wsv-ext1 state) (+ wsb 2)
                        (oss4d-dw-ext0 state) (- dw0 1 sq3)
                        (oss4d-dw-ext1 state) (- dw0 2 sq3)))
                (t
                 (let ((dw (- dw0 sq3)))
                   (psetf (oss4d-wsv-ext0 state) wsb
                          (oss4d-wsv-ext1 state) wsb
                          (oss4d-dw-ext0 state) dw
                          (oss4d-dw-ext1 state) dw))))))
        (let ((c1 (if a-bigger-p point-a point-b))
              (c2 (if a-bigger-p point-b point-a)))
          (if (not (zerop (logand c1 1)))
              (psetf (oss4d-xsv-ext0 state) (+ xsb 2)
                     (oss4d-xsv-ext1 state) (1+ xsb)
                     (oss4d-dx-ext0 state) (- dx0 2 sq3)
                     (oss4d-dx-ext1 state) (- dx0 1 sq3))
              (let ((dx (- dx0 sq3)))
                (psetf (oss4d-xsv-ext0 state) xsb
                       (oss4d-xsv-ext1 state) xsb
                       (oss4d-dx-ext0 state) dx
                       (oss4d-dx-ext1 state) dx)))
          (cond
            ((not (zerop (logand c1 2)))
             (let ((ysv (1+ ysb))
                   (dy (- dy0 1 sq3)))
               (psetf (oss4d-ysv-ext0 state) ysv
                      (oss4d-ysv-ext1 state) ysv
                      (oss4d-dy-ext0 state) dy
                      (oss4d-dy-ext1 state) dy))
             (cond
               ((zerop (logand c1 1))
                (incf (oss4d-ysv-ext0 state))
                (decf (oss4d-dy-ext0 state)))
               (t
                (incf (oss4d-ysv-ext1 state))
                (decf (oss4d-dy-ext1 state)))))
            (t
             (let ((dy (- dy0 sq3)))
               (psetf (oss4d-ysv-ext0 state) ysb
                      (oss4d-ysv-ext1 state) ysb
                      (oss4d-dy-ext0 state) dy
                      (oss4d-dy-ext1 state) dy))))
          (cond
            ((not (zerop (logand c1 4)))
             (let ((zsv (1+ zsb))
                   (dz (- dz0 1 sq3)))
               (psetf (oss4d-zsv-ext0 state) zsv
                      (oss4d-zsv-ext1 state) zsv
                      (oss4d-dz-ext0 state) dz
                      (oss4d-dz-ext1 state) dz))
             (cond
               ((zerop (logand c1 3))
                (incf (oss4d-zsv-ext0 state))
                (decf (oss4d-dz-ext0 state)))
               (t
                (incf (oss4d-zsv-ext1 state))
                (decf (oss4d-dz-ext1 state)))))
            (t
             (let ((dz (- dz0 sq3)))
               (psetf (oss4d-zsv-ext0 state) zsb
                      (oss4d-zsv-ext1 state) zsb
                      (oss4d-dz-ext0 state) dz
                      (oss4d-dz-ext1 state) dz))))
          (if (not (zerop (logand c1 8)))
              (psetf (oss4d-wsv-ext0 state) (1+ wsb)
                     (oss4d-wsv-ext1 state) (+ wsb 2)
                     (oss4d-dw-ext0 state) (- dw0 1 sq3)
                     (oss4d-dw-ext1 state) (- dw0 2 sq3))
              (let ((dw (- dw0 sq3)))
                (psetf (oss4d-wsv-ext0 state) wsb
                       (oss4d-wsv-ext1 state) wsb
                       (oss4d-dw-ext0 state) dw
                       (oss4d-dw-ext1 state) dw)))
          (psetf (oss4d-xsv-ext2 state) (1+ xsb)
                 (oss4d-ysv-ext2 state) (1+ ysb)
                 (oss4d-zsv-ext2 state) (1+ zsb)
                 (oss4d-wsv-ext2 state) (1+ wsb)
                 (oss4d-dx-ext2 state) (- dx0 1 sq2)
                 (oss4d-dy-ext2 state) (- dy0 1 sq2)
                 (oss4d-dz-ext2 state) (- dz0 1 sq2)
                 (oss4d-dw-ext2 state) (- dw0 1 sq2))
          (cond
            ((zerop (logand c2 1))
             (decf (oss4d-xsv-ext2 state) 2)
             (incf (oss4d-dx-ext2 state) 2))
            ((zerop (logand c2 2))
             (decf (oss4d-ysv-ext2 state) 2)
             (incf (oss4d-dy-ext2 state) 2))
            ((zerop (logand c2 4))
             (decf (oss4d-zsv-ext2 state) 2)
             (incf (oss4d-dz-ext2 state) 2))
            (t
             (decf (oss4d-wsv-ext2 state) 2)
             (incf (oss4d-dw-ext2 state) 2)))))
    (let ((dw3 (- dw0 1 sq3))
          (dx4 (- dx0 1 sq3))
          (dy4 (- dy0 1 sq3))
          (dz4 (- dz0 1 sq3))
          (dx5 (- dx0 1 sq2))
          (dy5 (- dy0 1 sq2))
          (dz5 (- dz0 sq2))
          (dw5 (- dw0 sq2))
          (dy6 (- dy0 sq2))
          (dz6 (- dz0 1 sq2))
          (dw7 (- dw0 1 sq2))
          (dx8 (- dx0 sq2)))
      (psetf (oss4d-dx4 state) dx4
             (oss4d-dy4 state) dy4
             (oss4d-dz4 state) dz4
             (oss4d-dw4 state) (- dw0 sq3)
             (oss4d-dx3 state) dx4
             (oss4d-dy3 state) dy4
             (oss4d-dz3 state) (- dz0 sq3)
             (oss4d-dw3 state) dw3
             (oss4d-dx2 state) dx4
             (oss4d-dy2 state) (- dy0 sq3)
             (oss4d-dz2 state) dz4
             (oss4d-dw2 state) dw3
             (oss4d-dx1 state) (- dx0 sq3)
             (oss4d-dy1 state) dy4
             (oss4d-dz1 state) dz4
             (oss4d-dw1 state) dw3
             (oss4d-dx5 state) dx5
             (oss4d-dy5 state) dy5
             (oss4d-dz5 state) dz5
             (oss4d-dw5 state) dw5
             (oss4d-dx6 state) dx5
             (oss4d-dy6 state) dy6
             (oss4d-dz6 state) dz6
             (oss4d-dw6 state) dw5
             (oss4d-dx7 state) dx5
             (oss4d-dy7 state) dy6
             (oss4d-dz7 state) dz5
             (oss4d-dw7 state) dw7
             (oss4d-dx8 state) dx8
             (oss4d-dy8 state) dy5
             (oss4d-dz8 state) dz6
             (oss4d-dw8 state) dw5
             (oss4d-dx9 state) dx8
             (oss4d-dy9 state) dy5
             (oss4d-dz9 state) dz5
             (oss4d-dw9 state) dw7
             (oss4d-dx10 state) dx8
             (oss4d-dy10 state) dy6
             (oss4d-dz10 state) dz6
             (oss4d-dw10 state) dw7))
    (values)))

(defun %open-simplex-4d (table x y z w)
  (declare (optimize speed)
           (u:f64 x y z w))
  (let ((state (make-open-simplex-4d-state table x y z w)))
    (cond
      ((<= (oss4d-ins state) 1)
       (open-simplex-4d/in1 state)
       (open-simplex-4d/contribute1 state))
      ((>= (oss4d-ins state) 3)
       (open-simplex-4d/in2 state)
       (open-simplex-4d/contribute2 state))
      ((<= (oss4d-ins state) 2)
       (open-simplex-4d/in3 state)
       (open-simplex-4d/contribute3 state))
      (t
       (open-simplex-4d/in4 state)
       (open-simplex-4d/contribute4 state)))
    (open-simplex-4d/contribute5 state)
    (float (* (oss4d-value state) +open-simplex-4d/scale+) 1f0)))
