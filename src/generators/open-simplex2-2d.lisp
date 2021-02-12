(in-package #:cl-user)

(defpackage #:coherent-noise.generators.open-simplex2-2d
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export #:open-simplex2-2d))

(in-package #:coherent-noise.generators.open-simplex2-2d)

(u:define-constant +gradients+
    (let ((gradients #(#(13.031324456287654d0 98.98273633310245d0)
                       #(38.20591014244875d0 92.23722642870753d0)
                       #(60.77682619065379d0 79.20590197241988d0)
                       #(79.20590197241988d0 60.77682619065379d0)
                       #(92.23722642870753d0 38.20591014244875d0)
                       #(98.98273633310245d0 13.031324456287555d0)
                       #(98.98273633310245d0 -13.031324456287555d0)
                       #(92.23722642870753d0 -38.20591014244875d0)
                       #(79.20590197241988d0 -60.776826190653686d0)
                       #(60.77682619065379d0 -79.20590197241988d0)
                       #(38.20591014244875d0 -92.23722642870753d0)
                       #(13.031324456287654d0 -98.98273633310245d0)
                       #(-13.031324456287654d0 -98.98273633310245d0)
                       #(-38.20591014244875d0 -92.23722642870753d0)
                       #(-60.77682619065379d0 -79.20590197241988d0)
                       #(-79.20590197241988d0 -60.77682619065379d0)
                       #(-92.23722642870753d0 -38.20591014244875d0)
                       #(-98.98273633310245d0 -13.031324456287654d0)
                       #(-98.98273633310245d0 13.031324456287555d0)
                       #(-92.23722642870753d0 38.20591014244875d0)
                       #(-79.20590197241988d0 60.77682619065379d0)
                       #(-60.77682619065379d0 79.20590197241988d0)
                       #(-38.20591014244875d0 92.23722642870753d0)
                       #(-13.031324456287654d0 98.98273633310245d0)))
          (table (make-array 4096 :element-type 'u:f64)))
      (dotimes (i 2048)
        (setf (aref table (ash i 1)) (aref (aref gradients (mod i 24)) 0)
              (aref table (1+ (ash i 1))) (aref (aref gradients (mod i 24)) 1)))
      table)
  :test #'equalp)

(u:define-constant +lookup+
    (let ((data #(-0.788675134594813d0 0.211324865405187d0 0d0 0d0 -0.577350269189626d0
                  -0.577350269189626d0 0.211324865405187d0 -0.788675134594813d0)))
      (make-array 8 :element-type 'u:f64 :initial-contents data))
  :test #'equalp)

(u:fn-> permute (rng:generator) (values (simple-array u:f64 (4096)) (simple-array u:b16 (2048))))
(defun permute (rng)
  (declare (optimize speed))
  (let ((source (make-array 2048 :element-type 'u:b16 :initial-element 0))
        (table (make-array 2048 :element-type 'u:b16 :initial-element 0))
        (gradients (make-array 4096 :element-type 'u:f64)))
    (dotimes (i 2048)
      (setf (aref source i) i))
    (loop :for i :from 2047 :downto 0
          :for r = (mod (+ (rng:int rng 0 #.(1- (expt 2 32)) nil) 31) (1+ i))
          :for x = (aref source r)
          :for pgi = (ash i 1)
          :for gi = (ash x 1)
          :do (setf (aref table i) x
                    (aref gradients pgi) (aref +gradients+ gi)
                    (aref gradients (1+ pgi)) (aref +gradients+ (1+ gi))
                    (aref source r) (aref source i)))
    (values gradients table)))

(u:fn-> %sample ((simple-array u:f64 (4096)) (simple-array u:b16 (2048)) int::f50 int::f50) u:f32)
(declaim (inline %sample))
(defun %sample (gradients table x y)
  (declare (optimize speed))
  (u:mvlet* ((value 0d0)
             (s (* (+ x y) 0.366025403784439d0))
             (xsb xsi (floor (+ x s)))
             (ysb ysi (floor (+ y s)))
             (index (truncate (1+ (* (- ysi xsi) 0.5))))
             (ssi (* (+ xsi ysi) -0.211324865405187d0))
             (xi (+ xsi ssi))
             (yi (+ ysi ssi)))
    (declare (u:f64 value))
    (dotimes (i 3 (float value 1f0))
      (block nil
        (let* ((lpx (* (+ index i) 2))
               (lpy (1+ lpx))
               (dx (+ xi (aref +lookup+ lpx)))
               (dy (+ yi (aref +lookup+ lpy)))
               (attn (- 0.5 (expt dx 2) (expt dy 2))))
          (when (minusp attn)
            (return))
          (let* ((pxm (logand (+ xsb (ldb (byte 1 lpx) #b10110001)) 2047))
                 (pym (logand (+ ysb (ldb (byte 1 lpy) #b10110001)) 2047))
                 (grad-index (ash (logxor (aref table pxm) pym) 1))
                 (grad-x (aref gradients grad-index))
                 (grad-y (aref gradients (1+ grad-index))))
            (setf attn (expt attn 2))
            (incf value (* (expt attn 2) (+ (* grad-x dx) (* grad-y dy))))))))))

(defun open-simplex2-2d (&key (seed "default"))
  (u:mvlet* ((rng (int::make-rng seed))
             (perm-grad perm (permute rng)))
    (lambda (x &optional (y 0d0) z w)
      (declare (ignore z w))
      (%sample perm-grad perm x y))))
