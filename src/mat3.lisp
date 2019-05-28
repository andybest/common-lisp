(in-package #:cl-user)

(defpackage #:box.math.mat3
  (:local-nicknames (#:% #:box.math.internal)
                    (#:v2 #:box.math.vec2)
                    (#:v3 #:box.math.vec3))
  (:use #:cl)
  (:shadow #:= #:+ #:- #:* #:trace)
  (:export
   #:matrix
   #:with-components
   #:mref
   #:+zero+
   #:+id+
   #:make
   #:zero!
   #:zero
   #:id!
   #:id
   #:=
   #:~
   #:copy!
   #:copy
   #:clamp!
   #:clamp
   #:+!
   #:+
   #:-!
   #:-
   #:*!
   #:*
   #:translation-to-vec2!
   #:translation-to-vec2
   #:translation-from-vec2!
   #:translation-from-vec2
   #:translate!
   #:translate
   #:copy-rotation!
   #:copy-rotation
   #:rotation-axis-to-vec2!
   #:rotation-axis-to-vec2
   #:rotation-axis-from-vec2!
   #:rotation-axis-from-vec2
   #:rotate!
   #:rotate
   #:scale-to-vec2!
   #:scale-to-vec2
   #:scale-from-vec2!
   #:scale-from-vec2
   #:scale!
   #:scale
   #:*v3!
   #:*v3
   #:transpose!
   #:transpose
   #:orthogonal-p
   #:trace
   #:diagonal-p
   #:main-diagonal!
   #:main-diagonal
   #:anti-diagonal!
   #:anti-diagonal))


(in-package #:box.math.mat3)

;;; Structure

(deftype matrix () '(simple-array single-float (9)))

(defstruct (matrix (:type (vector single-float))
                   (:constructor %make (m00 m01 m02
                                        m10 m11 m12
                                        m20 m21 m22))
                   (:conc-name nil)
                   (:copier nil))
  "A 3x3 column-major matrix consisting of column vectors. This represents
either the rotation sub-matrix of a 3-dimensional transformation matrix, or a
complete 2-dimensional transformation matrix."
  (m00 0.0f0 :type single-float)
  (m10 0.0f0 :type single-float)
  (m20 0.0f0 :type single-float)
  (m01 0.0f0 :type single-float)
  (m11 0.0f0 :type single-float)
  (m21 0.0f0 :type single-float)
  (m02 0.0f0 :type single-float)
  (m12 0.0f0 :type single-float)
  (m22 0.0f0 :type single-float))

(defmacro with-components (((prefix matrix) &rest rest) &body body)
  "A convenience macro for concisely accessing the components of matrices."
  `(with-accessors ((,prefix identity)
                    (,(%::make-accessor-symbol prefix "00") m00)
                    (,(%::make-accessor-symbol prefix "01") m01)
                    (,(%::make-accessor-symbol prefix "02") m02)
                    (,(%::make-accessor-symbol prefix "10") m10)
                    (,(%::make-accessor-symbol prefix "11") m11)
                    (,(%::make-accessor-symbol prefix "12") m12)
                    (,(%::make-accessor-symbol prefix "20") m20)
                    (,(%::make-accessor-symbol prefix "21") m21)
                    (,(%::make-accessor-symbol prefix "22") m22))
       ,matrix
     ,(if rest
          `(with-components ,rest ,@body)
          `(progn ,@body))))

(declaim (inline mref))
(declaim (ftype (function (matrix (integer 0 8) (integer 0 8)) single-float)
                mref))
(defun mref (matrix row column)
  "A virtualized matrix component reader. Use this instead of AREF to prevent
unintended behavior should ordering of a matrix ever change."
  (aref matrix (cl:+ row (cl:* column 3))))

(declaim (inline (setf mref)))
(declaim (ftype (function (single-float matrix (integer 0 8) (integer 0 8))
                          single-float)
                (setf mref)))
(defun (setf mref) (value matrix row column)
  "A virtualized matrix component writer. Use this instead of (SETF AREF) to
prevent unintended behavior should ordering of a matrix ever change."
  (setf (aref matrix (cl:+ row (cl:* column 3))) value))

;;; Constants

(au:define-constant +zero+
    (make-array 9 :element-type 'single-float
                  :initial-contents '(0.0f0 0.0f0 0.0f0
                                      0.0f0 0.0f0 0.0f0
                                      0.0f0 0.0f0 0.0f0))
  :test #'equalp
  :documentation "A matrix with each component as zero.")

(au:define-constant +id+
    (make-array 9 :element-type 'single-float
                  :initial-contents '(1.0f0 0.0f0 0.0f0
                                      0.0f0 1.0f0 0.0f0
                                      0.0f0 0.0f0 1.0f0))
  :test #'equalp
  :documentation "An identity matrix.")

;;; Operations

(declaim (inline make))
(declaim (ftype (function (real real real real real real real real real)
                          matrix)
                make))
(defun make (m00 m01 m02 m10 m11 m12 m20 m21 m22)
  "Create a new matrix."
  (%make (float m00 1.0f0) (float m01 1.0f0) (float m02 1.0f0)
         (float m10 1.0f0) (float m11 1.0f0) (float m12 1.0f0)
         (float m20 1.0f0) (float m21 1.0f0) (float m22 1.0f0)))

(declaim (inline zero!))
(declaim (ftype (function (matrix) matrix) zero!))
(defun zero! (matrix)
  "Set each component of MATRIX to zero."
  (with-components ((m matrix))
    (psetf m00 0.0f0 m01 0.0f0 m02 0.0f0
           m10 0.0f0 m11 0.0f0 m12 0.0f0
           m20 0.0f0 m21 0.0f0 m22 0.0f0))
  matrix)

(declaim (inline zero))
(declaim (ftype (function () matrix) zero))
(defun zero ()
  "Create a new matrix with all components initialized to zero."
  (%make 0.0f0 0.0f0 0.0f0
         0.0f0 0.0f0 0.0f0
         0.0f0 0.0f0 0.0f0))

(declaim (inline id!))
(declaim (ftype (function (matrix) matrix) id!))
(defun id! (matrix)
  "Modify MATRIX to be an identity matrix."
  (with-components ((m matrix))
    (psetf m00 1.0f0 m01 0.0f0 m02 0.0f0
           m10 0.0f0 m11 1.0f0 m12 0.0f0
           m20 0.0f0 m21 0.0f0 m22 1.0f0))
  matrix)

(declaim (inline id))
(declaim (ftype (function () matrix) id))
(defun id ()
  "Create an identity matrix."
  (%make 1.0f0 0.0f0 0.0f0
         0.0f0 1.0f0 0.0f0
         0.0f0 0.0f0 1.0f0))

(declaim (inline =))
(declaim (ftype (function (matrix matrix) boolean) =))
(defun = (matrix1 matrix2)
  "Check if all components of MATRIX1 are numerically equal to the components of
MATRIX2."
  (with-components ((a matrix1) (b matrix2))
    (and (cl:= a00 b00) (cl:= a01 b01) (cl:= a02 b02)
         (cl:= a10 b10) (cl:= a11 b11) (cl:= a12 b12)
         (cl:= a20 b20) (cl:= a21 b21) (cl:= a22 b22))))

(declaim (inline ~))
(declaim (ftype (function (matrix matrix &key (:tolerance single-float))
                          boolean)
                ~))
(defun ~ (matrix1 matrix2 &key (tolerance 1e-7))
  "Check if all components of MATRIX1 are approximately equal to the components
of MATRIX2, according to TOLERANCE."
  (with-components ((a matrix1) (b matrix2))
    (and (%::~ a00 b00 tolerance)
         (%::~ a01 b01 tolerance)
         (%::~ a02 b02 tolerance)
         (%::~ a10 b10 tolerance)
         (%::~ a11 b11 tolerance)
         (%::~ a12 b12 tolerance)
         (%::~ a20 b20 tolerance)
         (%::~ a21 b21 tolerance)
         (%::~ a22 b22 tolerance))))

(declaim (inline copy!))
(declaim (ftype (function (matrix matrix) matrix) copy!))
(defun copy! (out matrix)
  "Copy each component of MATRIX to the existing matrix, OUT."
  (with-components ((o out) (m matrix))
    (psetf o00 m00 o01 m01 o02 m02
           o10 m10 o11 m11 o12 m12
           o20 m20 o21 m21 o22 m22))
  out)

(declaim (inline copy))
(declaim (ftype (function (matrix) matrix) copy))
(defun copy (matrix)
  "Copy each component of MATRIX to a freshly allocated matrix."
  (copy! (zero) matrix))

(declaim (ftype (function (matrix matrix &key (:min single-float)
                                  (:max single-float))
                          matrix)
                clamp!))
(defun clamp! (out matrix
               &key (min most-negative-single-float)
                 (max most-positive-single-float))
  "Clamp each component of MATRIX within the range of [MIN, MAX], storing the
result in the existing matrix, OUT."
  (with-components ((o out) (m matrix))
    (psetf o00 (au:clamp m00 min max)
           o01 (au:clamp m01 min max)
           o02 (au:clamp m02 min max)
           o10 (au:clamp m10 min max)
           o11 (au:clamp m11 min max)
           o12 (au:clamp m12 min max)
           o20 (au:clamp m20 min max)
           o21 (au:clamp m21 min max)
           o22 (au:clamp m22 min max)))
  out)

(declaim (inline clamp))
(declaim (ftype (function (matrix &key (:min single-float)
                                  (:max single-float))
                          matrix)
                clamp))
(defun clamp (matrix
              &key (min most-negative-single-float)
                (max most-positive-single-float))
  "Clamp each component of MATRIX within the range of [MIN, MAX], storing the
result in a freshly allocated matrix."
  (clamp! (zero) matrix :min min :max max))

(declaim (inline +!))
(declaim (ftype (function (matrix matrix matrix) matrix) +!))
(defun +! (out matrix1 matrix2)
  "Calculate the sum of MATRIX1 and MATRIX2, storing the result in the existing
matrix, OUT."
  (with-components ((o out) (a matrix1) (b matrix2))
    (psetf o00 (cl:+ a00 b00)
           o10 (cl:+ a10 b10)
           o20 (cl:+ a20 b20)
           o01 (cl:+ a01 b01)
           o11 (cl:+ a11 b11)
           o21 (cl:+ a21 b21)
           o02 (cl:+ a02 b02)
           o12 (cl:+ a12 b12)
           o22 (cl:+ a22 b22)))
  out)

(declaim (inline +))
(declaim (ftype (function (matrix matrix) matrix) +))
(defun + (matrix1 matrix2)
  "Calculate the sum of MATRIX1 and MATRIX2, storing the result in a freshly
allocated matrix."
  (+! (zero) matrix1 matrix2))

(declaim (inline -!))
(declaim (ftype (function (matrix matrix matrix) matrix) -!))
(defun -! (out matrix1 matrix2)
  "Calculate the difference of MATRIX2 and MATRIX1, storing the result in the
existing matrix, OUT."
  (with-components ((o out) (a matrix1) (b matrix2))
    (psetf o00 (cl:- a00 b00)
           o10 (cl:- a10 b10)
           o20 (cl:- a20 b20)
           o01 (cl:- a01 b01)
           o11 (cl:- a11 b11)
           o21 (cl:- a21 b21)
           o02 (cl:- a02 b02)
           o12 (cl:- a12 b12)
           o22 (cl:- a22 b22)))
  out)

(declaim (inline -))
(declaim (ftype (function (matrix matrix) matrix) -))
(defun - (matrix1 matrix2)
  "Calculate the difference of MATRIX2 and MATRIX1, storing the result in a
freshly allocated matrix."
  (-! (zero) matrix1 matrix2))

(declaim (inline *!))
(declaim (ftype (function (matrix matrix matrix) matrix) *!))
(defun *! (out matrix1 matrix2)
  "Calculate the product of MATRIX1 and MATRIX2, storing the result in the
existing matrix, OUT."
  (with-components ((o out) (a matrix1) (b matrix2))
    (psetf o00 (cl:+ (cl:* a00 b00) (cl:* a01 b10) (cl:* a02 b20))
           o10 (cl:+ (cl:* a10 b00) (cl:* a11 b10) (cl:* a12 b20))
           o20 (cl:+ (cl:* a20 b00) (cl:* a21 b10) (cl:* a22 b20))
           o01 (cl:+ (cl:* a00 b01) (cl:* a01 b11) (cl:* a02 b21))
           o11 (cl:+ (cl:* a10 b01) (cl:* a11 b11) (cl:* a12 b21))
           o21 (cl:+ (cl:* a20 b01) (cl:* a21 b11) (cl:* a22 b21))
           o02 (cl:+ (cl:* a00 b02) (cl:* a01 b12) (cl:* a02 b22))
           o12 (cl:+ (cl:* a10 b02) (cl:* a11 b12) (cl:* a12 b22))
           o22 (cl:+ (cl:* a20 b02) (cl:* a21 b12) (cl:* a22 b22))))
  out)

(declaim (inline *))
(declaim (ftype (function (matrix matrix) matrix) *))
(defun * (matrix1 matrix2)
  "Calculate the product of MATRIX1 and MATRIX2, storing the result in a freshly
allocated matrix."
  (*! (zero) matrix1 matrix2))

(declaim (inline translation-to-vec2!))
(declaim (ftype (function (v2:vec matrix) v2:vec) translation-to-vec2!))
(defun translation-to-vec2! (out matrix)
  "Copy the translation column of MATRIX to the existing vector, OUT."
  (v2:with-components ((o out))
    (with-components ((m matrix))
      (psetf ox m02 oy m12)))
  out)

(declaim (inline translation-to-vec2))
(declaim (ftype (function (matrix) v2:vec) translation-to-vec2))
(defun translation-to-vec2 (matrix)
  "Copy the translation column of MATRIX to a freshly allocated vector."
  (translation-to-vec2! (v2:zero) matrix))

(declaim (inline translation-from-vec2!))
(declaim (ftype (function (matrix v2:vec) matrix) translation-from-vec2!))
(defun translation-from-vec2! (matrix vec)
  "Copy the components of VEC to the translation column of MATRIX. This
destructively modifies MATRIX."
  (with-components ((m matrix))
    (v2:with-components ((v vec))
      (psetf m02 vx m12 vy)))
  matrix)

(declaim (inline translation-from-vec2))
(declaim (ftype (function (matrix v2:vec) matrix) translation-from-vec2))
(defun translation-from-vec2 (matrix vec)
  "Copy the components of VEC to the translation column of MATRIX. This
allocates a fresh matrix, leaving the original un-modified."
  (translation-from-vec2! (copy matrix) vec))

(declaim (inline translate!))
(declaim (ftype (function (matrix matrix v2:vec) matrix) translate!))
(defun translate! (out matrix vec)
  "Translate MATRIX by VEC, storing the result in the existing matrix, OUT."
  (*! out (translation-from-vec2 (id) vec) matrix))

(declaim (inline translate))
(declaim (ftype (function (matrix v2:vec) matrix) translate))
(defun translate (matrix vec)
  "Translate MATRIX by VEC, storing the result in a freshly allocated matrix."
  (translate! (id) matrix vec))

(declaim (inline copy-rotation!))
(declaim (ftype (function (matrix matrix) matrix) copy-rotation!))
(defun copy-rotation! (out matrix)
  "Copy the rotation components of MATRIX to the existing matrix, OUT."
  (with-components ((o out) (m matrix))
    (psetf o00 m00 o01 m01
           o10 m10 o11 m11))
  out)

(declaim (inline copy-rotation))
(declaim (ftype (function (matrix) matrix) copy-rotation))
(defun copy-rotation (matrix)
  "Copy the rotation components of MATRIX to a freshly allocated matrix."
  (copy-rotation! (id) matrix))

(declaim (inline rotation-axis-to-vec2!))
(declaim (ftype (function (v2:vec matrix keyword) v2:vec)
                rotation-axis-to-vec2!))
(defun rotation-axis-to-vec2! (out matrix axis)
  "Copy the rotation axis from MATRIX denoted by the keyword symbol AXIS, to the
existing vector, OUT."
  (v2:with-components ((v out))
    (with-components ((m matrix))
      (ecase axis
        (:x (psetf vx m00 vy m10))
        (:y (psetf vx m01 vy m11)))))
  out)

(declaim (inline rotation-axis-to-vec2))
(declaim (ftype (function (matrix keyword) v2:vec) rotation-axis-to-vec2))
(defun rotation-axis-to-vec2 (matrix axis)
  "Copy the rotation axis from MATRIX denoted by the keyword symbol AXIS, to a
freshly allocated vector."
  (rotation-axis-to-vec2! (v2:zero) matrix axis))

(declaim (inline rotation-axis-from-vec2!))
(declaim (ftype (function (matrix v2:vec keyword) matrix)
                rotation-axis-from-vec2!))
(defun rotation-axis-from-vec2! (matrix vec axis)
  "Copy the components of VEC into the rotation axis of MATRIX denoted by the
keyword symbol AXIS. This destructively modifies MATRIX."
  (with-components ((m matrix))
    (v2:with-components ((v vec))
      (ecase axis
        (:x (psetf m00 vx m10 vy))
        (:y (psetf m01 vx m11 vy)))))
  matrix)

(declaim (inline rotation-axis-from-vec2))
(declaim (ftype (function (matrix v2:vec keyword) matrix)
                rotation-axis-from-vec2))
(defun rotation-axis-from-vec2 (matrix vec axis)
  "Copy the components of VEC into the rotation axis of MATRIX denoted by the
keyword symbol AXIS. This allocates a fresh matrix, leaving the original
un-modified."
  (rotation-axis-from-vec2! (copy matrix) vec axis))

(declaim (ftype (function (matrix matrix float) matrix) rotate!))
(defun rotate! (out matrix angle)
  "Rotate MATRIX by the Euler angle, ANGLE, storing the result in the existing
matrix, OUT."
  (with-components ((m (id)))
    (copy! out matrix)
    (when (cl:> (abs angle) 1e-7)
      (let* ((angle (float angle 1.0f0))
             (s (sin angle))
             (c (cos angle)))
        (psetf m00 c m01 (cl:- s)
               m10 s m11 c)
        (*! out out m))))
  out)

(declaim (inline rotate))
(declaim (ftype (function (matrix float) matrix) rotate))
(defun rotate (matrix angle)
  "Rotate MATRIX by the Euler angle, ANGLE, storing the result in a freshly
allocated matrix."
  (rotate! (id) matrix angle))

(declaim (inline scale-to-vec2!))
(declaim (ftype (function (v2:vec matrix) v2:vec) scale-to-vec2!))
(defun scale-to-vec2! (out matrix)
  "Copy the scaling transform of MATRIX to the existing vector, OUT."
  (v2:with-components ((o out))
    (with-components ((m matrix))
      (psetf ox m00 oy m11)))
  out)

(declaim (inline scale-to-vec2))
(declaim (ftype (function (matrix) v2:vec) scale-to-vec2))
(defun scale-to-vec2 (matrix)
  "Copy the scaling transform of MATRIX to a freshly allocated vector."
  (scale-to-vec2! (v2:zero) matrix))

(declaim (inline scale-from-vec2!))
(declaim (ftype (function (matrix v2:vec) matrix) scale-from-vec2!))
(defun scale-from-vec2! (matrix vec)
  "Copy the components of VEC to the scaling components of MATRIX. This
destructively modifies MATRIX."
  (with-components ((m matrix))
    (v2:with-components ((v vec))
      (psetf m00 vx m11 vy)))
  matrix)

(declaim (inline scale-from-vec2))
(declaim (ftype (function (matrix v2:vec) matrix) scale-from-vec2))
(defun scale-from-vec2 (matrix vec)
  "Copy the components of VEC to the scaling components of MATRIX. This
allocates a fresh matrix, leaving the origin un-modified."
  (scale-from-vec2! (copy matrix) vec))

(declaim (inline scale!))
(declaim (ftype (function (matrix matrix v2:vec) matrix) scale!))
(defun scale! (out matrix vec)
  "Scale MATRIX by each scalar in VEC, storing the result in the existing
matrix, OUT."
  (*! out (scale-from-vec2 (id) vec) matrix))

(declaim (inline scale))
(declaim (ftype (function (matrix v2:vec) matrix) scale))
(defun scale (matrix vec)
  "Scale MATRIX by each scalar in VEC, storing the result in a freshly allocated
matrix."
  (scale! (id) matrix vec))

(declaim (inline *v3!))
(declaim (ftype (function (v2:vec matrix v2:vec) v2:vec) *v2!))
(defun *v3! (out matrix vec)
  "Calculate the product of MATRIX and VEC, storing the result in the existing
vector, OUT."
  (v3:with-components ((v vec) (o out))
    (with-components ((m matrix))
      (psetf ox (cl:+ (cl:* m00 vx) (cl:* m01 vy) (cl:* m02 vz))
             oy (cl:+ (cl:* m10 vx) (cl:* m11 vy) (cl:* m12 vz))
             oz (cl:+ (cl:* m20 vx) (cl:* m21 vy) (cl:* m22 vz)))))
  out)

(declaim (inline *v3))
(declaim (ftype (function (matrix v2:vec) v2:vec) *v2))
(defun *v3 (matrix vec)
  "Calculate the product of MATRIX and VEC, storing the result in a freshly
allocated vector."
  (*v3! (v3:zero) matrix vec))

(declaim (inline transpose!))
(declaim (ftype (function (matrix matrix) matrix) transpose!))
(defun transpose! (out matrix)
  "Transpose MATRIX, storing the result in the existing matrix, OUT."
  (with-components ((o (copy! out matrix)))
    (rotatef o01 o10)
    (rotatef o02 o20)
    (rotatef o12 o21))
  out)

(declaim (inline transpose))
(declaim (ftype (function (matrix) matrix) transpose))
(defun transpose (matrix)
  "Transpose MATRIX, storing the result in a freshly allocated matrix."
  (transpose! (id) matrix))

(declaim (ftype (function (matrix) boolean) orthogonal-p))
(defun orthogonal-p (matrix)
  "Check if MATRIX is orthogonal. An orthogonal matrix is a square matrix with
all of its rows (or columns) being perpendicular to each other, and of unit
length."
  (~ (* matrix (transpose matrix)) +id+))

(declaim (inline trace))
(declaim (ftype (function (matrix) single-float) trace))
(defun trace (matrix)
  "Calculates the sum of the components along the main diagonal of MATRIX."
  (with-components ((m matrix))
    (cl:+ m00 m11 m22)))

(declaim (inline diagonal-p))
(declaim (ftype (function (matrix) boolean) diagonal-p))
(defun diagonal-p (matrix)
  "Check if the components outside of the main diagonal of MATRIX are all zero."
  (with-components ((m matrix))
    (and (zerop m10)
         (zerop m20)
         (zerop m01)
         (zerop m21)
         (zerop m02)
         (zerop m12))))

(declaim (inline main-diagonal!))
(declaim (ftype (function (v3:vec matrix) v3:vec) main-diagonal!))
(defun main-diagonal! (out matrix)
  "Copy the components along the main diagonal of MATRIX to the existing vector,
OUT."
  (with-components ((m matrix))
    (v3:with-components ((v out))
      (setf vx m00 vy m11 vz m22)))
  out)

(declaim (inline main-diagonal))
(declaim (ftype (function (matrix) v3:vec) main-diagonal))
(defun main-diagonal (matrix)
  "Copy the components along the main diagonal of MATRIX to a freshly allocated
vector."
  (main-diagonal! (v3:zero) matrix))

(declaim (inline anti-diagonal!))
(declaim (ftype (function (v3:vec matrix) v3:vec) anti-diagonal!))
(defun anti-diagonal! (out matrix)
  "Copy the components along the anti-diagonal of MATRIX to the existing vector,
OUT."
  (with-components ((m matrix))
    (v3:with-components ((v out))
      (setf vx m02 vy m11 vz m20)))
  out)

(declaim (inline anti-diagonal))
(declaim (ftype (function (matrix) v3:vec) anti-diagonal))
(defun anti-diagonal (matrix)
  "Copy the components along the anti-diagonal of MATRIX to a freshly allocated
vector."
  (anti-diagonal! (v3:zero) matrix))
