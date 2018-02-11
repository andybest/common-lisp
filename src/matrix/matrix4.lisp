(in-package :box.math.mat4)

;;; Structure

(deftype matrix () '(simple-array single-float (16)))

(defstruct (matrix (:type (vector single-float))
                   (:constructor %make (m00 m01 m02 m03
                                        m10 m11 m12 m13
                                        m20 m21 m22 m23
                                        m30 m31 m32 m33))
                   (:conc-name nil)
                   (:copier nil))
  "A 4x4 column-major matrix consisting of column vectors. This represents a complete 3-dimensional
transformation matrix."
  (m00 0.0f0 :type single-float)
  (m10 0.0f0 :type single-float)
  (m20 0.0f0 :type single-float)
  (m30 0.0f0 :type single-float)
  (m01 0.0f0 :type single-float)
  (m11 0.0f0 :type single-float)
  (m21 0.0f0 :type single-float)
  (m31 0.0f0 :type single-float)
  (m02 0.0f0 :type single-float)
  (m12 0.0f0 :type single-float)
  (m22 0.0f0 :type single-float)
  (m32 0.0f0 :type single-float)
  (m03 0.0f0 :type single-float)
  (m13 0.0f0 :type single-float)
  (m23 0.0f0 :type single-float)
  (m33 0.0f0 :type single-float))

(defmacro with-components (((prefix matrix) &rest rest) &body body)
  "A convenience macro for concisely accessing the components of matrices."
  `(with-accessors ((,prefix identity)
                    (,(box.math.base::%make-accessor-symbol prefix "00") m00)
                    (,(box.math.base::%make-accessor-symbol prefix "01") m01)
                    (,(box.math.base::%make-accessor-symbol prefix "02") m02)
                    (,(box.math.base::%make-accessor-symbol prefix "03") m03)
                    (,(box.math.base::%make-accessor-symbol prefix "10") m10)
                    (,(box.math.base::%make-accessor-symbol prefix "11") m11)
                    (,(box.math.base::%make-accessor-symbol prefix "12") m12)
                    (,(box.math.base::%make-accessor-symbol prefix "13") m13)
                    (,(box.math.base::%make-accessor-symbol prefix "20") m20)
                    (,(box.math.base::%make-accessor-symbol prefix "21") m21)
                    (,(box.math.base::%make-accessor-symbol prefix "22") m22)
                    (,(box.math.base::%make-accessor-symbol prefix "23") m23)
                    (,(box.math.base::%make-accessor-symbol prefix "30") m30)
                    (,(box.math.base::%make-accessor-symbol prefix "31") m31)
                    (,(box.math.base::%make-accessor-symbol prefix "32") m32)
                    (,(box.math.base::%make-accessor-symbol prefix "33") m33))
       ,matrix
     ,(if rest
          `(with-components ,rest ,@body)
          `(progn ,@body))))

(declaim (inline mref))
(declaim (ftype (function (matrix (integer 0 15) (integer 0 15)) single-float) mref))
(defun mref (matrix row column)
  "A virtualized matrix component reader. Use this instead of AREF to prevent unintended behavior
should ordering of a matrix ever change."
  (aref matrix (+ row (cl:* column 4))))

(declaim (inline mref))
(declaim (ftype (function (matrix (integer 0 15) (integer 0 15)) single-float) mref))
(defun (setf mref) (value matrix row column)
  "A virtualized matrix component writer. Use this instead of (SETF AREF) to prevent unintended
behavior should ordering of a matrix ever change."
  (setf (aref matrix (+ row (cl:* column 4))) value))

;;; Constants

(alexandria:define-constant +zero+
    (make-array 16 :element-type 'single-float
                   :initial-contents '(0.0f0 0.0f0 0.0f0 0.0f0
                                       0.0f0 0.0f0 0.0f0 0.0f0
                                       0.0f0 0.0f0 0.0f0 0.0f0
                                       0.0f0 0.0f0 0.0f0 0.0f0))
  :test #'equalp
  :documentation "A matrix with each component as zero.")

(alexandria:define-constant +id+
    (make-array 16 :element-type 'single-float
                   :initial-contents '(1.0f0 0.0f0 0.0f0 0.0f0
                                       0.0f0 1.0f0 0.0f0 0.0f0
                                       0.0f0 0.0f0 1.0f0 0.0f0
                                       0.0f0 0.0f0 0.0f0 1.0f0))
  :test #'equalp
  :documentation "An identity matrix.")

;;; Operations

(declaim (inline make))
(declaim (ftype (function
                 (real real real real real real real real real real real real real real real real)
                 matrix)
                make))
(defun make (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33)
  "Create a new matrix."
  (%make (float m00 1.0f0) (float m01 1.0f0) (float m02 1.0f0) (float m03 1.0f0) (float m10 1.0f0)
         (float m11 1.0f0) (float m12 1.0f0) (float m13 1.0f0) (float m20 1.0f0) (float m21 1.0f0)
         (float m22 1.0f0) (float m23 1.0f0) (float m30 1.0f0) (float m31 1.0f0) (float m32 1.0f0)
         (float m33 1.0f0)))

(declaim (inline zero!))
(declaim (ftype (function (matrix) matrix) zero!))
(defun zero! (matrix)
  "Set each component of MATRIX to zero."
  (with-components ((m matrix))
    (psetf m00 0.0f0 m01 0.0f0 m02 0.0f0 m03 0.0f0
           m10 0.0f0 m11 0.0f0 m12 0.0f0 m13 0.0f0
           m20 0.0f0 m21 0.0f0 m22 0.0f0 m23 0.0f0
           m30 0.0f0 m31 0.0f0 m32 0.0f0 m33 0.0f0))
  matrix)

(declaim (inline zero))
(declaim (ftype (function () matrix) zero))
(defun zero ()
  "Create a new matrix with all components initialized to zero."
  (%make 0.0f0 0.0f0 0.0f0 0.0f0
         0.0f0 0.0f0 0.0f0 0.0f0
         0.0f0 0.0f0 0.0f0 0.0f0
         0.0f0 0.0f0 0.0f0 0.0f0))

(declaim (inline id!))
(declaim (ftype (function (matrix) matrix) id!))
(defun id! (matrix)
  "Modify MATRIX to be an identity matrix."
  (with-components ((m matrix))
    (psetf m00 1.0f0 m01 0.0f0 m02 0.0f0 m03 0.0f0
           m10 0.0f0 m11 1.0f0 m12 0.0f0 m13 0.0f0
           m20 0.0f0 m21 0.0f0 m22 1.0f0 m23 0.0f0
           m30 0.0f0 m31 0.0f0 m32 0.0f0 m33 1.0f0))
  matrix)

(declaim (inline id))
(declaim (ftype (function () matrix) id))
(defun id ()
  "Create an identity matrix."
  (%make 1.0f0 0.0f0 0.0f0 0.0f0
         0.0f0 1.0f0 0.0f0 0.0f0
         0.0f0 0.0f0 1.0f0 0.0f0
         0.0f0 0.0f0 0.0f0 1.0f0))

(declaim (inline =))
(declaim (ftype (function (matrix matrix) boolean) =))
(defun = (matrix1 matrix2)
  "Check if all components of MATRIX1 are numerically equal to the components of MATRIX2."
  (with-components ((a matrix1) (b matrix2))
    (and (cl:= a00 b00) (cl:= a01 b01) (cl:= a02 b02) (cl:= a03 b03)
         (cl:= a10 b10) (cl:= a11 b11) (cl:= a12 b12) (cl:= a13 b13)
         (cl:= a20 b20) (cl:= a21 b21) (cl:= a22 b22) (cl:= a23 b23)
         (cl:= a30 b30) (cl:= a31 b31) (cl:= a32 b32) (cl:= a33 b33))))

(declaim (inline ~))
(declaim (ftype (function (matrix matrix &key (:tolerance single-float)) boolean) ~))
(defun ~ (matrix1 matrix2 &key (tolerance +epsilon+))
  "Check if all components of MATRIX1 are approximately equal to the components of MATRIX2,
according to TOLERANCE."
  (with-components ((a matrix1) (b matrix2))
    (and (box.math.base::%~ a00 b00 tolerance)
         (box.math.base::%~ a01 b01 tolerance)
         (box.math.base::%~ a02 b02 tolerance)
         (box.math.base::%~ a03 b03 tolerance)
         (box.math.base::%~ a10 b10 tolerance)
         (box.math.base::%~ a11 b11 tolerance)
         (box.math.base::%~ a12 b12 tolerance)
         (box.math.base::%~ a13 b13 tolerance)
         (box.math.base::%~ a20 b20 tolerance)
         (box.math.base::%~ a21 b21 tolerance)
         (box.math.base::%~ a22 b22 tolerance)
         (box.math.base::%~ a23 b23 tolerance)
         (box.math.base::%~ a30 b30 tolerance)
         (box.math.base::%~ a31 b31 tolerance)
         (box.math.base::%~ a32 b32 tolerance)
         (box.math.base::%~ a33 b33 tolerance))))

(declaim (inline copy!))
(declaim (ftype (function (matrix matrix) matrix) copy!))
(defun copy! (out matrix)
  "Copy each component of MATRIX to the existing matrix, OUT."
  (with-components ((o out) (m matrix))
    (psetf o00 m00 o01 m01 o02 m02 o03 m03
           o10 m10 o11 m11 o12 m12 o13 m13
           o20 m20 o21 m21 o22 m22 o23 m23
           o30 m30 o31 m31 o32 m32 o33 m33))
  out)

(declaim (inline copy))
(declaim (ftype (function (matrix) matrix) copy))
(defun copy (matrix)
  "Copy each component of MATRIX to a freshly allocated matrix."
  (copy! (zero) matrix))

(declaim (ftype (function (matrix matrix &key (:min single-float) (:max single-float)) matrix)
                clamp!))
(defun clamp! (out matrix &key (min most-negative-single-float) (max most-positive-single-float))
  "Clamp each component of MATRIX within the range of [MIN, MAX], storing the result in the existing
matrix, OUT."
  (with-components ((o out) (m matrix))
    (psetf o00 (alexandria:clamp m00 min max)
           o01 (alexandria:clamp m01 min max)
           o02 (alexandria:clamp m02 min max)
           o03 (alexandria:clamp m03 min max)
           o10 (alexandria:clamp m10 min max)
           o11 (alexandria:clamp m11 min max)
           o12 (alexandria:clamp m12 min max)
           o13 (alexandria:clamp m13 min max)
           o20 (alexandria:clamp m20 min max)
           o21 (alexandria:clamp m21 min max)
           o22 (alexandria:clamp m22 min max)
           o23 (alexandria:clamp m23 min max)
           o30 (alexandria:clamp m30 min max)
           o31 (alexandria:clamp m31 min max)
           o32 (alexandria:clamp m32 min max)
           o33 (alexandria:clamp m33 min max)))
  out)

(declaim (inline clamp))
(declaim (ftype (function (matrix &key (:min single-float) (:max single-float)) matrix) clamp))
(defun clamp (matrix &key (min most-negative-single-float) (max most-positive-single-float))
  "Clamp each component of MATRIX within the range of [MIN, MAX], storing the result in a freshly
allocated matrix."
  (clamp! (zero) matrix :min min :max max))

(declaim (inline *!))
(declaim (ftype (function (matrix matrix matrix) matrix) *!))
(defun *! (out matrix1 matrix2)
  "Calculate the product of MATRIX1 and MATRIX2, storing the result in the existing matrix, OUT."
  (with-components ((o out) (a matrix1) (b matrix2))
    (psetf o00 (+ (cl:* a00 b00) (cl:* a01 b10) (cl:* a02 b20) (cl:* a03 b30))
           o10 (+ (cl:* a10 b00) (cl:* a11 b10) (cl:* a12 b20) (cl:* a13 b30))
           o20 (+ (cl:* a20 b00) (cl:* a21 b10) (cl:* a22 b20) (cl:* a23 b30))
           o30 (+ (cl:* a30 b00) (cl:* a31 b10) (cl:* a32 b20) (cl:* a33 b30))
           o01 (+ (cl:* a00 b01) (cl:* a01 b11) (cl:* a02 b21) (cl:* a03 b31))
           o11 (+ (cl:* a10 b01) (cl:* a11 b11) (cl:* a12 b21) (cl:* a13 b31))
           o21 (+ (cl:* a20 b01) (cl:* a21 b11) (cl:* a22 b21) (cl:* a23 b31))
           o31 (+ (cl:* a30 b01) (cl:* a31 b11) (cl:* a32 b21) (cl:* a33 b31))
           o02 (+ (cl:* a00 b02) (cl:* a01 b12) (cl:* a02 b22) (cl:* a03 b32))
           o12 (+ (cl:* a10 b02) (cl:* a11 b12) (cl:* a12 b22) (cl:* a13 b32))
           o22 (+ (cl:* a20 b02) (cl:* a21 b12) (cl:* a22 b22) (cl:* a23 b32))
           o32 (+ (cl:* a30 b02) (cl:* a31 b12) (cl:* a32 b22) (cl:* a33 b32))
           o03 (+ (cl:* a00 b03) (cl:* a01 b13) (cl:* a02 b23) (cl:* a03 b33))
           o13 (+ (cl:* a10 b03) (cl:* a11 b13) (cl:* a12 b23) (cl:* a13 b33))
           o23 (+ (cl:* a20 b03) (cl:* a21 b13) (cl:* a22 b23) (cl:* a23 b33))
           o33 (+ (cl:* a30 b03) (cl:* a31 b13) (cl:* a32 b23) (cl:* a33 b33))))
  out)

(declaim (inline *))
(declaim (ftype (function (matrix matrix) matrix) *))
(defun * (matrix1 matrix2)
  "Calculate the product of MATRIX1 and MATRIX2, storing the result in a freshly allocated matrix."
  (*! (zero) matrix1 matrix2))

(declaim (inline translation-to-vec3!))
(declaim (ftype (function (v3:vec matrix) v3:vec) translation-to-vec3!))
(defun translation-to-vec3! (out matrix)
  "Copy the translation column of MATRIX to the existing vector, OUT."
  (v3:with-components ((o out))
    (with-components ((m matrix))
      (psetf ox m03 oy m13 oz m23)))
  out)

(declaim (inline translation-to-vec3))
(declaim (ftype (function (matrix) v3:vec) translation-to-vec3))
(defun translation-to-vec3 (matrix)
  "Copy the translation column of MATRIX to a freshly allocated vector."
  (translation-to-vec3! (v3:zero) matrix))

(declaim (inline translation-from-vec3!))
(declaim (ftype (function (matrix v3:vec) matrix) translation-from-vec3!))
(defun translation-from-vec3! (matrix vec)
  "Copy the components of VEC to the translation column of MATRIX. This destructively modifies
MATRIX."
  (with-components ((m matrix))
    (v3:with-components ((v vec))
      (psetf m03 vx m13 vy m23 vz)))
  matrix)

(declaim (inline translation-from-vec3))
(declaim (ftype (function (matrix v3:vec) matrix) translation-from-vec3))
(defun translation-from-vec3 (matrix vec)
  "Copy the components of VEC to the translation column of MATRIX. This allocates a fresh matrix,
leaving the original un-modified."
  (translation-from-vec3! (copy matrix) vec))

(declaim (inline translate!))
(declaim (ftype (function (matrix matrix v3:vec) matrix) translate!))
(defun translate! (out matrix vec)
  "Translate MATRIX by VEC, storing the result in the existing matrix, OUT."
  (*! out (translation-from-vec3 (id) vec) matrix))

(declaim (inline translate))
(declaim (ftype (function (matrix v3:vec) matrix) translate))
(defun translate (matrix vec)
  "Translate MATRIX by VEC, storing the result in a freshly allocated matrix."
  (translate! (id) matrix vec))

(declaim (inline copy-rotation!))
(declaim (ftype (function (matrix matrix) matrix) copy-rotation!))
(defun copy-rotation! (out matrix)
  "Copy the rotation components of MATRIX to the existing matrix, OUT."
  (with-components ((o out) (m matrix))
    (psetf o00 m00 o01 m01 o02 m02
           o10 m10 o11 m11 o12 m12
           o20 m20 o21 m21 o22 m22))
  out)

(declaim (inline copy-rotation))
(declaim (ftype (function (matrix) matrix) copy-rotation))
(defun copy-rotation (matrix)
  "Copy the rotation components of MATRIX to a freshly allocated matrix."
  (copy-rotation! (id) matrix))

(declaim (inline rotation-axis-to-vec3!))
(declaim (ftype (function (v3:vec matrix keyword) v3:vec) rotation-axis-to-vec3!))
(defun rotation-axis-to-vec3! (out matrix axis)
  "Copy the rotation axis from MATRIX denoted by the keyword symbol AXIS, to the existing vector,
OUT."
  (v3:with-components ((v out))
    (with-components ((m matrix))
      (ecase axis
        (:x (psetf vx m00 vy m10 vz m20))
        (:y (psetf vx m01 vy m11 vz m21))
        (:z (psetf vx m02 vy m12 vz m22)))))
  out)

(declaim (inline rotation-axis-to-vec3))
(declaim (ftype (function (matrix keyword) v3:vec) rotation-axis-to-vec3))
(defun rotation-axis-to-vec3 (matrix axis)
  "Copy the rotation axis from MATRIX denoted by the keyword symbol AXIS, to a freshly allocated
vector."
  (rotation-axis-to-vec3! (v3:zero) matrix axis))

(declaim (inline rotation-axis-from-vec3!))
(declaim (ftype (function (matrix v3:vec keyword) matrix) rotation-axis-from-vec3!))
(defun rotation-axis-from-vec3! (matrix vec axis)
  "Copy the components of VEC into the rotation axis of MATRIX denoted by the keyword symbol AXIS.
This destructively modifies MATRIX."
  (with-components ((m matrix))
    (v3:with-components ((v vec))
      (ecase axis
        (:x (psetf m00 vx m10 vy m20 vz))
        (:y (psetf m01 vx m11 vy m21 vz))
        (:z (psetf m02 vx m12 vy m22 vz)))))
  matrix)

(declaim (inline rotation-axis-from-vec3))
(declaim (ftype (function (matrix v3:vec keyword) matrix) rotation-axis-from-vec3))
(defun rotation-axis-from-vec3 (matrix vec axis)
  "Copy the components of VEC into the rotation axis of MATRIX denoted by the keyword symbol AXIS.
This allocates a fresh matrix, leaving the original un-modified."
  (rotation-axis-from-vec3! (copy matrix) vec axis))

(declaim (ftype (function (matrix matrix v3:vec) matrix) rotate!))
(defun rotate! (out matrix vec)
  "Rotate MATRIX by the vector of Euler angles, VEC, storing the result in the existing matrix,
OUT."
  (macrolet ((rotate-angle (angle s c &body body)
               `(when (> (abs ,angle) +epsilon+)
                  (let ((,s (sin ,angle))
                        (,c (cos ,angle)))
                    ,@body
                    (*! out out m)))))
    (with-components ((m (id)))
      (v3:with-components ((v vec))
        (copy! out matrix)
        (rotate-angle vz s c
                      (psetf m00 c m01 (- s)
                             m10 s m11 c))
        (rotate-angle vx s c
                      (psetf m00 1.0f0 m01 0.0f0 m02 0.0f0
                             m10 0.0f0 m11 c m12 (- s)
                             m20 0.0f0 m21 s m22 c))
        (rotate-angle vy s c
                      (psetf m00 c m01 0.0f0 m02 s
                             m10 0.0f0 m11 1.0f0 m12 0.0f0
                             m20 (- s) m21 0.0f0 m22 c)))))
  out)

(declaim (inline rotate))
(declaim (ftype (function (matrix v3:vec) matrix) rotate))
(defun rotate (matrix vec)
  "Rotate MATRIX by the vector of Euler angles, VEC, storing the result in a freshly allocated
matrix."
  (rotate! (id) matrix vec))

(declaim (inline scale-to-vec3!))
(declaim (ftype (function (v3:vec matrix) v3:vec) scale-to-vec3!))
(defun scale-to-vec3! (out matrix)
  "Copy the scaling transform of MATRIX to the existing vector, OUT."
  (v3:with-components ((o out))
    (with-components ((m matrix))
      (psetf ox m00 oy m11 oz m22)))
  out)

(declaim (inline scale-to-vec3))
(declaim (ftype (function (matrix) v3:vec) scale-to-vec3))
(defun scale-to-vec3 (matrix)
  "Copy the scaling transform of MATRIX to a freshly allocated vector."
  (scale-to-vec3! (v3:zero) matrix))

(declaim (inline scale-from-vec3!))
(declaim (ftype (function (matrix v3:vec) matrix) scale-from-vec3!))
(defun scale-from-vec3! (matrix vec)
  "Copy the components of VEC to the scaling components of MATRIX. This destructively modifies
MATRIX."
  (with-components ((m matrix))
    (v3:with-components ((v vec))
      (psetf m00 vx m11 vy m22 vz)))
  matrix)

(declaim (inline scale-from-vec3))
(declaim (ftype (function (matrix v3:vec) matrix) scale-from-vec3))
(defun scale-from-vec3 (matrix vec)
  "Copy the components of VEC to the scaling components of MATRIX. This allocates a fresh matrix,
leaving the origin un-modified."
  (scale-from-vec3! (copy matrix) vec))

(declaim (inline scale!))
(declaim (ftype (function (matrix matrix v3:vec) matrix) scale!))
(defun scale! (out matrix vec)
  "Scale MATRIX by each scalar in VEC, storing the result in the existing matrix, OUT."
  (*! out (scale-from-vec3 (id) vec) matrix))

(declaim (inline scale))
(declaim (ftype (function (matrix v3:vec) matrix) scale))
(defun scale (matrix vec)
  "Scale MATRIX by each scalar in VEC, storing the result in a freshly allocated matrix."
  (scale! (id) matrix vec))

(declaim (inline *v4!))
(declaim (ftype (function (v4:vec matrix v4:vec) v4:vec) *v4!))
(defun *v4! (out matrix vec)
  "Calculate the product of MATRIX and VEC, storing the result in the existing vector, OUT."
  (v4:with-components ((v vec) (o out))
    (with-components ((m matrix))
      (psetf ox (+ (cl:* m00 vx) (cl:* m01 vy) (cl:* m02 vz) (cl:* m03 vw))
             oy (+ (cl:* m10 vx) (cl:* m11 vy) (cl:* m12 vz) (cl:* m13 vw))
             oz (+ (cl:* m20 vx) (cl:* m21 vy) (cl:* m22 vz) (cl:* m23 vw))
             ow (+ (cl:* m30 vx) (cl:* m31 vy) (cl:* m32 vz) (cl:* m33 vw)))))
  out)

(declaim (inline *v4))
(declaim (ftype (function (matrix v4:vec) v4:vec) *v4))
(defun *v4 (matrix vec)
  "Calculate the product of MATRIX and VEC, storing the result in a freshly allocated vector."
  (*v4! (v4:zero) matrix vec))

(declaim (inline transpose!))
(declaim (ftype (function (matrix matrix) matrix) transpose!))
(defun transpose! (out matrix)
  "Transpose MATRIX, storing the result in the existing matrix, OUT."
  (with-components ((o (copy! out matrix)))
    (rotatef o01 o10)
    (rotatef o02 o20)
    (rotatef o03 o30)
    (rotatef o12 o21)
    (rotatef o13 o31)
    (rotatef o23 o32))
  out)

(declaim (inline transpose))
(declaim (ftype (function (matrix) matrix) transpose))
(defun transpose (matrix)
  "Transpose MATRIX, storing the result in a freshly allocated matrix."
  (transpose! (id) matrix))

(declaim (ftype (function (matrix) boolean) orthogonalp))
(defun orthogonalp (matrix)
  "Check if MATRIX is orthogonal. An orthogonal matrix is a square matrix with all of its rows (or
columns) being perpendicular to each other, and of unit length."
  (~ (* matrix (transpose matrix)) +id+))

(declaim (ftype (function (matrix matrix) matrix) orthonormalize!))
(defun orthonormalize! (out matrix)
  "Orthogonalize a matrix using the modified Gram-Schmidt process (MGS), storing the result in the
existing matrix, OUT."
  (let* ((x (rotation-axis-to-vec3 matrix :x))
         (y (rotation-axis-to-vec3 matrix :y))
         (z (rotation-axis-to-vec3 matrix :z)))
    (v3:normalize! x x)
    (v3:normalize! y (v3:- y (v3:scale x (v3:dot y x))))
    (v3:cross! z x y)
    (rotation-axis-from-vec3! out x :x)
    (rotation-axis-from-vec3! out y :y)
    (rotation-axis-from-vec3! out z :z))
  out)

(declaim (ftype (function (matrix) matrix) orthonormalize))
(declaim (inline orthonormalize))
(defun orthonormalize (matrix)
  "Orthogonalize a matrix using the modified Gram-Schmidt process (MGS), storing the result in a
freshly allocated matrix."
  (orthonormalize! (id) matrix))

(declaim (inline trace))
(declaim (ftype (function (matrix) single-float) trace))
(defun trace (matrix)
  "Calculates the sum of the components along the main diagonal of MATRIX."
  (with-components ((m matrix))
    (+ m00 m11 m22 m33)))

(declaim (inline diagonalp))
(declaim (ftype (function (matrix) boolean) diagonalp))
(defun diagonalp (matrix)
  "Check if the components outside of the main diagonal of MATRIX are all zero."
  (with-components ((m matrix))
    (and (zerop m10)
         (zerop m20)
         (zerop m30)
         (zerop m01)
         (zerop m21)
         (zerop m31)
         (zerop m02)
         (zerop m12)
         (zerop m32)
         (zerop m03)
         (zerop m13)
         (zerop m23))))

(declaim (inline main-diagonal!))
(declaim (ftype (function (v4:vec matrix) v4:vec) main-diagonal!))
(defun main-diagonal! (out matrix)
  "Copy the components along the main diagonal of MATRIX to the existing vector, OUT."
  (with-components ((m matrix))
    (v4:with-components ((v out))
      (setf vx m00 vy m11 vz m22 vw m33)))
  out)

(declaim (inline main-diagonal))
(declaim (ftype (function (matrix) v4:vec) main-diagonal))
(defun main-diagonal (matrix)
  "Copy the components along the main diagonal of MATRIX to a freshly allocated vector."
  (main-diagonal! (v4:zero) matrix))

(declaim (inline anti-diagonal!))
(declaim (ftype (function (v4:vec matrix) v4:vec) anti-diagonal!))
(defun anti-diagonal! (out matrix)
  "Copy the components along the anti-diagonal of MATRIX to the existing vector, OUT."
  (with-components ((m matrix))
    (v4:with-components ((v out))
      (setf vx m03 vy m12 vz m21 vw m30)))
  out)

(declaim (inline anti-diagonal))
(declaim (ftype (function (matrix) v4:vec) anti-diagonal))
(defun anti-diagonal (matrix)
  "Copy the components along the anti-diagonal of MATRIX to a freshly allocated vector."
  (anti-diagonal! (v4:zero) matrix))

(declaim (inline determinant))
(declaim (ftype (function (matrix) single-float) determinant))
(defun determinant (matrix)
  "Calculate the determinant of MATRIX. Returns a scalar."
  (with-components ((m matrix))
    (- (+ (cl:* m00 m11 m22 m33) (cl:* m00 m12 m23 m31) (cl:* m00 m13 m21 m32)
          (cl:* m01 m10 m23 m32) (cl:* m01 m12 m20 m33) (cl:* m01 m13 m22 m30)
          (cl:* m02 m10 m21 m33) (cl:* m02 m11 m23 m30) (cl:* m02 m13 m20 m31)
          (cl:* m03 m10 m22 m31) (cl:* m03 m11 m20 m32) (cl:* m03 m12 m21 m30))
       (cl:* m00 m11 m23 m32) (cl:* m00 m12 m21 m33) (cl:* m00 m13 m22 m31)
       (cl:* m01 m10 m22 m33) (cl:* m01 m12 m23 m30) (cl:* m01 m13 m20 m32)
       (cl:* m02 m10 m23 m31) (cl:* m02 m11 m20 m33) (cl:* m02 m13 m21 m30)
       (cl:* m03 m10 m21 m32) (cl:* m03 m11 m22 m30) (cl:* m03 m12 m20 m31))))

(declaim (inline invert-orthogonal!))
(declaim (ftype (function (matrix matrix) matrix) invert-orthogonal!))
(defun invert-orthogonal! (out matrix)
  "Invert MATRIX if its rotation sub-matrix is an orthogonal matrix, storing the result in the
existing matrix, OUT.

Note: This will only work with matrices that have an orthogonal rotation sub-matrix.
See INVERT! for other cases."
  (copy! out matrix)
  (with-components ((o out))
    (rotatef o10 o01)
    (rotatef o20 o02)
    (rotatef o21 o12)
    (psetf o03 (+ (cl:* o00 (- o03)) (cl:* o01 (- o13)) (cl:* o02 (- o23)))
           o13 (+ (cl:* o10 (- o03)) (cl:* o11 (- o13)) (cl:* o12 (- o23)))
           o23 (+ (cl:* o20 (- o03)) (cl:* o21 (- o13)) (cl:* o22 (- o23)))))
  out)

(declaim (inline invert-orthogonal))
(declaim (ftype (function (matrix) matrix) invert-orthogonal))
(defun invert-orthogonal (matrix)
  "Invert MATRIX if its rotation sub-matrix is an orthogonal matrix, storing the result in a freshly
allocated matrix.

Note: This will only work with matrices that have an orthogonal rotation sub-matrix.
See INVERT for other cases."
  (invert-orthogonal! (id) matrix))

(declaim (ftype (function (matrix matrix) matrix) invert!))
(defun invert! (out matrix)
  "Invert MATRIX, storing the result in the existing matrix, OUT.

Note: A matrix with a determinant of zero cannot be inverted, and will raise an error.

Note: This method is slower than INVERT-ORTHOGONAL!, but not all matrices can be inverted with the
fast method.

See INVERT-ORTHOGONAL!"
  (let ((determinant (determinant matrix)))
    (when (< (abs determinant) +epsilon+)
      (error "Cannot invert a matrix with a determinant of zero."))
    (with-components ((o out) (m matrix))
      (psetf o00 (/ (- (+ (cl:* m11 m22 m33) (cl:* m12 m23 m31) (cl:* m13 m21 m32))
                       (cl:* m11 m23 m32) (cl:* m12 m21 m33) (cl:* m13 m22 m31))
                    determinant)
             o01 (/ (- (+ (cl:* m01 m23 m32) (cl:* m02 m21 m33) (cl:* m03 m22 m31))
                       (cl:* m01 m22 m33) (cl:* m02 m23 m31) (cl:* m03 m21 m32))
                    determinant)
             o02 (/ (- (+ (cl:* m01 m12 m33) (cl:* m02 m13 m31) (cl:* m03 m11 m32))
                       (cl:* m01 m13 m32) (cl:* m02 m11 m33) (cl:* m03 m12 m31))
                    determinant)
             o03 (/ (- (+ (cl:* m01 m13 m22) (cl:* m02 m11 m23) (cl:* m03 m12 m21))
                       (cl:* m01 m12 m23) (cl:* m02 m13 m21) (cl:* m03 m11 m22))
                    determinant)
             o10 (/ (- (+ (cl:* m10 m23 m32) (cl:* m12 m20 m33) (cl:* m13 m22 m30))
                       (cl:* m10 m22 m33) (cl:* m12 m23 m30) (cl:* m13 m20 m32))
                    determinant)
             o11 (/ (- (+ (cl:* m00 m22 m33) (cl:* m02 m23 m30) (cl:* m03 m20 m32))
                       (cl:* m00 m23 m32) (cl:* m02 m20 m33) (cl:* m03 m22 m30))
                    determinant)
             o12 (/ (- (+ (cl:* m00 m13 m32) (cl:* m02 m10 m33) (cl:* m03 m12 m30))
                       (cl:* m00 m12 m33) (cl:* m02 m13 m30) (cl:* m03 m10 m32))
                    determinant)
             o13 (/ (- (+ (cl:* m00 m12 m23) (cl:* m02 m13 m20) (cl:* m03 m10 m22))
                       (cl:* m00 m13 m22) (cl:* m02 m10 m23) (cl:* m03 m12 m20))
                    determinant)
             o20 (/ (- (+ (cl:* m10 m21 m33) (cl:* m11 m23 m30) (cl:* m13 m20 m31))
                       (cl:* m10 m23 m31) (cl:* m11 m20 m33) (cl:* m13 m21 m30))
                    determinant)
             o21 (/ (- (+ (cl:* m00 m23 m31) (cl:* m01 m20 m33) (cl:* m03 m21 m30))
                       (cl:* m00 m21 m33) (cl:* m01 m23 m30) (cl:* m03 m20 m31))
                    determinant)
             o22 (/ (- (+ (cl:* m00 m11 m33) (cl:* m01 m13 m30) (cl:* m03 m10 m31))
                       (cl:* m00 m13 m31) (cl:* m01 m10 m33) (cl:* m03 m11 m30))
                    determinant)
             o23 (/ (- (+ (cl:* m00 m13 m21) (cl:* m01 m10 m23) (cl:* m03 m11 m20))
                       (cl:* m00 m11 m23) (cl:* m01 m13 m20) (cl:* m03 m10 m21))
                    determinant)
             o30 (/ (- (+ (cl:* m10 m22 m31) (cl:* m11 m20 m32) (cl:* m12 m21 m30))
                       (cl:* m10 m21 m32) (cl:* m11 m22 m30) (cl:* m12 m20 m31))
                    determinant)
             o31 (/ (- (+ (cl:* m00 m21 m32) (cl:* m01 m22 m30) (cl:* m02 m20 m31))
                       (cl:* m00 m22 m31) (cl:* m01 m20 m32) (cl:* m02 m21 m30))
                    determinant)
             o32 (/ (- (+ (cl:* m00 m12 m31) (cl:* m01 m10 m32) (cl:* m02 m11 m30))
                       (cl:* m00 m11 m32) (cl:* m01 m12 m30) (cl:* m02 m10 m31))
                    determinant)
             o33 (/ (- (+ (cl:* m00 m11 m22) (cl:* m01 m12 m20) (cl:* m02 m10 m21))
                       (cl:* m00 m12 m21) (cl:* m01 m10 m22) (cl:* m02 m11 m20))
                    determinant))))
  out)

(declaim (inline invert))
(declaim (ftype (function (matrix) matrix) invert))
(defun invert (matrix)
  "Invert MATRIX, storing the result in a freshly allocated matrix.

Note: A matrix with a determinant of zero cannot be inverted, and will raise an error.

Note: This method is slower than INVERT-ORTHOGONAL, but not all matrices can be inverted with the
fast method.

See INVERT-ORTHOGONAL."
  (invert! (id) matrix))

(declaim (ftype (function (matrix v3:vec v3:vec v3:vec) matrix) view!))
(defun view! (out eye target up)
  "Create a view matrix, storing the result in the existing matrix, OUT."
  (let ((f (v3:zero))
        (s (v3:zero))
        (u (v3:zero))
        (inv-eye (v3:zero))
        (translation (id)))
    (with-components ((o (id! out)))
      (v3:with-components ((f (v3:normalize! f (v3:-! f target eye)))
                           (s (v3:normalize! s (v3:cross! s f up)))
                           (u (v3:cross! u s f)))
        (psetf o00 sx o10 ux o20 (- fx)
               o01 sy o11 uy o12 (- fy)
               o02 sz o12 uz o22 (- fz))
        (translation-from-vec3! translation (v3:negate! inv-eye eye))
        (*! out out translation))))
  out)

(declaim (inline view))
(declaim (ftype (function (v3:vec v3:vec v3:vec) matrix) view))
(defun view (eye target up)
  "Create a view matrix, storing the result in a freshly allocated matrix."
  (view! (id) eye target up))

(declaim (ftype (function (matrix real real real real real real) matrix) orthographic-projection!))
(defun orthographic-projection! (out left right bottom top near far)
  "Create an orthographic projection matrix, storing the result in the existing matrix, OUT."
  (let ((right-left (float (- right left) 1.0f0))
        (top-bottom (float (- top bottom) 1.0f0))
        (far-near (float (- far near) 1.0f0)))
    (with-components ((m (id! out)))
      (psetf m00 (/ 2.0f0 right-left)
             m03 (- (/ (+ right left) right-left))
             m11 (/ 2.0f0 top-bottom)
             m13 (- (/ (+ top bottom) top-bottom))
             m22 (/ -2.0f0 far-near)
             m23 (- (/ (+ far near) far-near))))
    out))

(declaim (inline orthographic))
(declaim (ftype (function (real real real real real real) matrix) orthographic-projection))
(defun orthographic-projection (left right bottom top near far)
  "Create an orthographic projection matrix, storing the result in a freshly allocated matrix."
  (orthographic-projection! (id) left right bottom top near far))

(declaim (ftype (function (matrix real real real real) matrix) perspective-projection!))
(defun perspective-projection! (out fov aspect near far)
  "Create a perspective projection matrix, storing the result in the existing matrix, OUT."
  (let ((f (float (/ (tan (/ fov 2))) 1.0f0))
        (z (float (- near far) 1.0f0)))
    (with-components ((m (zero! out)))
      (psetf m00 (/ f aspect)
             m11 f
             m22 (/ (+ near far) z)
             m23 (/ (cl:* 2 near far) z)
             m32 -1.0f0)))
  out)

(declaim (inline perspective-projection))
(declaim (ftype (function (real real real real) matrix) perspective-projection))
(defun perspective-projection (fov aspect near far)
  "Create a perspective projection matrix, storing the result in a freshly allocated matrix."
  (perspective-projection! (id) fov aspect near far))
