(in-package #:cl-user)

(defpackage #:box.math.mat4
  (:local-nicknames (#:% #:box.math.internal)
                    (#:v3 #:box.math.vec3)
                    (#:v4 #:box.math.vec4))
  (:use #:cl)
  (:shadow
   #:=
   #:+
   #:-
   #:*
   #:trace)
  (:export
   #:matrix
   #:with-matrices
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
   #:get-translation!
   #:get-translation
   #:set-translation!
   #:set-translation
   #:translate!
   #:translate
   #:copy-rotation!
   #:copy-rotation
   #:rotation-axis-to-vec3!
   #:rotation-axis-to-vec3
   #:rotation-axis-from-vec3!
   #:rotation-axis-from-vec3
   #:rotate!
   #:rotate
   #:get-scale!
   #:get-scale
   #:set-scale!
   #:set-scale
   #:scale!
   #:scale
   #:*v4!
   #:*v4
   #:transpose!
   #:transpose
   #:orthogonal-p
   #:orthonormalize!
   #:orthonormalize
   #:trace
   #:diagonal-p
   #:main-diagonal!
   #:main-diagonal
   #:anti-diagonal!
   #:anti-diagonal
   #:determinant
   #:invert-orthogonal!
   #:invert-orthogonal
   #:invert!
   #:invert
   #:set-view!
   #:set-view
   #:set-projection/orthographic!
   #:set-projection/orthographic
   #:set-projection/perspective!
   #:set-projection/perspective))

(in-package #:box.math.mat4)

;;; Structure

(deftype matrix () '(simple-array single-float (16)))

(defstruct (matrix (:type (vector single-float))
                   (:constructor %make (m00 m01 m02 m03
                                        m10 m11 m12 m13
                                        m20 m21 m22 m23
                                        m30 m31 m32 m33))
                   (:conc-name nil)
                   (:copier nil))
  "A 4x4 column-major matrix consisting of column vectors. This represents a
complete 3-dimensional transformation matrix."
  (m00 0f0 :type single-float)
  (m10 0f0 :type single-float)
  (m20 0f0 :type single-float)
  (m30 0f0 :type single-float)
  (m01 0f0 :type single-float)
  (m11 0f0 :type single-float)
  (m21 0f0 :type single-float)
  (m31 0f0 :type single-float)
  (m02 0f0 :type single-float)
  (m12 0f0 :type single-float)
  (m22 0f0 :type single-float)
  (m32 0f0 :type single-float)
  (m03 0f0 :type single-float)
  (m13 0f0 :type single-float)
  (m23 0f0 :type single-float)
  (m33 0f0 :type single-float))

(defmacro with-matrices (((prefix matrix) &rest rest) &body body)
  "A convenience macro for concisely accessing the components of matrices."
  `(with-accessors ((,prefix identity)
                    (,(%::make-accessor-symbol prefix "00") m00)
                    (,(%::make-accessor-symbol prefix "01") m01)
                    (,(%::make-accessor-symbol prefix "02") m02)
                    (,(%::make-accessor-symbol prefix "03") m03)
                    (,(%::make-accessor-symbol prefix "10") m10)
                    (,(%::make-accessor-symbol prefix "11") m11)
                    (,(%::make-accessor-symbol prefix "12") m12)
                    (,(%::make-accessor-symbol prefix "13") m13)
                    (,(%::make-accessor-symbol prefix "20") m20)
                    (,(%::make-accessor-symbol prefix "21") m21)
                    (,(%::make-accessor-symbol prefix "22") m22)
                    (,(%::make-accessor-symbol prefix "23") m23)
                    (,(%::make-accessor-symbol prefix "30") m30)
                    (,(%::make-accessor-symbol prefix "31") m31)
                    (,(%::make-accessor-symbol prefix "32") m32)
                    (,(%::make-accessor-symbol prefix "33") m33))
       ,matrix
     ,(if rest
          `(with-matrices ,rest ,@body)
          `(progn ,@body))))

;;; Constants

(au:define-constant +zero+
    (make-array 16 :element-type 'single-float
                   :initial-contents '(0f0 0f0 0f0 0f0
                                       0f0 0f0 0f0 0f0
                                       0f0 0f0 0f0 0f0
                                       0f0 0f0 0f0 0f0))
  :test #'equalp
  :documentation "A matrix with each component as zero.")

(au:define-constant +id+
    (make-array 16 :element-type 'single-float
                   :initial-contents '(1f0 0f0 0f0 0f0
                                       0f0 1f0 0f0 0f0
                                       0f0 0f0 1f0 0f0
                                       0f0 0f0 0f0 1f0))
  :test #'equalp
  :documentation "An identity matrix.")

;;; Operations

(declaim (inline make))
(declaim (ftype (function
                 (real real real real real real real real real real real real
                       real real real real)
                 matrix)
                make))
(defun make (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33)
  "Create a new matrix."
  (%make (float m00 1f0) (float m01 1f0) (float m02 1f0) (float m03 1f0)
         (float m10 1f0) (float m11 1f0) (float m12 1f0) (float m13 1f0)
         (float m20 1f0) (float m21 1f0) (float m22 1f0) (float m23 1f0)
         (float m30 1f0) (float m31 1f0) (float m32 1f0)
         (float m33 1f0)))

(declaim (inline zero!))
(declaim (ftype (function (matrix) matrix) zero!))
(defun zero! (matrix)
  "Set each component of MATRIX to zero."
  (with-matrices ((m matrix))
    (psetf m00 0f0 m01 0f0 m02 0f0 m03 0f0
           m10 0f0 m11 0f0 m12 0f0 m13 0f0
           m20 0f0 m21 0f0 m22 0f0 m23 0f0
           m30 0f0 m31 0f0 m32 0f0 m33 0f0))
  matrix)

(declaim (inline zero))
(declaim (ftype (function () matrix) zero))
(defun zero ()
  "Create a new matrix with all components initialized to zero."
  (%make 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0))

(declaim (inline id!))
(declaim (ftype (function (matrix) matrix) id!))
(defun id! (matrix)
  "Modify MATRIX to be an identity matrix."
  (with-matrices ((m matrix))
    (psetf m00 1f0 m01 0f0 m02 0f0 m03 0f0
           m10 0f0 m11 1f0 m12 0f0 m13 0f0
           m20 0f0 m21 0f0 m22 1f0 m23 0f0
           m30 0f0 m31 0f0 m32 0f0 m33 1f0))
  matrix)

(declaim (inline id))
(declaim (ftype (function () matrix) id))
(defun id ()
  "Create an identity matrix."
  (%make 1f0 0f0 0f0 0f0 0f0 1f0 0f0 0f0 0f0 0f0 1f0 0f0 0f0 0f0 0f0 1f0))

(declaim (inline =))
(declaim (ftype (function (matrix matrix) boolean) =))
(defun = (matrix1 matrix2)
  "Check if all components of MATRIX1 are numerically equal to the components of
MATRIX2."
  (with-matrices ((a matrix1)
                  (b matrix2))
    (and (cl:= a00 b00) (cl:= a01 b01) (cl:= a02 b02) (cl:= a03 b03)
         (cl:= a10 b10) (cl:= a11 b11) (cl:= a12 b12) (cl:= a13 b13)
         (cl:= a20 b20) (cl:= a21 b21) (cl:= a22 b22) (cl:= a23 b23)
         (cl:= a30 b30) (cl:= a31 b31) (cl:= a32 b32) (cl:= a33 b33))))

(declaim (inline ~))
(declaim (ftype (function (matrix matrix &key (:tolerance single-float))
                          boolean)
                ~))
(defun ~ (matrix1 matrix2 &key (tolerance 1e-7))
  "Check if all components of MATRIX1 are approximately equal to the components
of MATRIX2, according to TOLERANCE."
  (with-matrices ((a matrix1)
                  (b matrix2))
    (and (%::~ a00 b00 tolerance)
         (%::~ a01 b01 tolerance)
         (%::~ a02 b02 tolerance)
         (%::~ a03 b03 tolerance)
         (%::~ a10 b10 tolerance)
         (%::~ a11 b11 tolerance)
         (%::~ a12 b12 tolerance)
         (%::~ a13 b13 tolerance)
         (%::~ a20 b20 tolerance)
         (%::~ a21 b21 tolerance)
         (%::~ a22 b22 tolerance)
         (%::~ a23 b23 tolerance)
         (%::~ a30 b30 tolerance)
         (%::~ a31 b31 tolerance)
         (%::~ a32 b32 tolerance)
         (%::~ a33 b33 tolerance))))

(declaim (inline copy!))
(declaim (ftype (function (matrix matrix) matrix) copy!))
(defun copy! (out matrix)
  "Copy each component of MATRIX to the existing matrix, OUT."
  (with-matrices ((o out)
                  (m matrix))
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

(declaim (ftype (function (matrix matrix &key (:min single-float)
                                  (:max single-float))
                          matrix)
                clamp!))
(defun clamp! (out matrix
               &key (min most-negative-single-float)
                 (max most-positive-single-float))
  "Clamp each component of MATRIX within the range of [MIN, MAX], storing the
result in the existing matrix, OUT."
  (with-matrices ((o out)
                  (m matrix))
    (psetf o00 (au:clamp m00 min max)
           o01 (au:clamp m01 min max)
           o02 (au:clamp m02 min max)
           o03 (au:clamp m03 min max)
           o10 (au:clamp m10 min max)
           o11 (au:clamp m11 min max)
           o12 (au:clamp m12 min max)
           o13 (au:clamp m13 min max)
           o20 (au:clamp m20 min max)
           o21 (au:clamp m21 min max)
           o22 (au:clamp m22 min max)
           o23 (au:clamp m23 min max)
           o30 (au:clamp m30 min max)
           o31 (au:clamp m31 min max)
           o32 (au:clamp m32 min max)
           o33 (au:clamp m33 min max)))
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
  (with-matrices ((o out)
                  (a matrix1)
                  (b matrix2))
    (psetf o00 (cl:+ a00 b00)
           o10 (cl:+ a10 b10)
           o20 (cl:+ a20 b20)
           o30 (cl:+ a30 b30)
           o01 (cl:+ a01 b01)
           o11 (cl:+ a11 b11)
           o21 (cl:+ a21 b21)
           o31 (cl:+ a31 b31)
           o02 (cl:+ a02 b02)
           o12 (cl:+ a12 b12)
           o22 (cl:+ a22 b22)
           o32 (cl:+ a32 b32)
           o03 (cl:+ a03 b03)
           o13 (cl:+ a13 b13)
           o23 (cl:+ a23 b23)
           o33 (cl:+ a33 b33)))
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
  (with-matrices ((o out)
                  (a matrix1)
                  (b matrix2))
    (psetf o00 (cl:- a00 b00)
           o10 (cl:- a10 b10)
           o20 (cl:- a20 b20)
           o30 (cl:- a30 b30)
           o01 (cl:- a01 b01)
           o11 (cl:- a11 b11)
           o21 (cl:- a21 b21)
           o31 (cl:- a31 b31)
           o02 (cl:- a02 b02)
           o12 (cl:- a12 b12)
           o22 (cl:- a22 b22)
           o32 (cl:- a32 b32)
           o03 (cl:- a03 b03)
           o13 (cl:- a13 b13)
           o23 (cl:- a23 b23)
           o33 (cl:- a33 b33)))
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
  (with-matrices ((o out)
                  (a matrix1)
                  (b matrix2))
    (psetf o00 (cl:+ (cl:* a00 b00) (cl:* a01 b10) (cl:* a02 b20)
                     (cl:* a03 b30))
           o10 (cl:+ (cl:* a10 b00) (cl:* a11 b10) (cl:* a12 b20)
                     (cl:* a13 b30))
           o20 (cl:+ (cl:* a20 b00) (cl:* a21 b10) (cl:* a22 b20)
                     (cl:* a23 b30))
           o30 (cl:+ (cl:* a30 b00) (cl:* a31 b10) (cl:* a32 b20)
                     (cl:* a33 b30))
           o01 (cl:+ (cl:* a00 b01) (cl:* a01 b11) (cl:* a02 b21)
                     (cl:* a03 b31))
           o11 (cl:+ (cl:* a10 b01) (cl:* a11 b11) (cl:* a12 b21)
                     (cl:* a13 b31))
           o21 (cl:+ (cl:* a20 b01) (cl:* a21 b11) (cl:* a22 b21)
                     (cl:* a23 b31))
           o31 (cl:+ (cl:* a30 b01) (cl:* a31 b11) (cl:* a32 b21)
                     (cl:* a33 b31))
           o02 (cl:+ (cl:* a00 b02) (cl:* a01 b12) (cl:* a02 b22)
                     (cl:* a03 b32))
           o12 (cl:+ (cl:* a10 b02) (cl:* a11 b12) (cl:* a12 b22)
                     (cl:* a13 b32))
           o22 (cl:+ (cl:* a20 b02) (cl:* a21 b12) (cl:* a22 b22)
                     (cl:* a23 b32))
           o32 (cl:+ (cl:* a30 b02) (cl:* a31 b12) (cl:* a32 b22)
                     (cl:* a33 b32))
           o03 (cl:+ (cl:* a00 b03) (cl:* a01 b13) (cl:* a02 b23)
                     (cl:* a03 b33))
           o13 (cl:+ (cl:* a10 b03) (cl:* a11 b13) (cl:* a12 b23)
                     (cl:* a13 b33))
           o23 (cl:+ (cl:* a20 b03) (cl:* a21 b13) (cl:* a22 b23)
                     (cl:* a23 b33))
           o33 (cl:+ (cl:* a30 b03) (cl:* a31 b13) (cl:* a32 b23)
                     (cl:* a33 b33))))
  out)

(declaim (inline *))
(declaim (ftype (function (matrix matrix) matrix) *))
(defun * (matrix1 matrix2)
  "Calculate the product of MATRIX1 and MATRIX2, storing the result in a freshly
allocated matrix."
  (*! (zero) matrix1 matrix2))

(declaim (inline get-translation!))
(declaim (ftype (function (v3:vec matrix) v3:vec) get-translation!))
(defun get-translation! (out matrix)
  "Copy the translation column of MATRIX to the existing vector, OUT."
  (with-matrices ((m matrix))
    (v3:with-vectors ((o out))
      (psetf ox m03 oy m13 oz m23)))
  out)

(declaim (inline get-translation))
(declaim (ftype (function (matrix) v3:vec) get-translation))
(defun get-translation (matrix)
  "Copy the translation column of MATRIX to a freshly allocated vector."
  (get-translation! (v3:zero) matrix))

(declaim (inline set-translation!))
(declaim (ftype (function (matrix matrix v3:vec) matrix) set-translation!))
(defun set-translation! (out matrix vec)
  "Copy the components of VEC to the translation column of MATRIX. This
destructively modifies MATRIX."
  (with-matrices ((o out))
    (v3:with-vectors ((v vec))
      (copy! out matrix)
      (psetf o03 vx o13 vy o23 vz)))
  out)

(declaim (inline set-translation))
(declaim (ftype (function (matrix v3:vec) matrix) set-translation))
(defun set-translation (matrix vec)
  "Copy the components of VEC to the translation column of MATRIX. This
allocates a fresh matrix, leaving the original un-modified."
  (set-translation! (copy matrix) matrix vec))

(declaim (inline translate!))
(declaim (ftype (function (matrix matrix v3:vec) matrix) translate!))
(defun translate! (out matrix vec)
  "Translate MATRIX by VEC, storing the result in the existing matrix, OUT."
  (*! out (set-translation (id) vec) matrix))

(declaim (inline translate))
(declaim (ftype (function (matrix v3:vec) matrix) translate))
(defun translate (matrix vec)
  "Translate MATRIX by VEC, storing the result in a freshly allocated matrix."
  (translate! (id) matrix vec))

(declaim (inline copy-rotation!))
(declaim (ftype (function (matrix matrix) matrix) copy-rotation!))
(defun copy-rotation! (out matrix)
  "Copy the rotation components of MATRIX to the existing matrix, OUT."
  (with-matrices ((o out)
                  (m matrix))
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
(declaim (ftype (function (v3:vec matrix keyword) v3:vec)
                rotation-axis-to-vec3!))
(defun rotation-axis-to-vec3! (out matrix axis)
  "Copy the rotation axis from MATRIX denoted by the keyword symbol AXIS, to the
existing vector, OUT."
  (v3:with-vectors ((v out))
    (with-matrices ((m matrix))
      (ecase axis
        (:x (psetf vx m00 vy m10 vz m20))
        (:y (psetf vx m01 vy m11 vz m21))
        (:z (psetf vx m02 vy m12 vz m22)))))
  out)

(declaim (inline rotation-axis-to-vec3))
(declaim (ftype (function (matrix keyword) v3:vec) rotation-axis-to-vec3))
(defun rotation-axis-to-vec3 (matrix axis)
  "Copy the rotation axis from MATRIX denoted by the keyword symbol AXIS, to a
freshly allocated vector."
  (rotation-axis-to-vec3! (v3:zero) matrix axis))

(declaim (inline rotation-axis-from-vec3!))
(declaim (ftype (function (matrix v3:vec keyword) matrix)
                rotation-axis-from-vec3!))
(defun rotation-axis-from-vec3! (matrix vec axis)
  "Copy the components of VEC into the rotation axis of MATRIX denoted by the
keyword symbol AXIS. This destructively modifies MATRIX."
  (with-matrices ((m matrix))
    (v3:with-vectors ((v vec))
      (ecase axis
        (:x (psetf m00 vx m10 vy m20 vz))
        (:y (psetf m01 vx m11 vy m21 vz))
        (:z (psetf m02 vx m12 vy m22 vz)))))
  matrix)

(declaim (inline rotation-axis-from-vec3))
(declaim (ftype (function (matrix v3:vec keyword) matrix)
                rotation-axis-from-vec3))
(defun rotation-axis-from-vec3 (matrix vec axis)
  "Copy the components of VEC into the rotation axis of MATRIX denoted by the
keyword symbol AXIS. This allocates a fresh matrix, leaving the original
un-modified."
  (rotation-axis-from-vec3! (copy matrix) vec axis))

(declaim (ftype (function (matrix matrix v3:vec &key (:space keyword)) matrix)
                rotate!))
(defun rotate! (out matrix vec &key (space :local))
  "Rotate MATRIX by the vector of Euler angles, VEC, storing the result in the
existing matrix, OUT."
  (macrolet ((rotate-angle (angle s c &body body)
               `(when (cl:> (abs ,angle) 1e-7)
                  (let ((,s (sin ,angle))
                        (,c (cos ,angle)))
                    ,@body
                    (ecase space
                      (:local (*! out out m))
                      (:world (*! out m out)))))))
    (with-matrices ((m (id)))
      (v3:with-vectors ((v vec))
        (copy! out matrix)
        (rotate-angle vz s c
                      (psetf m00 c m01 (cl:- s)
                             m10 s m11 c))
        (rotate-angle vx s c
                      (psetf m00 1f0 m01 0f0 m02 0f0
                             m10 0f0 m11 c m12 (cl:- s)
                             m20 0f0 m21 s m22 c))
        (rotate-angle vy s c
                      (psetf m00 c m01 0f0 m02 s
                             m10 0f0 m11 1f0 m12 0f0
                             m20 (cl:- s) m21 0f0 m22 c)))))
  out)

(declaim (inline rotate))
(declaim (ftype (function (matrix v3:vec) matrix) rotate))
(defun rotate (matrix vec)
  "Rotate MATRIX by the vector of Euler angles, VEC, storing the result in a
freshly allocated matrix."
  (rotate! (id) matrix vec))

(declaim (inline get-scale!))
(declaim (ftype (function (v3:vec matrix) v3:vec) get-scale!))
(defun get-scale! (out matrix)
  "Copy the scaling transform of MATRIX to the existing vector, OUT."
  (with-matrices ((m matrix))
    (v3:with-vectors ((o out))
      (psetf ox m00 oy m11 oz m22)))
  out)

(declaim (inline get-scale))
(declaim (ftype (function (matrix) v3:vec) get-scale))
(defun get-scale (matrix)
  "Copy the scaling transform of MATRIX to a freshly allocated vector."
  (get-scale! (v3:zero) matrix))

(declaim (inline set-scale!))
(declaim (ftype (function (matrix matrix v3:vec) matrix) set-scale!))
(defun set-scale! (out matrix vec)
  "Copy the components of VEC to the scaling components of MATRIX. This
destructively modifies MATRIX."
  (with-matrices ((o out))
    (v3:with-vectors ((v vec))
      (copy! out matrix)
      (psetf o00 vx o11 vy o22 vz)))
  out)

(declaim (inline set-scale))
(declaim (ftype (function (matrix v3:vec) matrix) set-scale))
(defun set-scale (matrix vec)
  "Copy the components of VEC to the scaling components of MATRIX. This
allocates a fresh matrix, leaving the origin un-modified."
  (set-scale! (copy matrix) matrix vec))

(declaim (inline scale!))
(declaim (ftype (function (matrix matrix v3:vec) matrix) scale!))
(defun scale! (out matrix vec)
  "Scale MATRIX by each scalar in VEC, storing the result in the existing
matrix, OUT."
  (*! out (set-scale (id) vec) matrix))

(declaim (inline scale))
(declaim (ftype (function (matrix v3:vec) matrix) scale))
(defun scale (matrix vec)
  "Scale MATRIX by each scalar in VEC, storing the result in a freshly allocated
matrix."
  (scale! (id) matrix vec))

(declaim (inline *v4!))
(declaim (ftype (function (v4:vec matrix v4:vec) v4:vec) *v4!))
(defun *v4! (out matrix vec)
  "Calculate the product of MATRIX and VEC, storing the result in the existing
vector, OUT."
  (v4:with-vectors ((v vec)
                    (o out))
    (with-matrices ((m matrix))
      (psetf ox (cl:+ (cl:* m00 vx) (cl:* m01 vy) (cl:* m02 vz)
                      (cl:* m03 vw))
             oy (cl:+ (cl:* m10 vx) (cl:* m11 vy) (cl:* m12 vz)
                      (cl:* m13 vw))
             oz (cl:+ (cl:* m20 vx) (cl:* m21 vy) (cl:* m22 vz)
                      (cl:* m23 vw))
             ow (cl:+ (cl:* m30 vx) (cl:* m31 vy) (cl:* m32 vz)
                      (cl:* m33 vw)))))
  out)

(declaim (inline *v4))
(declaim (ftype (function (matrix v4:vec) v4:vec) *v4))
(defun *v4 (matrix vec)
  "Calculate the product of MATRIX and VEC, storing the result in a freshly
allocated vector."
  (*v4! (v4:zero) matrix vec))

(declaim (inline transpose!))
(declaim (ftype (function (matrix matrix) matrix) transpose!))
(defun transpose! (out matrix)
  "Transpose MATRIX, storing the result in the existing matrix, OUT."
  (with-matrices ((o (copy! out matrix)))
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

(declaim (ftype (function (matrix) boolean) orthogonal-p))
(defun orthogonal-p (matrix)
  "Check if MATRIX is orthogonal. An orthogonal matrix is a square matrix with
all of its rows (or columns) being perpendicular to each other, and of unit
length."
  (~ (* matrix (transpose matrix)) +id+))

(declaim (ftype (function (matrix matrix) matrix) orthonormalize!))
(defun orthonormalize! (out matrix)
  "Orthogonalize a matrix using the modified Gram-Schmidt process (MGS), storing
the result in the existing matrix, OUT."
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
  "Orthogonalize a matrix using the modified Gram-Schmidt process (MGS), storing
the result in a freshly allocated matrix."
  (orthonormalize! (id) matrix))

(declaim (inline trace))
(declaim (ftype (function (matrix) single-float) trace))
(defun trace (matrix)
  "Calculates the sum of the components along the main diagonal of MATRIX."
  (with-matrices ((m matrix))
    (cl:+ m00 m11 m22 m33)))

(declaim (inline diagonal-p))
(declaim (ftype (function (matrix) boolean) diagonal-p))
(defun diagonal-p (matrix)
  "Check if the components outside of the main diagonal of MATRIX are all zero."
  (with-matrices ((m matrix))
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
  "Copy the components along the main diagonal of MATRIX to the existing vector,
OUT."
  (with-matrices ((m matrix))
    (v4:with-vectors ((v out))
      (setf vx m00 vy m11 vz m22 vw m33)))
  out)

(declaim (inline main-diagonal))
(declaim (ftype (function (matrix) v4:vec) main-diagonal))
(defun main-diagonal (matrix)
  "Copy the components along the main diagonal of MATRIX to a freshly allocated
vector."
  (main-diagonal! (v4:zero) matrix))

(declaim (inline anti-diagonal!))
(declaim (ftype (function (v4:vec matrix) v4:vec) anti-diagonal!))
(defun anti-diagonal! (out matrix)
  "Copy the components along the anti-diagonal of MATRIX to the existing vector,
OUT."
  (with-matrices ((m matrix))
    (v4:with-vectors ((v out))
      (setf vx m03 vy m12 vz m21 vw m30)))
  out)

(declaim (inline anti-diagonal))
(declaim (ftype (function (matrix) v4:vec) anti-diagonal))
(defun anti-diagonal (matrix)
  "Copy the components along the anti-diagonal of MATRIX to a freshly allocated
vector."
  (anti-diagonal! (v4:zero) matrix))

(declaim (inline determinant))
(declaim (ftype (function (matrix) single-float) determinant))
(defun determinant (matrix)
  "Calculate the determinant of MATRIX. Returns a scalar."
  (with-matrices ((m matrix))
    (cl:- (cl:+ (cl:* m00 m11 m22 m33) (cl:* m00 m12 m23 m31)
                (cl:* m00 m13 m21 m32) (cl:* m01 m10 m23 m32)
                (cl:* m01 m12 m20 m33) (cl:* m01 m13 m22 m30)
                (cl:* m02 m10 m21 m33) (cl:* m02 m11 m23 m30)
                (cl:* m02 m13 m20 m31) (cl:* m03 m10 m22 m31)
                (cl:* m03 m11 m20 m32) (cl:* m03 m12 m21 m30))
          (cl:* m00 m11 m23 m32) (cl:* m00 m12 m21 m33) (cl:* m00 m13 m22 m31)
          (cl:* m01 m10 m22 m33) (cl:* m01 m12 m23 m30) (cl:* m01 m13 m20 m32)
          (cl:* m02 m10 m23 m31) (cl:* m02 m11 m20 m33) (cl:* m02 m13 m21 m30)
          (cl:* m03 m10 m21 m32) (cl:* m03 m11 m22 m30)
          (cl:* m03 m12 m20 m31))))

(declaim (inline invert-orthogonal!))
(declaim (ftype (function (matrix matrix) matrix) invert-orthogonal!))
(defun invert-orthogonal! (out matrix)
  "Invert MATRIX if its rotation sub-matrix is an orthogonal matrix, storing the
result in the existing matrix, OUT.

Note: This will only work with matrices that have an orthogonal rotation
sub-matrix. See INVERT! for other cases."
  (copy! out matrix)
  (with-matrices ((o out))
    (rotatef o10 o01)
    (rotatef o20 o02)
    (rotatef o21 o12)
    (psetf o03 (cl:+ (cl:* o00 (cl:- o03)) (cl:* o01 (cl:- o13))
                     (cl:* o02 (cl:- o23)))
           o13 (cl:+ (cl:* o10 (cl:- o03)) (cl:* o11 (cl:- o13))
                     (cl:* o12 (cl:- o23)))
           o23 (cl:+ (cl:* o20 (cl:- o03)) (cl:* o21 (cl:- o13))
                     (cl:* o22 (cl:- o23)))))
  out)

(declaim (inline invert-orthogonal))
(declaim (ftype (function (matrix) matrix) invert-orthogonal))
(defun invert-orthogonal (matrix)
  "Invert MATRIX if its rotation sub-matrix is an orthogonal matrix, storing the
result in a freshly allocated matrix.

Note: This will only work with matrices that have an orthogonal rotation
sub-matrix. See INVERT for other cases."
  (invert-orthogonal! (id) matrix))

(declaim (ftype (function (matrix matrix) matrix) invert!))
(defun invert! (out matrix)
  "Invert MATRIX, storing the result in the existing matrix, OUT.

Note: A matrix with a determinant of zero cannot be inverted, and will raise an
error.

Note: This method is slower than INVERT-ORTHOGONAL!, but not all matrices can be
inverted with the fast method.

See INVERT-ORTHOGONAL!"
  (let ((determinant (determinant matrix)))
    (when (< (abs determinant) 1e-7)
      (error "Cannot invert a matrix with a determinant of zero."))
    (with-matrices ((o out)
                    (m matrix))
      (psetf o00 (/ (cl:- (cl:+ (cl:* m11 m22 m33) (cl:* m12 m23 m31)
                                (cl:* m13 m21 m32))
                          (cl:* m11 m23 m32) (cl:* m12 m21 m33)
                          (cl:* m13 m22 m31))
                    determinant)
             o01 (/ (cl:- (cl:+ (cl:* m01 m23 m32) (cl:* m02 m21 m33)
                                (cl:* m03 m22 m31))
                          (cl:* m01 m22 m33) (cl:* m02 m23 m31)
                          (cl:* m03 m21 m32))
                    determinant)
             o02 (/ (cl:- (cl:+ (cl:* m01 m12 m33) (cl:* m02 m13 m31)
                                (cl:* m03 m11 m32))
                          (cl:* m01 m13 m32) (cl:* m02 m11 m33)
                          (cl:* m03 m12 m31))
                    determinant)
             o03 (/ (cl:- (cl:+ (cl:* m01 m13 m22) (cl:* m02 m11 m23)
                                (cl:* m03 m12 m21))
                          (cl:* m01 m12 m23) (cl:* m02 m13 m21)
                          (cl:* m03 m11 m22))
                    determinant)
             o10 (/ (cl:- (cl:+ (cl:* m10 m23 m32) (cl:* m12 m20 m33)
                                (cl:* m13 m22 m30))
                          (cl:* m10 m22 m33) (cl:* m12 m23 m30)
                          (cl:* m13 m20 m32))
                    determinant)
             o11 (/ (cl:- (cl:+ (cl:* m00 m22 m33) (cl:* m02 m23 m30)
                                (cl:* m03 m20 m32))
                          (cl:* m00 m23 m32) (cl:* m02 m20 m33)
                          (cl:* m03 m22 m30))
                    determinant)
             o12 (/ (cl:- (cl:+ (cl:* m00 m13 m32) (cl:* m02 m10 m33)
                                (cl:* m03 m12 m30))
                          (cl:* m00 m12 m33) (cl:* m02 m13 m30)
                          (cl:* m03 m10 m32))
                    determinant)
             o13 (/ (cl:- (cl:+ (cl:* m00 m12 m23) (cl:* m02 m13 m20)
                                (cl:* m03 m10 m22))
                          (cl:* m00 m13 m22) (cl:* m02 m10 m23)
                          (cl:* m03 m12 m20))
                    determinant)
             o20 (/ (cl:- (cl:+ (cl:* m10 m21 m33) (cl:* m11 m23 m30)
                                (cl:* m13 m20 m31))
                          (cl:* m10 m23 m31) (cl:* m11 m20 m33)
                          (cl:* m13 m21 m30))
                    determinant)
             o21 (/ (cl:- (cl:+ (cl:* m00 m23 m31) (cl:* m01 m20 m33)
                                (cl:* m03 m21 m30))
                          (cl:* m00 m21 m33) (cl:* m01 m23 m30)
                          (cl:* m03 m20 m31))
                    determinant)
             o22 (/ (cl:- (cl:+ (cl:* m00 m11 m33) (cl:* m01 m13 m30)
                                (cl:* m03 m10 m31))
                          (cl:* m00 m13 m31) (cl:* m01 m10 m33)
                          (cl:* m03 m11 m30))
                    determinant)
             o23 (/ (cl:- (cl:+ (cl:* m00 m13 m21) (cl:* m01 m10 m23)
                                (cl:* m03 m11 m20))
                          (cl:* m00 m11 m23) (cl:* m01 m13 m20)
                          (cl:* m03 m10 m21))
                    determinant)
             o30 (/ (cl:- (cl:+ (cl:* m10 m22 m31) (cl:* m11 m20 m32)
                                (cl:* m12 m21 m30))
                          (cl:* m10 m21 m32) (cl:* m11 m22 m30)
                          (cl:* m12 m20 m31))
                    determinant)
             o31 (/ (cl:- (cl:+ (cl:* m00 m21 m32) (cl:* m01 m22 m30)
                                (cl:* m02 m20 m31))
                          (cl:* m00 m22 m31) (cl:* m01 m20 m32)
                          (cl:* m02 m21 m30))
                    determinant)
             o32 (/ (cl:- (cl:+ (cl:* m00 m12 m31) (cl:* m01 m10 m32)
                                (cl:* m02 m11 m30))
                          (cl:* m00 m11 m32) (cl:* m01 m12 m30)
                          (cl:* m02 m10 m31))
                    determinant)
             o33 (/ (cl:- (cl:+ (cl:* m00 m11 m22) (cl:* m01 m12 m20)
                                (cl:* m02 m10 m21))
                          (cl:* m00 m12 m21) (cl:* m01 m10 m22)
                          (cl:* m02 m11 m20))
                    determinant))))
  out)

(declaim (inline invert))
(declaim (ftype (function (matrix) matrix) invert))
(defun invert (matrix)
  "Invert MATRIX, storing the result in a freshly allocated matrix.

Note: A matrix with a determinant of zero cannot be inverted, and will raise an
error.

Note: This method is slower than INVERT-ORTHOGONAL, but not all matrices can be
inverted with the fast method.

See INVERT-ORTHOGONAL"
  (invert! (id) matrix))

(declaim (ftype (function (matrix v3:vec v3:vec v3:vec) matrix) set-view!))
(defun set-view! (out eye target up)
  (with-matrices ((o (id! out)))
    (v3:with-vectors ((e eye)
                      (s target)
                      (u up))
      (macrolet ((%normalize (place-x x place-y y place-z z)
                   (au:once-only (x y z)
                     `(let ((denom (sqrt
                                    (cl:+
                                     (cl:* ,x ,x) (cl:* ,y ,y) (cl:* ,z ,z)))))
                        (psetf ,place-x (cl:/ ,x denom)
                               ,place-y (cl:/ ,y denom)
                               ,place-z (cl:/ ,z denom))))))
        (%normalize o20 (cl:- sx ex)
                    o21 (cl:- sy ey)
                    o22 (cl:- sz ez))
        (%normalize o00 (cl:- (cl:* o21 uz) (cl:* o22 uy))
                    o01 (cl:- (cl:* o22 ux) (cl:* o20 uz))
                    o02 (cl:- (cl:* o20 uy) (cl:* o21 ux)))
        (psetf o10 (cl:- (cl:* o01 o22) (cl:* o02 o21))
               o11 (cl:- (cl:* o02 o20) (cl:* o00 o22))
               o12 (cl:- (cl:* o00 o21) (cl:* o01 o20)))
        (psetf o20 (cl:- o20)
               o21 (cl:- o21)
               o22 (cl:- o22))
        (psetf o03 (cl:+ (cl:* o00 (cl:- ex))
                         (cl:* o01 (cl:- ey))
                         (cl:* o02 (cl:- ez))
                         o03)
               o13 (cl:+ (cl:* o10 (cl:- ex))
                         (cl:* o11 (cl:- ey))
                         (cl:* o12 (cl:- ez))
                         o13)
               o23 (cl:+ (cl:* o20 (cl:- ex))
                         (cl:* o21 (cl:- ey))
                         (cl:* o22 (cl:- ez))
                         o23)
               o33 (cl:+ (cl:* o30 (cl:- ex))
                         (cl:* o31 (cl:- ey))
                         (cl:* o32 (cl:- ez))
                         o33)))))
  out)

(declaim (inline set-view))
(declaim (ftype (function (v3:vec v3:vec v3:vec) matrix) set-view))
(defun set-view (eye target up)
  "Create a view matrix, storing the result in a freshly allocated matrix."
  (set-view! (id) eye target up))

(declaim (ftype (function (matrix real real real real real real) matrix)
                set-projection/orthographic!))
(defun set-projection/orthographic! (out left right bottom top near far)
  "Create an orthographic projection matrix, storing the result in the existing
matrix, OUT."
  (let ((right-left (float (cl:- right left) 1f0))
        (top-bottom (float (cl:- top bottom) 1f0))
        (far-near (float (cl:- far near) 1f0)))
    (with-matrices ((m (id! out)))
      (psetf m00 (/ 2f0 right-left)
             m03 (cl:- (/ (cl:+ right left) right-left))
             m11 (/ 2f0 top-bottom)
             m13 (cl:- (/ (cl:+ top bottom) top-bottom))
             m22 (/ -2f0 far-near)
             m23 (cl:- (/ (cl:+ far near) far-near))))
    out))

(declaim (inline set-projection/orthographic))
(declaim (ftype (function (real real real real real real) matrix)
                set-projection/orthographic))
(defun set-projection/orthographic (left right bottom top near far)
  "Create an orthographic projection matrix, storing the result in a freshly
allocated matrix."
  (set-projection/orthographic! (id) left right bottom top near far))

(declaim (ftype (function (matrix real real real real) matrix)
                set-projection/perspective!))
(defun set-projection/perspective! (out fov aspect near far)
  "Create a perspective projection matrix, storing the result in the existing
matrix, OUT."
  (let ((f (float (/ (tan (/ fov 2))) 1f0))
        (z (float (cl:- near far) 1f0)))
    (with-matrices ((m (zero! out)))
      (psetf m00 (/ f aspect)
             m11 f
             m22 (/ (cl:+ near far) z)
             m23 (/ (cl:* 2 near far) z)
             m32 -1f0)))
  out)

(declaim (inline set-projection/perspective))
(declaim (ftype (function (real real real real) matrix)
                set-projection/perspective))
(defun set-projection/perspective (fov aspect near far)
  "Create a perspective projection matrix, storing the result in a freshly
allocated matrix."
  (set-projection/perspective! (id) fov aspect near far))
