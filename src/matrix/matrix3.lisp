(in-package :box.math.mat3)

;;; Structure

(deftype matrix () '(simple-array single-float (9)))

(defstruct (matrix (:type (vector single-float))
                   (:constructor %make (m00 m01 m02
                                        m10 m11 m12
                                        m20 m21 m22))
                   (:conc-name nil)
                   (:copier nil))
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
  `(with-accessors ((,prefix identity)
                    (,(%make-accessor-symbol prefix "00") m00)
                    (,(%make-accessor-symbol prefix "01") m01)
                    (,(%make-accessor-symbol prefix "02") m02)
                    (,(%make-accessor-symbol prefix "10") m10)
                    (,(%make-accessor-symbol prefix "11") m11)
                    (,(%make-accessor-symbol prefix "12") m12)
                    (,(%make-accessor-symbol prefix "20") m20)
                    (,(%make-accessor-symbol prefix "21") m21)
                    (,(%make-accessor-symbol prefix "22") m22))
       ,matrix
     ,(if rest
          `(with-components ,rest ,@body)
          `(progn ,@body))))

(declaim (inline mref))
(defun* (mref -> single-float) ((matrix matrix) (row (integer 0 8)) (column (integer 0 8)))
  (aref matrix (+ row (cl:* column 3))))

(defun* (setf mref) ((value single-float) (matrix matrix) (row (integer 0 8)) (column (integer 0 8)))
  (:returns single-float)
  (setf (aref matrix (+ row (cl:* column 3))) value))

;;; Constants

(alexandria:define-constant +zero+
    (make-array 9 :element-type 'single-float
                  :initial-contents '(0.0f0 0.0f0 0.0f0
                                      0.0f0 0.0f0 0.0f0
                                      0.0f0 0.0f0 0.0f0))
  :test #'equalp)

(alexandria:define-constant +id+
    (make-array 9 :element-type 'single-float
                  :initial-contents '(1.0f0 0.0f0 0.0f0
                                      0.0f0 1.0f0 0.0f0
                                      0.0f0 0.0f0 1.0f0))
  :test #'equalp)

;;; Operations

(declaim (inline make))
(defun* (make -> matrix) ((m00 real) (m01 real) (m02 real) (m10 real) (m11 real) (m12 real)
                          (m20 real) (m21 real) (m22 real))
  (%make (float m00 1.0f0) (float m01 1.0f0) (float m02 1.0f0) (float m10 1.0f0) (float m11 1.0f0)
         (float m12 1.0f0) (float m20 1.0f0) (float m21 1.0f0) (float m22 1.0f0)))

(declaim (inline zero!))
(defun* (zero! -> matrix) ((matrix matrix))
  (with-components ((m matrix))
    (psetf m00 0.0f0 m01 0.0f0 m02 0.0f0
           m10 0.0f0 m11 0.0f0 m12 0.0f0
           m20 0.0f0 m21 0.0f0 m22 0.0f0))
  matrix)

(declaim (inline zero))
(defun* (zero -> matrix) ()
  (%make 0.0f0 0.0f0 0.0f0
         0.0f0 0.0f0 0.0f0
         0.0f0 0.0f0 0.0f0))

(declaim (inline id!))
(defun* (id! -> matrix) ((matrix matrix))
  (with-components ((m matrix))
    (psetf m00 1.0f0 m01 0.0f0 m02 0.0f0
           m10 0.0f0 m11 1.0f0 m12 0.0f0
           m20 0.0f0 m21 0.0f0 m22 1.0f0))
  matrix)

(declaim (inline id))
(defun* (id -> matrix) ()
  (%make 1.0f0 0.0f0 0.0f0
         0.0f0 1.0f0 0.0f0
         0.0f0 0.0f0 1.0f0))

(declaim (inline =))
(defun* (= -> boolean) ((matrix1 matrix) (matrix2 matrix))
  (with-components ((a matrix1) (b matrix2))
    (and (cl:= a00 b00) (cl:= a01 b01) (cl:= a02 b02)
         (cl:= a10 b10) (cl:= a11 b11) (cl:= a12 b12)
         (cl:= a20 b20) (cl:= a21 b21) (cl:= a22 b22))))

(declaim (inline ~))
(defun* (~ -> boolean) ((matrix1 matrix) (matrix2 matrix) &key ((tolerance single-float) +epsilon+))
  (with-components ((a matrix1) (b matrix2))
    (and (%~ a00 b00 tolerance) (%~ a01 b01 tolerance) (%~ a02 b02 tolerance)
         (%~ a10 b10 tolerance) (%~ a11 b11 tolerance) (%~ a12 b12 tolerance)
         (%~ a20 b20 tolerance) (%~ a21 b21 tolerance) (%~ a22 b22 tolerance))))

(declaim (inline copy!))
(defun* (copy! -> matrix) ((out matrix) (matrix matrix))
  (with-components ((o out) (m matrix))
    (psetf o00 m00 o01 m01 o02 m02
           o10 m10 o11 m11 o12 m12
           o20 m20 o21 m21 o22 m22))
  out)

(declaim (inline copy))
(defun* (copy -> matrix) ((matrix matrix))
  (copy! (zero) matrix))

(defun* (clamp! -> matrix) ((out matrix) (matrix matrix) &key
                            ((min single-float) most-negative-single-float)
                            ((max single-float) most-positive-single-float))
  (with-components ((o out) (m matrix))
    (psetf o00 (alexandria:clamp m00 min max)
           o01 (alexandria:clamp m01 min max)
           o02 (alexandria:clamp m02 min max)
           o10 (alexandria:clamp m10 min max)
           o11 (alexandria:clamp m11 min max)
           o12 (alexandria:clamp m12 min max)
           o20 (alexandria:clamp m20 min max)
           o21 (alexandria:clamp m21 min max)
           o22 (alexandria:clamp m22 min max)))
  out)

(declaim (inline clamp))
(defun* (clamp -> matrix) ((matrix matrix) &key
                           ((min single-float) most-negative-single-float)
                           ((max single-float) most-positive-single-float))
  (clamp! (zero) matrix :min min :max max))

(declaim (inline *!))
(defun* (*! -> matrix) ((out matrix) (matrix1 matrix) (matrix2 matrix))
  (with-components ((o out) (a matrix1) (b matrix2))
    (psetf o00 (+ (cl:* a00 b00) (cl:* a01 b10) (cl:* a02 b20))
           o10 (+ (cl:* a10 b00) (cl:* a11 b10) (cl:* a12 b20))
           o20 (+ (cl:* a20 b00) (cl:* a21 b10) (cl:* a22 b20))
           o01 (+ (cl:* a00 b01) (cl:* a01 b11) (cl:* a02 b21))
           o11 (+ (cl:* a10 b01) (cl:* a11 b11) (cl:* a12 b21))
           o21 (+ (cl:* a20 b01) (cl:* a21 b11) (cl:* a22 b21))
           o02 (+ (cl:* a00 b02) (cl:* a01 b12) (cl:* a02 b22))
           o12 (+ (cl:* a10 b02) (cl:* a11 b12) (cl:* a12 b22))
           o22 (+ (cl:* a20 b02) (cl:* a21 b12) (cl:* a22 b22))))
  out)

(declaim (inline *))
(defun* (* -> matrix) ((matrix1 matrix) (matrix2 matrix))
  (*! (zero) matrix1 matrix2))

(declaim (inline translation-to-vec2!))
(defun* (translation-to-vec2! -> v2:vec) ((out v2:vec) (matrix matrix))
  (v2:with-components ((o out))
    (with-components ((m matrix))
      (psetf ox m02 oy m12)))
  out)

(declaim (inline translation-to-vec2))
(defun* (translation-to-vec2 -> v2:vec) ((matrix matrix))
  (translation-to-vec2! (v2:zero) matrix))

(declaim (inline translation-from-vec2!))
(defun* (translation-from-vec2! -> matrix) ((matrix matrix) (vec v2:vec))
  (with-components ((m matrix))
    (v2:with-components ((v vec))
      (psetf m02 vx m12 vy)))
  matrix)

(declaim (inline translation-from-vec2))
(defun* (translation-from-vec2 -> matrix) ((matrix matrix) (vec v2:vec))
  (translation-from-vec2! (copy matrix) vec))

(declaim (inline translate!))
(defun* (translate! -> matrix) ((out matrix) (matrix matrix) (vec v2:vec))
  (*! out (translation-from-vec2 (id) vec) matrix))

(declaim (inline translate))
(defun* (translate -> matrix) ((matrix matrix) (vec v2:vec))
  (translate! (id) matrix vec))

(declaim (inline copy-rotation!))
(defun* (copy-rotation! -> matrix) ((out matrix) (matrix matrix))
  (with-components ((o out) (m matrix))
    (psetf o00 m00 o01 m01
           o10 m10 o11 m11))
  out)

(declaim (inline copy-rotation))
(defun* (copy-rotation -> matrix) ((matrix matrix))
  (copy-rotation! (id) matrix))

(declaim (inline rotation-axis-to-vec2!))
(defun* (rotation-axis-to-vec2! -> v2:vec) ((out v2:vec) (matrix matrix) (axis keyword))
  (v2:with-components ((v out))
    (with-components ((m matrix))
      (ecase axis
        (:x (psetf vx m00 vy m10))
        (:y (psetf vx m01 vy m11)))))
  out)

(declaim (inline rotation-axis-to-vec2))
(defun* (rotation-axis-to-vec2 -> v2:vec) ((matrix matrix) (axis keyword))
  (rotation-axis-to-vec2! (v2:zero) matrix axis))

(declaim (inline rotation-axis-from-vec2!))
(defun* (rotation-axis-from-vec2! -> matrix) ((matrix matrix) (vec v2:vec) (axis keyword))
  (with-components ((m matrix))
    (v2:with-components ((v vec))
      (ecase axis
        (:x (psetf m00 vx m10 vy))
        (:y (psetf m01 vx m11 vy)))))
  matrix)

(declaim (inline rotation-axis-from-vec2))
(defun* (rotation-axis-from-vec2 -> matrix) ((matrix matrix) (vec v2:vec) (axis keyword))
  (rotation-axis-from-vec2! (copy matrix) vec axis))

(defun* (rotate! -> matrix) ((out matrix) (matrix matrix) (angle float))
  (with-components ((m (id)))
    (copy! out matrix)
    (when (> (abs angle) +epsilon+)
      (let* ((angle (float angle 1.0f0))
             (s (sin angle))
             (c (cos angle)))
        (psetf m00 c m01 (- s)
               m10 s m11 c)
        (*! out out m))))
  out)

(declaim (inline rotate))
(defun* (rotate -> matrix) ((matrix matrix) (angle float))
  (rotate! (id) matrix angle))

(declaim (inline scale-to-vec2!))
(defun* (scale-to-vec2! -> v2:vec) ((out v2:vec) (matrix matrix))
  (v2:with-components ((o out))
    (with-components ((m matrix))
      (psetf ox m00 oy m11)))
  out)

(declaim (inline scale-to-vec2))
(defun* (scale-to-vec2 -> v2:vec) ((matrix matrix))
  (scale-to-vec2! (v2:zero) matrix))

(declaim (inline scale-from-vec2!))
(defun* (scale-from-vec2! -> matrix) ((matrix matrix) (vec v2:vec))
  (with-components ((m matrix))
    (v2:with-components ((v vec))
      (psetf m00 vx m11 vy)))
  matrix)

(declaim (inline scale-from-vec2))
(defun* (scale-from-vec2 -> matrix) ((matrix matrix) (vec v2:vec))
  (scale-from-vec2! (copy matrix) vec))

(declaim (inline scale!))
(defun* (scale! -> matrix) ((out matrix) (matrix matrix) (vec v2:vec))
  (*! out (scale-from-vec2 (id) vec) matrix))

(declaim (inline scale))
(defun* (scale -> matrix) ((matrix matrix) (vec v2:vec))
  (scale! (id) matrix vec))

(declaim (inline *v3!))
(defun* (*v3! -> v3:vec) ((out v3:vec) (matrix matrix) (vec v3:vec))
  (v3:with-components ((v vec) (o out))
    (with-components ((m matrix))
      (psetf ox (+ (cl:* m00 vx) (cl:* m01 vy) (cl:* m02 vz))
             oy (+ (cl:* m10 vx) (cl:* m11 vy) (cl:* m12 vz))
             oz (+ (cl:* m20 vx) (cl:* m21 vy) (cl:* m22 vz)))))
  out)

(declaim (inline *v3))
(defun* (*v3 -> v3:vec) ((matrix matrix) (vec v3:vec))
  (*v3! (v3:zero) matrix vec))

(declaim (inline transpose!))
(defun* (transpose! -> matrix) ((out matrix) (matrix matrix))
  (with-components ((o (copy! out matrix)))
    (rotatef o01 o10)
    (rotatef o02 o20)
    (rotatef o12 o21))
  out)

(declaim (inline transpose))
(defun* (transpose -> matrix) ((matrix matrix))
  (transpose! (id) matrix))

(defun* (orthogonalp -> boolean) ((matrix matrix))
  (~ (* matrix (transpose matrix)) +id+))

(declaim (inline trace))
(defun* (trace -> single-float) ((matrix matrix))
  (with-components ((m matrix))
    (+ m00 m11 m22)))

(declaim (inline diagonalp))
(defun* (diagonalp -> boolean) ((matrix matrix))
  (with-components ((m matrix))
    (and (zerop m10)
         (zerop m20)
         (zerop m01)
         (zerop m21)
         (zerop m02)
         (zerop m12))))

(declaim (inline main-diagonal!))
(defun* (main-diagonal! -> v3:vec) ((out v3:vec) (matrix matrix))
  (with-components ((m matrix))
    (v3:with-components ((v out))
      (setf vx m00 vy m11 vz m22)))
  out)

(declaim (inline main-diagonal))
(defun* (main-diagonal -> v3:vec) ((matrix matrix))
  (main-diagonal! (v3:zero) matrix))

(declaim (inline anti-diagonal!))
(defun* (anti-diagonal! -> v3:vec) ((out v3:vec) (matrix matrix))
  (with-components ((m matrix))
    (v3:with-components ((v out))
      (setf vx m02 vy m11 vz m20)))
  out)

(declaim (inline anti-diagonal))
(defun* (anti-diagonal -> v3:vec) ((matrix matrix))
  (anti-diagonal! (v3:zero) matrix))
