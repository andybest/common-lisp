(in-package :box.math.mat2)

;;; Structure

(deftype matrix () '(simple-array single-float (4)))

(defstruct (matrix (:type (vector single-float))
                   (:constructor %make (m00 m01
                                        m10 m11))
                   (:conc-name nil)
                   (:copier nil))
  (m00 0.0f0 :type single-float)
  (m10 0.0f0 :type single-float)
  (m01 0.0f0 :type single-float)
  (m11 0.0f0 :type single-float))

(defmacro with-components (((prefix matrix) &rest rest) &body body)
  `(with-accessors ((,prefix identity)
                    (,(%make-accessor-symbol prefix "00") m00)
                    (,(%make-accessor-symbol prefix "01") m01)
                    (,(%make-accessor-symbol prefix "10") m10)
                    (,(%make-accessor-symbol prefix "11") m11))
       ,matrix
     ,(if rest
          `(with-components ,rest ,@body)
          `(progn ,@body))))

(declaim (inline mref))
(declaim (ftype (function (matrix (integer 0 3) (integer 0 3)) single-float) mref))
(defun mref (matrix row column)
  (aref matrix (+ row (cl:* column 2))))

(declaim (inline (setf mref)))
(declaim (ftype (function (single-float matrix (integer 0 3) (integer 0 3)) single-float)
                (setf mref)))
(defun (setf mref) (value matrix row column)
  (setf (aref matrix (+ row (cl:* column 2))) value))

;;; Constants

(alexandria:define-constant +zero+
    (make-array 4 :element-type 'single-float
                  :initial-contents '(0.0f0 0.0f0
                                      0.0f0 0.0f0))
  :test #'equalp)

(alexandria:define-constant +id+
    (make-array 4 :element-type 'single-float
                  :initial-contents '(1.0f0 0.0f0
                                      0.0f0 1.0f0))
  :test #'equalp)

;;; Operations

(declaim (inline make))
(declaim (ftype (function (real real real real) matrix) make))
(defun make (m00 m01 m10 m11)
  (%make (float m00 1.0f0) (float m01 1.0f0)
         (float m10 1.0f0) (float m11 1.0f0)))

(declaim (inline zero!))
(declaim (ftype (function (matrix) matrix) zero!))
(defun zero! (matrix)
  (with-components ((m matrix))
    (psetf m00 0.0f0 m01 0.0f0
           m10 0.0f0 m11 0.0f0))
  matrix)

(declaim (inline zero))
(declaim (ftype (function () matrix) zero))
(defun zero ()
  (%make 0.0f0 0.0f0
         0.0f0 0.0f0))

(declaim (inline id!))
(declaim (ftype (function (matrix) matrix) id!))
(defun id! (matrix)
  (with-components ((m matrix))
    (psetf m00 1.0f0 m01 0.0f0
           m10 0.0f0 m11 1.0f0))
  matrix)

(declaim (inline id))
(declaim (ftype (function () matrix) id))
(defun id ()
  (%make 1.0f0 0.0f0
         0.0f0 1.0f0))

(declaim (inline =))
(declaim (ftype (function (matrix matrix) boolean) =))
(defun = (matrix1 matrix2)
  (with-components ((a matrix1) (b matrix2))
    (and (cl:= a00 b00) (cl:= a01 b01)
         (cl:= a10 b10) (cl:= a11 b11))))

(declaim (inline ~))
(declaim (ftype (function (matrix matrix &key (:tolerance single-float)) boolean) ~))
(defun ~ (matrix1 matrix2 &key (tolerance +epsilon+))
  (with-components ((a matrix1) (b matrix2))
    (and (%~ a00 b00 tolerance) (%~ a01 b01 tolerance)
         (%~ a10 b10 tolerance) (%~ a11 b11 tolerance))))

(declaim (inline copy!))
(declaim (ftype (function (matrix matrix) matrix) copy!))
(defun copy! (out matrix)
  (with-components ((o out) (m matrix))
    (psetf o00 m00 o01 m01
           o10 m10 o11 m11))
  out)

(declaim (inline copy))
(declaim (ftype (function (matrix) matrix) copy))
(defun copy (matrix)
  (copy! (zero) matrix))

(declaim (ftype (function (matrix matrix &key (:min single-float) (:max single-float)) matrix)
                clamp!))
(defun clamp! (out matrix &key (min most-negative-single-float) (max most-positive-single-float))
  (with-components ((o out) (m matrix))
    (psetf o00 (alexandria:clamp m00 min max)
           o01 (alexandria:clamp m01 min max)
           o10 (alexandria:clamp m10 min max)
           o11 (alexandria:clamp m11 min max)))
  out)

(declaim (inline clamp))
(declaim (ftype (function (matrix &key (:min single-float) (:max single-float)) matrix) clamp))
(defun clamp (matrix &key (min most-negative-single-float) (max most-positive-single-float))
  (clamp! (zero) matrix :min min :max max))

(declaim (inline *!))
(declaim (ftype (function (matrix matrix matrix) matrix) *!))
(defun *! (out matrix1 matrix2)
  (with-components ((o out) (a matrix1) (b matrix2))
    (psetf o00 (+ (cl:* a00 b00) (cl:* a01 b10))
           o10 (+ (cl:* a10 b00) (cl:* a11 b10))
           o01 (+ (cl:* a00 b01) (cl:* a01 b11))
           o11 (+ (cl:* a10 b01) (cl:* a11 b11))))
  out)

(declaim (inline *))
(declaim (ftype (function (matrix matrix) matrix) *))
(defun * (matrix1 matrix2)
  (*! (zero) matrix1 matrix2))

(declaim (inline rotation-axis-to-vec2!))
(declaim (ftype (function (v2:vec matrix keyword) v2:vec) rotation-axis-to-vec2!))
(defun rotation-axis-to-vec2! (out matrix axis)
  (v2:with-components ((v out))
    (with-components ((m matrix))
      (ecase axis
        (:x (psetf vx m00 vy m10))
        (:y (psetf vx m01 vy m11)))))
  out)

(declaim (inline rotation-axis-to-vec2))
(declaim (ftype (function (matrix keyword) v2:vec) rotation-axis-to-vec2))
(defun rotation-axis-to-vec2 (matrix axis)
  (rotation-axis-to-vec2! (v2:zero) matrix axis))

(declaim (inline rotation-axis-from-vec2!))
(declaim (ftype (function (matrix v2:vec keyword) matrix) rotation-axis-from-vec2!))
(defun rotation-axis-from-vec2! (matrix vec axis)
  (with-components ((m matrix))
    (v2:with-components ((v vec))
      (ecase axis
        (:x (psetf m00 vx m10 vy))
        (:y (psetf m01 vx m11 vy)))))
  matrix)

(declaim (inline rotation-axis-from-vec2))
(declaim (ftype (function (matrix v2:vec keyword) matrix) rotation-axis-from-vec2))
(defun rotation-axis-from-vec2 (matrix vec axis)
  (rotation-axis-from-vec2! (copy matrix) vec axis))

(declaim (ftype (function (matrix matrix float) matrix) rotate!))
(defun rotate! (out matrix angle)
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
(declaim (ftype (function (matrix float) matrix) rotate))
(defun rotate (matrix angle)
  (rotate! (id) matrix angle))

(declaim (inline scale-to-vec2!))
(declaim (ftype (function (v2:vec matrix) v2:vec) scale-to-vec2!))
(defun scale-to-vec2! (out matrix)
  (v2:with-components ((o out))
    (with-components ((m matrix))
      (psetf ox m00 oy m11)))
  out)

(declaim (inline scale-to-vec2))
(declaim (ftype (function (matrix) v2:vec) scale-to-vec2))
(defun scale-to-vec2 (matrix)
  (scale-to-vec2! (v2:zero) matrix))

(declaim (inline scale-from-vec2!))
(declaim (ftype (function (matrix v2:vec) matrix) scale-from-vec2!))
(defun scale-from-vec2! (matrix vec)
  (with-components ((m matrix))
    (v2:with-components ((v vec))
      (psetf m00 vx m11 vy)))
  matrix)

(declaim (inline scale-from-vec2))
(declaim (ftype (function (matrix v2:vec) matrix) scale-from-vec2))
(defun scale-from-vec2 (matrix vec)
  (scale-from-vec2! (copy matrix) vec))

(declaim (inline scale!))
(declaim (ftype (function (matrix matrix v2:vec) matrix) scale!))
(defun scale! (out matrix vec)
  (*! out (scale-from-vec2 (id) vec) matrix))

(declaim (inline scale))
(declaim (ftype (function (matrix v2:vec) matrix) scale))
(defun scale (matrix vec)
  (scale! (id) matrix vec))

(declaim (inline *v2!))
(declaim (ftype (function (v2:vec matrix v2:vec) v2:vec) *v2!))
(defun *v2! (out matrix vec)
  (v2:with-components ((v vec) (o out))
    (with-components ((m matrix))
      (psetf ox (+ (cl:* m00 vx) (cl:* m01 vy))
             oy (+ (cl:* m10 vx) (cl:* m11 vy)))))
  out)

(declaim (inline *v2))
(declaim (ftype (function (matrix v2:vec) v2:vec) *v2))
(defun *v2 (matrix vec)
  (*v2! (v2:zero) matrix vec))

(declaim (inline transpose!))
(declaim (ftype (function (matrix matrix) matrix) transpose!))
(defun transpose! (out matrix)
  (with-components ((o (copy! out matrix)))
    (rotatef o01 o10))
  out)

(declaim (inline transpose))
(declaim (ftype (function (matrix) matrix) transpose))
(defun transpose (matrix)
  (transpose! (id) matrix))

(declaim (ftype (function (matrix) boolean) orthogonalp))
(defun orthogonalp (matrix)
  (~ (* matrix (transpose matrix)) +id+))

(declaim (inline trace))
(declaim (ftype (function (matrix) single-float) trace))
(defun trace (matrix)
  (with-components ((m matrix))
    (+ m00 m11)))

(declaim (inline diagonalp))
(declaim (ftype (function (matrix) boolean) diagonalp))
(defun diagonalp (matrix)
  (with-components ((m matrix))
    (and (zerop m10)
         (zerop m01))))

(declaim (inline main-diagonal!))
(declaim (ftype (function (v2:vec matrix) v2:vec) main-diagonal!))
(defun main-diagonal! (out matrix)
  (with-components ((m matrix))
    (v2:with-components ((v out))
      (setf vx m00 vy m11)))
  out)

(declaim (inline main-diagonal))
(declaim (ftype (function (matrix) v2:vec) main-diagonal))
(defun main-diagonal (matrix)
  (main-diagonal! (v2:zero) matrix))

(declaim (inline anti-diagonal!))
(declaim (ftype (function (v2:vec matrix) v2:vec) anti-diagonal!))
(defun anti-diagonal! (out matrix)
  (with-components ((m matrix))
    (v2:with-components ((v out))
      (setf vx m01 vy m10)))
  out)

(declaim (inline anti-diagonal))
(declaim (ftype (function (matrix) v2:vec) anti-diagonal))
(defun anti-diagonal (matrix)
  (anti-diagonal! (v2:zero) matrix))
