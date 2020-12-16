(in-package #:net.mfiano.lisp.origin.mat2)

;;; constructors

(u:fn-> %mat (&rest u:f32) mat)
(u:eval-always
  (u:defun-inline %mat (&rest args)
    (declare (optimize speed))
    (make-array 4 :element-type 'single-float :initial-contents args)))

(ss:defstore mat (&rest args))

(ss:defspecialization (mat :inline t) () mat
  (%mat 0f0 0f0 0f0 0f0))

(ss:defspecialization (mat :inline t) ((x real)) mat
  (%mat (float x 1f0) 0f0 0f0 (float x 1f0)))

(ss:defspecialization (mat :inline t) ((mat mat)) mat
  (with-components ((m mat))
    (%mat m00 m10 m01 m11)))

(ss:defspecialization (mat :inline t) ((mat net.mfiano.lisp.origin.mat3:mat))
    mat
  (net.mfiano.lisp.origin.mat3:with-components ((m mat))
    (%mat m00 m01 m10 m11)))

(ss:defspecialization (mat :inline t) ((mat net.mfiano.lisp.origin.mat4:mat))
    mat
  (net.mfiano.lisp.origin.mat4:with-components ((m mat))
    (%mat m00 m01 m10 m11)))

(ss:defspecialization (mat :inline t) ((a v2:vec) (b v2:vec)) mat
  (v2:with-components ((a a) (b b))
    (%mat ax ay bx by)))

(ss:defspecialization (mat :inline t) ((a real) (b real) (c real) (d real)) mat
  (%mat (float a 1f0) (float b 1f0) (float c 1f0) (float d 1f0)))

(ss:defspecialization (mat :inline t) ((mat net.mfiano.lisp.origin.dmat2:mat))
    mat
  (net.mfiano.lisp.origin.dmat2:with-components ((m mat))
    (%mat (float m00 1f0) (float m10 1f0) (float m01 1f0) (float m11 1f0))))

;;; constants

(u:define-constant +zero+ (%mat 0f0 0f0 0f0 0f0) :test #'equalp)

(u:define-constant +id+ (%mat 1f0 0f0 0f0 1f0) :test #'equalp)

;;; operators

(u:fn-> = (mat mat &key (:rel u:f32) (:abs u:f32)) boolean)
(u:defun-inline = (mat1 mat2 &key (rel 1e-7) (abs rel))
  (with-components ((a mat1) (b mat2))
    (and (int:= a00 b00 rel abs)
         (int:= a01 b01 rel abs)
         (int:= a10 b10 rel abs)
         (int:= a11 b11 rel abs))))

(u:fn-> zero! (mat) mat)
(u:defun-inline zero! (mat)
  (declare (optimize speed))
  (with-components ((m mat))
    (psetf m00 0f0 m01 0f0
           m10 0f0 m11 0f0))
  mat)

(u:fn-> zero-p (mat) boolean)
(u:defun-inline zero-p (mat)
  (declare (optimize speed))
  (= mat +zero+))

(u:fn-> id! (mat) mat)
(u:defun-inline id! (mat)
  (declare (optimize speed))
  (with-components ((m mat))
    (psetf m00 1f0 m01 0f0
           m10 0f0 m11 1f0))
  mat)

(u:fn-> id () mat)
(u:defun-inline id ()
  (declare (optimize speed))
  (id! (mat)))

(u:fn-> id-p (mat) boolean)
(u:defun-inline id-p (mat)
  (declare (optimize speed))
  (= mat +id+))

(u:fn-> random! (mat u:f32 u:f32) mat)
(u:defun-inline random! (out min max)
  (declare (optimize speed))
  (let ((diff (cl:- max min)))
    (with-components ((o out))
      (psetf o00 (cl:+ min (cl:random diff))
             o01 (cl:+ min (cl:random diff))
             o10 (cl:+ min (cl:random diff))
             o11 (cl:+ min (cl:random diff)))))
  out)

(u:fn-> random (u:f32 u:f32) mat)
(u:defun-inline random (min max)
  (declare (optimize speed))
  (random! (mat) min max))

(u:fn-> copy! (mat mat) mat)
(u:defun-inline copy! (out mat)
  (declare (optimize speed))
  (with-components ((o out) (m mat))
    (psetf o00 m00 o01 m01
           o10 m10 o11 m11))
  out)

(u:fn-> copy (mat) mat)
(u:defun-inline copy (mat)
  (declare (optimize speed))
  (copy! (mat) mat))

(u:fn-> clamp! (mat mat u:f32 u:f32) mat)
(u:defun-inline clamp! (out mat min max)
  (declare (optimize speed))
  (with-components ((o out) (m mat))
    (psetf o00 (u:clamp m00 min max)
           o01 (u:clamp m01 min max)
           o10 (u:clamp m10 min max)
           o11 (u:clamp m11 min max)))
  out)

(u:fn-> clamp (mat u:f32 u:f32) mat)
(u:defun-inline clamp (mat min max)
  (declare (optimize speed))
  (clamp! (mat) mat min max))

(u:fn-> +! (mat mat mat) mat)
(u:defun-inline +! (out mat1 mat2)
  (declare (optimize speed))
  (with-components ((o out) (a mat1) (b mat2))
    (psetf o00 (cl:+ a00 b00)
           o10 (cl:+ a10 b10)
           o01 (cl:+ a01 b01)
           o11 (cl:+ a11 b11)))
  out)

(u:fn-> + (mat mat) mat)
(u:defun-inline + (mat1 mat2)
  (declare (optimize speed))
  (+! (mat) mat1 mat2))

(u:fn-> -! (mat mat mat) mat)
(u:defun-inline -! (out mat1 mat2)
  (declare (optimize speed))
  (with-components ((o out) (a mat1) (b mat2))
    (psetf o00 (cl:- a00 b00)
           o10 (cl:- a10 b10)
           o01 (cl:- a01 b01)
           o11 (cl:- a11 b11)))
  out)

(u:fn-> - (mat mat) mat)
(u:defun-inline - (mat1 mat2)
  (declare (optimize speed))
  (-! (mat) mat1 mat2))

(defmacro %* (o00 o01 o10 o11 a00 a01 a10 a11 b00 b01 b10 b11)
  `(psetf ,o00 (cl:+ (cl:* ,a00 ,b00) (cl:* ,a01 ,b10))
          ,o10 (cl:+ (cl:* ,a10 ,b00) (cl:* ,a11 ,b10))
          ,o01 (cl:+ (cl:* ,a00 ,b01) (cl:* ,a01 ,b11))
          ,o11 (cl:+ (cl:* ,a10 ,b01) (cl:* ,a11 ,b11))))

(u:fn-> *! (mat mat mat) mat)
(u:defun-inline *! (out mat1 mat2)
  (declare (optimize speed))
  (with-components ((o out) (a mat1) (b mat2))
    (%* o00 o01 o10 o11 a00 a01 a10 a11 b00 b01 b10 b11))
  out)

(u:fn-> * (mat mat) mat)
(u:defun-inline * (mat1 mat2)
  (declare (optimize speed))
  (*! (mat) mat1 mat2))

(u:fn-> get-column! (v2:vec mat (integer 0 1)) v2:vec)
(u:defun-inline get-column! (out mat index)
  (declare (optimize speed))
  (with-components ((m mat))
    (v2:with-components ((o out))
      (ecase index
        (0 (psetf ox m00 oy m10))
        (1 (psetf ox m01 oy m11)))))
  out)

(u:fn-> get-column (mat (integer 0 1)) v2:vec)
(u:defun-inline get-column (mat index)
  (declare (optimize speed))
  (get-column! (v2:vec) mat index))

(u:fn-> set-column! (mat mat v2:vec (integer 0 1)) mat)
(u:defun-inline set-column! (out mat vec index)
  (declare (optimize speed))
  (with-components ((o out))
    (v2:with-components ((v vec))
      (copy! out mat)
      (ecase index
        (0 (psetf o00 vx o10 vy))
        (1 (psetf o01 vx o11 vy)))))
  out)

(u:fn-> set-column (mat v2:vec (integer 0 1)) mat)
(u:defun-inline set-column (mat vec index)
  (declare (optimize speed))
  (set-column! (mat 1) mat vec index))

(u:fn-> rotation-axis-to-vec2! (v2:vec mat keyword) v2:vec)
(u:defun-inline rotation-axis-to-vec2! (out mat axis)
  (declare (optimize speed))
  (v2:with-components ((v out))
    (with-components ((m mat))
      (ecase axis
        (:x (psetf vx m00 vy m10))
        (:y (psetf vx m01 vy m11)))))
  out)

(u:fn-> rotation-axis-to-vec2 (mat keyword) v2:vec)
(u:defun-inline rotation-axis-to-vec2 (mat axis)
  (declare (optimize speed))
  (rotation-axis-to-vec2! (v2:vec) mat axis))

(u:fn-> rotation-axis-from-vec2! (mat v2:vec keyword) mat)
(u:defun-inline rotation-axis-from-vec2! (out vec axis)
  (declare (optimize speed))
  (with-components ((o out))
    (v2:with-components ((v vec))
      (ecase axis
        (:x (psetf o00 vx o10 vy))
        (:y (psetf o01 vx o11 vy)))))
  out)

(u:fn-> rotation-axis-from-vec2 (mat v2:vec keyword) mat)
(u:defun-inline rotation-axis-from-vec2 (mat vec axis)
  (declare (optimize speed))
  (rotation-axis-from-vec2! (copy mat) vec axis))

(u:fn-> rotate! (mat mat u:f32 &key (:space keyword)) mat)
(u:defun-inline rotate! (out mat angle &key (space :local))
  (with-components ((m (mat 1)))
    (copy! out mat)
    (when (cl:> (abs angle) 1e-7)
      (let ((s (sin angle))
            (c (cos angle)))
        (psetf m00 c m01 (cl:- s)
               m10 s m11 c)
        (ecase space
          (:local (*! out out m))
          (:world (*! out m out))))))
  out)

(u:fn-> rotate (mat u:f32) mat)
(u:defun-inline rotate (mat vec)
  (declare (optimize speed))
  (rotate! (mat 1) mat vec))

(u:fn-> get-scale! (v2:vec mat) v2:vec)
(u:defun-inline get-scale! (out mat)
  (declare (optimize speed))
  (v2:with-components ((o out))
    (psetf ox (v2:length (rotation-axis-to-vec2 mat :x))
           oy (v2:length (rotation-axis-to-vec2 mat :y))))
  out)

(u:fn-> get-scale (mat) v2:vec)
(u:defun-inline get-scale (mat)
  (declare (optimize speed))
  (get-scale! (v2:vec) mat))

(u:fn-> set-scale! (mat mat v2:vec) mat)
(u:defun-inline set-scale! (out mat vec)
  (declare (optimize speed))
  (with-components ((o out))
    (v2:with-components ((v vec))
      (copy! out mat)
      (psetf o00 vx o11 vy)))
  out)

(u:fn-> set-scale (mat v2:vec) mat)
(u:defun-inline set-scale (mat vec)
  (declare (optimize speed))
  (set-scale! (copy mat) mat vec))

(u:fn-> scale! (mat mat v2:vec) mat)
(u:defun-inline scale! (out mat vec)
  (declare (optimize speed))
  (*! out (set-scale (mat 1) vec) mat))

(u:fn-> scale (mat v2:vec) mat)
(u:defun-inline scale (mat vec)
  (declare (optimize speed))
  (scale! (mat 1) mat vec))

(u:fn-> *v2! (v2:vec mat v2:vec) v2:vec)
(u:defun-inline *v2! (out mat vec)
  (declare (optimize speed))
  (v2:with-components ((v vec) (o out))
    (with-components ((m mat))
      (psetf ox (cl:+ (cl:* m00 vx) (cl:* m01 vy))
             oy (cl:+ (cl:* m10 vx) (cl:* m11 vy)))))
  out)

(u:fn-> *v2 (mat v2:vec) v2:vec)
(u:defun-inline *v2 (mat vec)
  (declare (optimize speed))
  (*v2! (v2:vec) mat vec))

(u:fn-> transpose! (mat mat) mat)
(u:defun-inline transpose! (out mat)
  (declare (optimize speed))
  (with-components ((o (copy! out mat)))
    (rotatef o01 o10))
  out)

(u:fn-> transpose (mat) mat)
(u:defun-inline transpose (mat)
  (declare (optimize speed))
  (transpose! (mat 1) mat))

(u:fn-> orthogonal-p (mat) boolean)
(u:defun-inline orthogonal-p (mat)
  (declare (optimize speed))
  (= (* mat (transpose mat)) +id+))

(u:fn-> trace (mat) u:f32)
(u:defun-inline trace (mat)
  (with-components ((m mat))
    (cl:+ m00 m11)))

(u:fn-> diagonal-p (mat) boolean)
(u:defun-inline diagonal-p (mat)
  (declare (optimize speed))
  (with-components ((m mat))
    (cl:= 0f0 m10 m01)))

(u:fn-> main-diagonal! (v2:vec mat) v2:vec)
(u:defun-inline main-diagonal! (out mat)
  (declare (optimize speed))
  (with-components ((m mat))
    (v2:with-components ((v out))
      (psetf vx m00 vy m11)))
  out)

(u:fn-> main-diagonal (mat) v2:vec)
(u:defun-inline main-diagonal (mat)
  (declare (optimize speed))
  (main-diagonal! (v2:vec) mat))

(u:fn-> anti-diagonal! (v2:vec mat) v2:vec)
(u:defun-inline anti-diagonal! (out mat)
  (declare (optimize speed))
  (with-components ((m mat))
    (v2:with-components ((v out))
      (psetf vx m01 vy m10)))
  out)

(u:fn-> anti-diagonal (mat) v2:vec)
(u:defun-inline anti-diagonal (mat)
  (declare (optimize speed))
  (anti-diagonal! (v2:vec) mat))
