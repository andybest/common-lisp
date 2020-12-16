(in-package #:net.mfiano.lisp.origin.dmat3)

;;; constructors

(u:fn-> %mat (&rest u:f64) mat)
(u:eval-always
  (u:defun-inline %mat (&rest args)
    (declare (optimize speed))
    (make-array 9 :element-type 'double-float :initial-contents args)))

(ss:defstore mat (&rest args))

(ss:defspecialization (mat :inline t) () mat
  (%mat 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0))

(ss:defspecialization (mat :inline t) ((x real)) mat
  (%mat (float x 1d0) 0d0 0d0
        0d0 (float x 1d0) 0d0
        0d0 0d0 (float x 1d0)))

(ss:defspecialization (mat :inline t) ((mat dm2:mat)) mat
  (dm2:with-components ((m mat))
    (%mat m00 m10 0d0 m01 m11 0d0 0d0 0d0 1d0)))

(ss:defspecialization (mat :inline t) ((mat mat)) mat
  (with-components ((m mat))
    (%mat m00 m10 m20 m01 m11 m21 m02 m12 m22)))

(ss:defspecialization (mat :inline t) ((mat net.mfiano.lisp.origin.dmat4:mat))
    mat
  (net.mfiano.lisp.origin.dmat4:with-components ((m mat))
    (%mat m00 m10 m20 m01 m11 m21 m02 m12 m22)))

(ss:defspecialization (mat :inline t) ((a dv3:vec) (b dv3:vec) (c dv3:vec)) mat
  (dv3:with-components ((a a) (b b) (c c))
    (%mat ax ay az bx by bz cx cy cz)))

(ss:defspecialization (mat :inline t) ((a real) (b real) (c real)
                                       (d real) (e real) (f real)
                                       (g real) (h real) (i real))
    mat
  (%mat (float a 1d0) (float b 1d0) (float c 1d0)
        (float d 1d0) (float e 1d0) (float f 1d0)
        (float g 1d0) (float h 1d0) (float i 1d0)))

(ss:defspecialization (mat :inline t) ((mat m3:mat))
    mat
  (m3:with-components ((m mat))
    (%mat (float m00 1d0) (float m10 1d0) (float m20 1d0)
          (float m01 1d0) (float m11 1d0) (float m21 1d0)
          (float m02 1d0) (float m12 1d0) (float m22 1d0))))

;;; constants

(u:define-constant +zero+ (%mat 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0)
  :test #'equalp)

(u:define-constant +id+ (%mat 1d0 0d0 0d0 0d0 1d0 0d0 0d0 0d0 1d0)
  :test #'equalp)

;;; operators

(u:fn-> = (mat mat &key (:rel u:f64) (:abs u:f64)) boolean)
(u:defun-inline = (mat1 mat2 &key (rel 1d-7) (abs rel))
  (with-components ((a mat1) (b mat2))
    (and (int:= a00 b00 rel abs)
         (int:= a01 b01 rel abs)
         (int:= a02 b02 rel abs)
         (int:= a10 b10 rel abs)
         (int:= a11 b11 rel abs)
         (int:= a12 b12 rel abs)
         (int:= a20 b20 rel abs)
         (int:= a21 b21 rel abs)
         (int:= a22 b22 rel abs))))

(u:fn-> zero! (mat) mat)
(u:defun-inline zero! (mat)
  (declare (optimize speed))
  (with-components ((m mat))
    (psetf m00 0d0 m01 0d0 m02 0d0
           m10 0d0 m11 0d0 m12 0d0
           m20 0d0 m21 0d0 m22 0d0))
  mat)

(u:fn-> zero-p (mat) boolean)
(u:defun-inline zero-p (mat)
  (declare (optimize speed))
  (= mat +zero+))

(u:fn-> id! (mat) mat)
(u:defun-inline id! (mat)
  (declare (optimize speed))
  (with-components ((m mat))
    (psetf m00 1d0 m01 0d0 m02 0d0
           m10 0d0 m11 1d0 m12 0d0
           m20 0d0 m21 0d0 m22 1d0))
  mat)

(u:fn-> id () mat)
(u:defun-inline id ()
  (declare (optimize speed))
  (id! (mat)))

(u:fn-> id-p (mat) boolean)
(u:defun-inline id-p (mat)
  (declare (optimize speed))
  (= mat +id+))

(u:fn-> random! (mat u:f64 u:f64) mat)
(u:defun-inline random! (out min max)
  (declare (optimize speed))
  (let ((diff (cl:- max min)))
    (with-components ((o out))
      (psetf o00 (cl:+ min (cl:random diff))
             o01 (cl:+ min (cl:random diff))
             o02 (cl:+ min (cl:random diff))
             o10 (cl:+ min (cl:random diff))
             o11 (cl:+ min (cl:random diff))
             o12 (cl:+ min (cl:random diff))
             o20 (cl:+ min (cl:random diff))
             o21 (cl:+ min (cl:random diff))
             o22 (cl:+ min (cl:random diff)))))
  out)

(u:fn-> random (u:f64 u:f64) mat)
(u:defun-inline random (min max)
  (declare (optimize speed))
  (random! (mat) min max))

(u:fn-> copy! (mat mat) mat)
(u:defun-inline copy! (out mat)
  (declare (optimize speed))
  (with-components ((o out) (m mat))
    (psetf o00 m00 o01 m01 o02 m02
           o10 m10 o11 m11 o12 m12
           o20 m20 o21 m21 o22 m22))
  out)

(u:fn-> copy (mat) mat)
(u:defun-inline copy (mat)
  (declare (optimize speed))
  (copy! (mat) mat))

(u:fn-> clamp! (mat mat u:f64 u:f64) mat)
(u:defun-inline clamp! (out mat min max)
  (declare (optimize speed))
  (with-components ((o out) (m mat))
    (psetf o00 (u:clamp m00 min max)
           o01 (u:clamp m01 min max)
           o02 (u:clamp m02 min max)
           o10 (u:clamp m10 min max)
           o11 (u:clamp m11 min max)
           o12 (u:clamp m12 min max)
           o20 (u:clamp m20 min max)
           o21 (u:clamp m21 min max)
           o22 (u:clamp m22 min max)))
  out)

(u:fn-> clamp (mat u:f64 u:f64) mat)
(u:defun-inline clamp (mat min max)
  (declare (optimize speed))
  (clamp! (mat) mat min max))

(u:fn-> +! (mat mat mat) mat)
(u:defun-inline +! (out mat1 mat2)
  (declare (optimize speed))
  (with-components ((o out) (a mat1) (b mat2))
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
           o20 (cl:- a20 b20)
           o01 (cl:- a01 b01)
           o11 (cl:- a11 b11)
           o21 (cl:- a21 b21)
           o02 (cl:- a02 b02)
           o12 (cl:- a12 b12)
           o22 (cl:- a22 b22)))
  out)

(u:fn-> - (mat mat) mat)
(u:defun-inline - (mat1 mat2)
  (declare (optimize speed))
  (-! (mat) mat1 mat2))

(defmacro %* (o00 o01 o02 o10 o11 o12 o20 o21 o22
              a00 a01 a02 a10 a11 a12 a20 a21 a22
              b00 b01 b02 b10 b11 b12 b20 b21 b22)
  `(psetf ,o00 (cl:+ (cl:* ,a00 ,b00) (cl:* ,a01 ,b10) (cl:* ,a02 ,b20))
          ,o10 (cl:+ (cl:* ,a10 ,b00) (cl:* ,a11 ,b10) (cl:* ,a12 ,b20))
          ,o20 (cl:+ (cl:* ,a20 ,b00) (cl:* ,a21 ,b10) (cl:* ,a22 ,b20))
          ,o01 (cl:+ (cl:* ,a00 ,b01) (cl:* ,a01 ,b11) (cl:* ,a02 ,b21))
          ,o11 (cl:+ (cl:* ,a10 ,b01) (cl:* ,a11 ,b11) (cl:* ,a12 ,b21))
          ,o21 (cl:+ (cl:* ,a20 ,b01) (cl:* ,a21 ,b11) (cl:* ,a22 ,b21))
          ,o02 (cl:+ (cl:* ,a00 ,b02) (cl:* ,a01 ,b12) (cl:* ,a02 ,b22))
          ,o12 (cl:+ (cl:* ,a10 ,b02) (cl:* ,a11 ,b12) (cl:* ,a12 ,b22))
          ,o22 (cl:+ (cl:* ,a20 ,b02) (cl:* ,a21 ,b12) (cl:* ,a22 ,b22))))

(u:fn-> *! (mat mat mat) mat)
(u:defun-inline *! (out mat1 mat2)
  (declare (optimize speed))
  (with-components ((o out) (a mat1) (b mat2))
    (%* o00 o01 o02 o10 o11 o12 o20 o21 o22
        a00 a01 a02 a10 a11 a12 a20 a21 a22
        b00 b01 b02 b10 b11 b12 b20 b21 b22))
  out)

(u:fn-> * (mat mat) mat)
(u:defun-inline * (mat1 mat2)
  (declare (optimize speed))
  (*! (mat) mat1 mat2))

(u:fn-> copy-rotation! (mat mat) mat)
(u:defun-inline copy-rotation! (out mat)
  (declare (optimize speed))
  (with-components ((o out) (m mat))
    (psetf o00 m00 o01 m01
           o10 m10 o11 m11))
  out)

(u:fn-> copy-rotation (mat) mat)
(u:defun-inline copy-rotation (mat)
  (declare (optimize speed))
  (copy-rotation! (mat 1) mat))

(u:fn-> rotation-to-mat2! (dm2:mat mat) dm2:mat)
(u:defun-inline rotation-to-mat2! (out mat)
  (declare (optimize speed))
  (dm2:with-components ((o out))
    (with-components ((m mat))
      (psetf o00 m00 o01 m01
             o10 m10 o11 m11)))
  out)

(u:fn-> rotation-to-mat2 (mat) dm2:mat)
(u:defun-inline rotation-to-mat2 (mat)
  (declare (optimize speed))
  (rotation-to-mat2! (dm2:mat 1) mat))

(u:fn-> normalize-rotation! (mat mat) mat)
(u:defun-inline normalize-rotation! (out mat)
  (declare (optimize speed))
  (with-components ((o out) (m mat))
    (dv2::%normalize o00 o10 m00 m10)
    (dv2::%normalize o01 o11 m01 m11)
    (dv2::%normalize o02 o12 m02 m12))
  out)

(u:fn-> normalize-rotation (mat) mat)
(u:defun-inline normalize-rotation (mat)
  (declare (optimize speed))
  (normalize-rotation! (copy mat) mat))

(u:fn-> get-column! (dv3:vec mat (integer 0 2)) dv3:vec)
(u:defun-inline get-column! (out mat index)
  (declare (optimize speed))
  (with-components ((m mat))
    (dv3:with-components ((o out))
      (ecase index
        (0 (psetf ox m00 oy m10 oz m20))
        (1 (psetf ox m01 oy m11 oz m21))
        (2 (psetf ox m02 oy m12 oz m22)))))
  out)

(u:fn-> get-column (mat (integer 0 2)) dv3:vec)
(u:defun-inline get-column (mat index)
  (declare (optimize speed))
  (get-column! (dv3:vec) mat index))

(u:fn-> set-column! (mat mat dv3:vec (integer 0 2)) mat)
(u:defun-inline set-column! (out mat vec index)
  (declare (optimize speed))
  (with-components ((o out))
    (dv3:with-components ((v vec))
      (copy! out mat)
      (ecase index
        (0 (psetf o00 vx o10 vy o20 vz))
        (1 (psetf o01 vx o11 vy o21 vz))
        (2 (psetf o02 vx o12 vy o22 vz)))))
  out)

(u:fn-> set-column (mat dv3:vec (integer 0 2)) mat)
(u:defun-inline set-column (mat vec index)
  (declare (optimize speed))
  (set-column! (mat 1) mat vec index))

(u:fn-> get-translation! (dv2:vec mat) dv2:vec)
(u:defun-inline get-translation! (out mat)
  (declare (optimize speed))
  (with-components ((m mat))
    (dv2:with-components ((o out))
      (psetf ox m02 oy m12)))
  out)

(u:fn-> get-translation (mat) dv2:vec)
(u:defun-inline get-translation (mat)
  (declare (optimize speed))
  (get-translation! (dv2:vec) mat))

(u:fn-> set-translation! (mat mat dv2:vec) mat)
(u:defun-inline set-translation! (out mat vec)
  (declare (optimize speed))
  (with-components ((o out) (m mat))
    (dv2:with-components ((v vec))
      (copy-rotation! out mat)
      (psetf o02 vx o12 vy o22 m22)))
  out)

(u:fn-> set-translation (mat dv2:vec) mat)
(u:defun-inline set-translation (mat vec)
  (declare (optimize speed))
  (set-translation! (copy mat) mat vec))

(u:fn-> translate! (mat mat dv2:vec) mat)
(u:defun-inline translate! (out mat vec)
  (declare (optimize speed))
  (with-components ((o out) (m mat))
    (dv2:with-components ((v vec))
      (copy! out mat)
      (psetf o00 (cl:+ m00 (cl:* m20 vx))
             o01 (cl:+ m01 (cl:* m21 vx))
             o02 (cl:+ m02 (cl:* m22 vx))
             o10 (cl:+ m10 (cl:* m20 vy))
             o11 (cl:+ m11 (cl:* m21 vy))
             o12 (cl:+ m12 (cl:* m22 vy))
             o20 m20
             o21 m21
             o22 m22)))
  out)

(u:fn-> translate (mat dv2:vec) mat)
(u:defun-inline translate (mat vec)
  (declare (optimize speed))
  (translate! (mat 1) mat vec))

(u:fn-> rotation-axis-to-vec2! (dv2:vec mat keyword) dv2:vec)
(u:defun-inline rotation-axis-to-vec2! (out mat axis)
  (declare (optimize speed))
  (dv2:with-components ((v out))
    (with-components ((m mat))
      (ecase axis
        (:x (psetf vx m00 vy m10))
        (:y (psetf vx m01 vy m11)))))
  out)

(u:fn-> rotation-axis-to-vec2 (mat keyword) dv2:vec)
(u:defun-inline rotation-axis-to-vec2 (mat axis)
  (declare (optimize speed))
  (rotation-axis-to-vec2! (dv2:vec) mat axis))

(u:fn-> rotation-axis-from-vec2! (mat dv2:vec keyword) mat)
(u:defun-inline rotation-axis-from-vec2! (out vec axis)
  (declare (optimize speed))
  (with-components ((o out))
    (dv2:with-components ((v vec))
      (ecase axis
        (:x (psetf o00 vx o10 vy))
        (:y (psetf o01 vx o11 vy)))))
  out)

(u:fn-> rotation-axis-from-vec2 (mat dv2:vec keyword) mat)
(u:defun-inline rotation-axis-from-vec2 (mat vec axis)
  (declare (optimize speed))
  (rotation-axis-from-vec2! (copy mat) vec axis))

(u:fn-> rotate! (mat mat u:f64 &key (:space keyword)) mat)
(u:defun-inline rotate! (out mat angle &key (space :local))
  (dm2:with-elements ((m 1d0 0d0 0d0 1d0))
    (with-components ((o out))
      (let ((s (sin angle))
            (c (cos angle)))
        (copy! out mat)
        (psetf m00 c m01 (cl:- s) m10 s m11 c)
        (ecase space
          (:local (dm2::%* o00 o01 o10 o11 o00 o01 o10 o11 m00 m01 m10 m11))
          (:world (dm2::%* o00 o01 o10 o11 m00 m01 m10 m11 o00 o01 o10 o11))))))
  out)

(u:fn-> rotate (mat u:f64) mat)
(u:defun-inline rotate (mat vec)
  (declare (optimize speed))
  (rotate! (mat 1) mat vec))

(u:fn-> get-scale! (dv2:vec mat) dv2:vec)
(u:defun-inline get-scale! (out mat)
  (declare (optimize speed))
  (dv2:with-components ((o out))
    (psetf ox (dv2:length (rotation-axis-to-vec2 mat :x))
           oy (dv2:length (rotation-axis-to-vec2 mat :y))))
  out)

(u:fn-> get-scale (mat) dv2:vec)
(u:defun-inline get-scale (mat)
  (declare (optimize speed))
  (get-scale! (dv2:vec) mat))

(u:fn-> set-scale! (mat mat dv2:vec) mat)
(u:defun-inline set-scale! (out mat vec)
  (declare (optimize speed))
  (with-components ((o out))
    (dv2:with-components ((v vec))
      (copy! out mat)
      (psetf o00 vx o11 vy)))
  out)

(u:fn-> set-scale (mat dv2:vec) mat)
(u:defun-inline set-scale (mat vec)
  (declare (optimize speed))
  (set-scale! (copy mat) mat vec))

(u:fn-> scale! (mat mat dv2:vec) mat)
(u:defun-inline scale! (out mat vec)
  (declare (optimize speed))
  (with-components ((o out) (m mat))
    (dv2:with-components ((v vec))
      (psetf o00 (cl:* m00 vx)
             o01 (cl:* m01 vx)
             o02 (cl:* m02 vx)
             o10 (cl:* m10 vy)
             o11 (cl:* m11 vy)
             o12 (cl:* m12 vy)
             o20 m20
             o21 m21
             o22 m22)))
  out)

(u:fn-> scale (mat dv2:vec) mat)
(u:defun-inline scale (mat vec)
  (declare (optimize speed))
  (scale! (mat 1) mat vec))

(u:fn-> *v3! (dv3:vec mat dv3:vec) dv3:vec)
(u:defun-inline *v3! (out mat vec)
  (declare (optimize speed))
  (dv3:with-components ((v vec) (o out))
    (with-components ((m mat))
      (psetf ox (cl:+ (cl:* m00 vx) (cl:* m01 vy) (cl:* m02 vz))
             oy (cl:+ (cl:* m10 vx) (cl:* m11 vy) (cl:* m12 vz))
             oz (cl:+ (cl:* m20 vx) (cl:* m21 vy) (cl:* m22 vz)))))
  out)

(u:fn-> *v3 (mat dv3:vec) dv3:vec)
(u:defun-inline *v3 (mat vec)
  (declare (optimize speed))
  (*v3! (dv3:vec) mat vec))

(u:fn-> transpose! (mat mat) mat)
(u:defun-inline transpose! (out mat)
  (declare (optimize speed))
  (with-components ((o (copy! out mat)))
    (rotatef o01 o10)
    (rotatef o02 o20)
    (rotatef o12 o21))
  out)

(u:fn-> transpose (mat) mat)
(u:defun-inline transpose (mat)
  (declare (optimize speed))
  (transpose! (mat 1) mat))

(u:fn-> orthogonal-p (mat) boolean)
(defun orthogonal-p (mat)
  (declare (optimize speed))
  (= (* mat (transpose mat)) +id+))

(defmacro %trace (m00 m11 m22)
  `(cl:+ ,m00 ,m11 ,m22))

(u:fn-> trace (mat) u:f64)
(u:defun-inline trace (mat)
  (with-components ((m mat))
    (%trace m00 m11 m22)))

(u:fn-> diagonal-p (mat) boolean)
(u:defun-inline diagonal-p (mat)
  (declare (optimize speed))
  (with-components ((m mat))
    (cl:= 0d0 m10 m20 m01 m21 m02 m12)))

(u:fn-> main-diagonal! (dv3:vec mat) dv3:vec)
(u:defun-inline main-diagonal! (out mat)
  (declare (optimize speed))
  (with-components ((m mat))
    (dv3:with-components ((v out))
      (psetf vx m00 vy m11 vz m22)))
  out)

(u:fn-> main-diagonal (mat) dv3:vec)
(u:defun-inline main-diagonal (mat)
  (declare (optimize speed))
  (main-diagonal! (dv3:vec) mat))

(u:fn-> anti-diagonal! (dv3:vec mat) dv3:vec)
(u:defun-inline anti-diagonal! (out mat)
  (declare (optimize speed))
  (with-components ((m mat))
    (dv3:with-components ((v out))
      (psetf vx m02 vy m11 vz m20)))
  out)

(u:fn-> anti-diagonal (mat) dv3:vec)
(u:defun-inline anti-diagonal (mat)
  (declare (optimize speed))
  (anti-diagonal! (dv3:vec) mat))
