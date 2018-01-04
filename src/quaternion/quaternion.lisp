(in-package :box.math.quat)

;;; Structure

(deftype quat () '(simple-array single-float (4)))

(defstruct (quat (:type (vector single-float))
                 (:constructor %make (w x y z))
                 (:conc-name nil)
                 (:copier nil))
  (w 0.0f0 :type single-float)
  (x 0.0f0 :type single-float)
  (y 0.0f0 :type single-float)
  (z 0.0f0 :type single-float))

(defmacro with-components (((prefix quat) &rest rest) &body body)
  `(with-accessors ((,prefix identity)
                    (,(%make-accessor-symbol prefix 'w) w)
                    (,(%make-accessor-symbol prefix 'x) x)
                    (,(%make-accessor-symbol prefix 'y) y)
                    (,(%make-accessor-symbol prefix 'z) z))
       ,quat
     ,(if rest
          `(with-components ,rest ,@body)
          `(progn ,@body))))

(set-pprint-dispatch
 'quat
 (lambda (stream object)
   (print-unreadable-object (object stream)
     (with-components ((q object))
       (format stream "~f ~f ~f ~f" qw qx qy qz))))
 1)

;;; Constants

(alexandria:define-constant +zero+
    (make-array 4 :element-type 'single-float
                  :initial-contents '(0.0f0 0.0f0 0.0f0 0.0f0))
  :test #'equalp)

(alexandria:define-constant +id+
    (make-array 4 :element-type 'single-float
                  :initial-contents '(1.0f0 0.0f0 0.0f0 0.0f0))
  :test #'equalp)

;;; Operations

(declaim (inline make))
(defun* (make -> quat) ((w real) (x real) (y real) (z real))
  (%make (float w 1.0f0) (float x 1.0f0) (float y 1.0f0) (float z 1.0f0)))

(defun* (id! -> quat) ((quat quat))
  (with-components ((q quat))
    (psetf qw 1.0f0 qx 0.0f0 qy 0.0f0 qz 0.0f0))
  quat)

(defun* (id -> quat) ()
  (id! (make 0 0 0 0)))

(declaim (inline zero!))
(defun* (zero! -> quat) ((quat quat))
  (with-components ((q quat))
    (psetf qw 0.0f0 qx 0.0f0 qy 0.0f0 qz 0.0f0))
  quat)

(declaim (inline zero))
(defun* (zero -> quat) ()
  (make 0 0 0 0))

(declaim (inline =))
(defun* (= -> boolean) ((quat1 quat) (quat2 quat))
  (with-components ((q1 quat1) (q2 quat2))
    (and (cl:= q1w q2w)
         (cl:= q1x q2x)
         (cl:= q1y q2y)
         (cl:= q1z q2z))))

(declaim (inline ~))
(defun* (~ -> boolean) ((quat1 quat) (quat2 quat)
                        &key
                        ((tolerance single-float) +epsilon+))
  (with-components ((q1 quat1) (q2 quat2))
    (and (%~ q1w q2w tolerance)
         (%~ q1x q2x tolerance)
         (%~ q1y q2y tolerance)
         (%~ q1z q2z tolerance))))

(declaim (inline copy!))
(defun* (copy! -> quat) ((out quat) (quat quat))
  (with-components ((o out) (q quat))
    (psetf ow qw ox qx oy qy oz qz))
  out)

(declaim (inline copy))
(defun* (copy -> quat) ((quat quat))
  (copy! (id) quat))

(declaim (inline +!))
(defun* (+! -> quat) ((out quat) (quat1 quat) (quat2 quat))
  (with-components ((o out) (q1 quat1) (q2 quat2))
    (psetf ow (cl:+ q1w q2w)
           ox (cl:+ q1x q2x)
           oy (cl:+ q1y q2y)
           oz (cl:+ q1z q2z)))
  out)

(declaim (inline +))
(defun* (+ -> quat) ((quat1 quat) (quat2 quat))
  (+! (id) quat1 quat2))

(declaim (inline -!))
(defun* (-! -> quat) ((out quat) (quat1 quat) (quat2 quat))
  (with-components ((o out) (q1 quat1) (q2 quat2))
    (psetf ow (cl:- q1w q2w)
           ox (cl:- q1x q2x)
           oy (cl:- q1y q2y)
           oz (cl:- q1z q2z)))
  out)

(declaim (inline -))
(defun* (- -> quat) ((quat1 quat) (quat2 quat))
  (-! (id) quat1 quat2))

(declaim (inline *!))
(defun* (*! -> quat) ((out quat) (quat1 quat) (quat2 quat))
  (with-components ((o out) (q1 quat1) (q2 quat2))
    (psetf ow (cl:- (cl:* q1w q2w) (cl:* q1x q2x) (cl:* q1y q2y) (cl:* q1z q2z))
           ox (cl:- (cl:+ (cl:* q1w q2x) (cl:* q1x q2w) (cl:* q1y q2z))
                    (cl:* q1z q2y))
           oy (cl:- (cl:+ (cl:* q1w q2y) (cl:* q1y q2w) (cl:* q1z q2x))
                    (cl:* q1x q2z))
           oz (cl:- (cl:+ (cl:* q1w q2z) (cl:* q1z q2w) (cl:* q1x q2y))
                    (cl:* q1y q2x))))
  out)

(declaim (inline *))
(defun* (* -> quat) ((quat1 quat) (quat2 quat))
  (*! (id) quat1 quat2))

(declaim (inline scale!))
(defun* (scale! -> quat) ((out quat) (quat quat) (scalar single-float))
  (with-components ((o out) (q quat))
    (psetf ow (cl:* qw scalar)
           ox (cl:* qx scalar)
           oy (cl:* qy scalar)
           oz (cl:* qz scalar)))
  out)

(declaim (inline scale))
(defun* (scale -> quat) ((quat quat) (scalar single-float))
  (scale! (id) quat scalar))

(declaim (inline conjugate!))
(defun* (conjugate! -> quat) ((out quat) (quat quat))
  (with-components ((o out) (q quat))
    (psetf ow qw
           ox (cl:- qx)
           oy (cl:- qy)
           oz (cl:- qz)))
  out)

(declaim (inline conjugate))
(defun* (conjugate -> quat) ((quat quat))
  (conjugate! (id) quat))

(declaim (inline cross!))
(defun* (cross! -> quat) ((out quat) (quat1 quat) (quat2 quat))
  (scale!
   out
   (+ (* quat2 (conjugate quat1))
      (* quat1 quat2))
   0.5f0))

(declaim (inline cross))
(defun* (cross -> quat) ((quat1 quat) (quat2 quat))
  (cross! (id) quat1 quat2))

(declaim (inline magnitude-squared))
(defun* (magnitude-squared -> single-float) ((quat quat))
  (with-components ((q quat))
    (cl:+ (cl:* qw qw) (cl:* qx qx) (cl:* qy qy) (cl:* qz qz))))

(declaim (inline magnitude))
(defun* (magnitude -> single-float) ((quat quat))
  (sqrt (magnitude-squared quat)))

(declaim (inline normalize!))
(defun* (normalize! -> quat) ((out quat) (quat quat))
  (let ((magnitude (magnitude quat)))
    (unless (zerop magnitude)
      (scale! out quat (/ magnitude))))
  out)

(declaim (inline normalize))
(defun* (normalize -> quat) ((quat quat))
  (normalize! (id) quat))

(declaim (inline negate!))
(defun* (negate! -> quat) ((out quat) (quat quat))
  (scale! out quat -1.0f0))

(declaim (inline negate))
(defun* (negate -> quat) ((quat quat))
  (negate! (id) quat))

(declaim (inline dot))
(defun* (dot -> single-float) ((quat1 quat) (quat2 quat))
  (with-components ((q1 quat1) (q2 quat2))
    (cl:+ (cl:* q1w q2w)
          (cl:* q1x q2x)
          (cl:* q1y q2y)
          (cl:* q1z q2z))))

(declaim (inline inverse!))
(defun* (inverse! -> quat) ((out quat) (quat quat))
  (conjugate! out quat)
  (scale! out out (/ (magnitude-squared quat)))
  out)

(declaim (inline inverse))
(defun* (inverse -> quat) ((quat quat))
  (inverse! (id) quat))

(declaim (inline rotate!))
(defun* (rotate! -> quat) ((out quat) (quat quat) (vec v3:vec))
  (with-components ((o out) (q (copy quat)))
    (v3:with-components ((v (v3:scale vec 0.5f0))
                         (c (v3:make (cos vx) (cos vy) (cos vz)))
                         (s (v3:make (sin vx) (sin vy) (sin vz))))
      (psetf ow (cl:- (cl:* cx cy cz) (cl:* sx sy sz))
             ox (cl:+ (cl:* sx cy cz) (cl:* cx sy sz))
             oy (cl:- (cl:* cx sy cz) (cl:* sx cy sz))
             oz (cl:+ (cl:* sx sy cz) (cl:* cx cy sz)))
      (*! out out q)))
  out)

(defun* (rotate -> quat) ((quat quat) (vec v3:vec))
  (rotate! (id) quat vec))

(declaim (inline to-vec3!))
(defun* (to-vec3! -> v3:vec) ((out v3:vec) (quat quat))
  (v3:with-components ((v out))
    (with-components ((q quat))
      (setf vx qx vy qy vz qz)))
  out)

(declaim (inline to-vec3))
(defun* (to-vec3 -> v3:vec) ((quat quat))
  (to-vec3! (v3:zero) quat))

(declaim (inline to-vec4!))
(defun* (to-vec4! -> v4:vec) ((out v4:vec) (quat quat))
  (v4:with-components ((v out))
    (with-components ((q quat))
      (setf vx qw vy qx vz qy vw qz)))
  out)

(declaim (inline to-vec4))
(defun* (to-vec4 -> v4:vec) ((quat quat))
  (to-vec4! (v4:zero) quat))

(declaim (inline from-vec3!))
(defun* (from-vec3! -> quat) ((out quat) (vec v3:vec))
  (with-components ((q out))
    (v3:with-components ((v vec))
      (setf qw 0.0f0 qx vx qy vy qz vz)))
  out)

(declaim (inline from-vec3))
(defun* (from-vec3 -> quat) ((vec v3:vec))
  (from-vec3! (zero) vec))

(declaim (inline from-vec4!))
(defun* (from-vec4! -> quat) ((out quat) (vec v4:vec))
  (with-components ((q out))
    (v4:with-components ((v vec))
      (setf qw vx qx vy qy vz qz vw)))
  out)

(declaim (inline v4->q))
(defun* (from-vec4 -> quat) ((vec v4:vec))
  (from-vec4! (zero) vec))

(declaim (inline to-mat4!))
(defun* (to-mat4! -> m4:matrix) ((out m4:matrix) (quat quat))
  (m4:with-components ((o out))
    (with-components ((q quat))
      (let* ((s (/ 2 (magnitude-squared quat)))
             (xs (cl:* qx s))
             (ys (cl:* qy s))
             (zs (cl:* qz s))
             (xx (cl:* qx xs))
             (xy (cl:* qx ys))
             (xz (cl:* qx zs))
             (yy (cl:* qy ys))
             (yz (cl:* qy zs))
             (zz (cl:* qz zs))
             (wx (cl:* qw xs))
             (wy (cl:* qw ys))
             (wz (cl:* qw zs)))
        (psetf o00 (cl:- 1 (cl:+ yy zz))
               o01 (cl:- xy wz)
               o02 (cl:+ xz wy)
               o03 0.0f0
               o10 (cl:+ xy wz)
               o11 (cl:- 1 (cl:+ xx zz))
               o12 (cl:- yz wx)
               o13 0.0f0
               o20 (cl:- xz wy)
               o21 (cl:+ yz wx)
               o22 (cl:- 1 (cl:+ xx yy))
               o23 0.0f0
               o30 0.0f0
               o31 0.0f0
               o32 0.0f0
               o33 1.0f0))))
  out)

(declaim (inline to-mat4))
(defun* (to-mat4 -> m4:matrix) ((quat quat))
  (to-mat4! (m4:id) quat))

(defun* (from-mat4! -> quat) ((out quat) (matrix m4:matrix))
  (with-components ((q out))
    (m4:with-components ((m matrix))
      (let ((trace (m4:trace matrix))
            (col1 (1+ (cl:- m00 m11 m22)))
            (col2 (1+ (cl:- m11 m00 m22)))
            (col3 (1+ (cl:- m22 m00 m11)))
            (s 0.0f0))
        (cond
          ((plusp trace)
           (setf s (/ 0.5f0 (sqrt trace))
                 qw (/ 0.25f0 s)
                 qx (cl:* (cl:- m21 m12) s)
                 qy (cl:* (cl:- m02 m20) s)
                 qz (cl:* (cl:- m10 m01) s)))
          ((and (>= col1 col2) (>= col1 col3))
           (setf s (/ 0.5f0 (sqrt col1))
                 qw (cl:* (cl:- m21 m12) s)
                 qx (/ 0.25f0 s)
                 qy (cl:* (cl:+ m10 m01) s)
                 qz (cl:* (cl:+ m02 m20) s)))
          ((and (>= col2 col1) (>= col2 col3))
           (setf s (/ 0.5f0 (sqrt col2))
                 qw (cl:* (cl:- m02 m20) s)
                 qx (cl:* (cl:+ m01 m10) s)
                 qy (/ 0.25f0 s)
                 qz (cl:* (cl:+ m12 m21) s)))
          (t
           (setf s (/ 0.5f0 (sqrt col3))
                 qw (cl:* (cl:- m10 m01) s)
                 qx (cl:* (cl:+ m02 m20) s)
                 qy (cl:* (cl:+ m12 m21) s)
                 qz (/ 0.25f0 s)))))))
  out)

(declaim (inline from-mat4))
(defun* (from-mat4 -> quat) ((matrix m4:matrix))
  (from-mat4! (id) matrix))

(defun* (slerp! -> quat) ((out quat) (quat1 quat) (quat2 quat)
                          (factor single-float))
  (with-components ((o out) (q1 quat1) (q2 quat2))
    (let ((dot (dot q1 q2))
          (q2 q2))
      (when (minusp dot)
        (negate! q2 q2)
        (setf dot (cl:- dot)))
      (if (> (abs dot) 0.9995f0)
          (psetf ow (alexandria:lerp factor q1w q2w)
                 ox (alexandria:lerp factor q1x q2x)
                 oy (alexandria:lerp factor q1y q2y)
                 oz (alexandria:lerp factor q1z q2z))
          (let* ((angle (acos (alexandria:clamp dot 0 1)))
                 (sin-angle (sin angle))
                 (scale1 (/ (sin (cl:* angle (cl:- 1 factor))) sin-angle))
                 (scale2 (/ (sin (cl:* factor angle)) sin-angle)))
            (psetf ow (cl:+ (cl:* q1w scale1) (cl:* q2w scale2))
                   ox (cl:+ (cl:* q1x scale1) (cl:* q2x scale2))
                   oy (cl:+ (cl:* q1y scale1) (cl:* q2y scale2))
                   oz (cl:+ (cl:* q1z scale1) (cl:* q2z scale2)))))))
  out)

(declaim (inline slerp))
(defun* (slerp -> quat) ((quat1 quat) (quat2 quat) (factor single-float))
  (slerp! (id) quat1 quat2 factor))
