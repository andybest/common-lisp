(in-package :box.math.dquat)

;;; Structure

(deftype dquat () '(simple-array q:quat (2)))

(defstruct (dquat (:type vector)
                  (:constructor dquat (real dual))
                  (:conc-name dq-)
                  (:copier nil))
  (real (q:zero) :type q:quat)
  (dual (q:zero) :type q:quat))

(defmacro with-components (((prefix dquat) . rest) &body body)
  `(q:with-components ((,prefix ,dquat)
                       (,(%make-accessor-symbol prefix '.r) (dq-real ,dquat))
                       (,(%make-accessor-symbol prefix '.d) (dq-dual ,dquat)))
     ,dquat
     ,(if rest
          `(with-components ,rest ,@body)
          `(progn ,@body))))

;;; Operations

(declaim (inline id!))
(defun* (id! -> dquat) ((dquat dquat))
  (with-components ((d dquat))
    (q:id! d.r)
    (psetf d.dw 0.0f0 d.dx 0.0f0 d.dy 0.0f0 d.dz 0.0f0))
  dquat)

(declaim (inline id))
(defun* (id -> dquat) ()
  (id! (dquat (q:id) (q:zero))))

(declaim (inline zero!))
(defun* (zero! -> dquat) ((dquat dquat))
  (with-components ((d dquat))
    (q:zero! d.r)
    (q:zero! d.d))
  dquat)

(declaim (inline zero))
(defun* zero ()
  (dquat (q:zero) (q:zero)))

(declaim (inline =))
(defun* (= -> boolean) ((dquat1 dquat) (dquat2 dquat))
  (with-components ((d1 dquat1) (d2 dquat2))
    (and (q:= d1.r d2.r)
         (q:= d1.d d2.d))))

(declaim (inline ~))
(defun* (~ -> boolean) ((dquat1 dquat) (dquat2 dquat)
                        &key
                        ((tolerance single-float) +epsilon+))
  (with-components ((d1 dquat1) (d2 dquat2))
    (and (q:~ d1.r d2.r :tolerance tolerance)
         (q:~ d1.d d2.d :tolerance tolerance))))

(declaim (inline copy!))
(defun* (copy! -> dquat) ((out dquat) (dquat dquat))
  (with-components ((o out) (d dquat))
    (q:copy! o.r d.r)
    (q:copy! o.d d.d))
  out)

(declaim (inline copy))
(defun* (copy -> dquat) ((dquat dquat))
  (copy! (id) dquat))

(declaim (inline +!))
(defun* (+! -> dquat) ((out dquat) (dquat1 dquat) (dquat2 dquat))
  (with-components ((o out) (d1 dquat1) (d2 dquat2))
    (q:+! o.r d1.r d2.r)
    (q:+! o.d d1.d d2.d))
  out)

(declaim (inline +))
(defun* (+ -> dquat) ((dquat1 dquat) (dquat2 dquat))
  (+! (id) dquat1 dquat2))

(declaim (inline -!))
(defun* (-! -> dquat) ((out dquat) (dquat1 dquat) (dquat2 dquat))
  (with-components ((o out) (d1 dquat1) (d2 dquat2))
    (q:-! o.r d1.r d2.r)
    (q:-! o.d d1.d d2.d))
  out)

(declaim (inline -))
(defun* (- -> dquat) ((dquat1 dquat) (dquat2 dquat))
  (-! (id) dquat1 dquat2))

(defun* (*! -> dquat) ((out dquat) (dquat1 dquat) (dquat2 dquat))
  (let ((dual1 (q:zero))
        (dual2 (q:zero)))
    (with-components ((o out) (d1 dquat1) (d2 dquat2))
      (q:*! o.r d1.r d2.r)
      (q:*! dual1 d1.r d2.d)
      (q:*! dual2 d1.d d2.r)
      (q:+! o.d dual1 dual2)))
  out)

(declaim (inline *))
(defun* (* -> dquat) ((dquat1 dquat) (dquat2 dquat))
  (*! (id) dquat1 dquat2))

(declaim (inline scale!))
(defun* (scale! -> dquat) ((out dquat) (dquat dquat)
                           (scalar single-float))
  (with-components ((o out) (d dquat))
    (q:scale! o.r d.r scalar)
    (q:scale! o.d d.d scalar))
  out)

(declaim (inline dqscale))
(defun* (scale -> dquat) ((dquat dquat) (scalar single-float))
  (scale! (id) dquat scalar))

(declaim (inline conjugate!))
(defun* (conjugate! -> dquat) ((out dquat) (dquat dquat))
  (with-components ((o out) (d dquat))
    (q:conjugate! o.r d.r)
    (q:conjugate! o.d d.d))
  out)

(declaim (inline conjugate))
(defun* (conjugate -> dquat) ((dquat dquat))
  (conjugate! (id) dquat))

(declaim (inline conjugate-full!))
(defun* (conjugate-full! -> dquat) ((out dquat) (dquat dquat))
  (with-components ((o out) (d dquat))
    (q:conjugate! o.r d.r)
    (psetf o.dw (cl:- d.dw) o.dx d.dx o.dy d.dy o.dz d.dz))
  out)

(declaim (inline conjugate-full))
(defun* (conjugate-full -> dquat) ((dquat dquat))
  (conjugate-full! (id) dquat))

(declaim (inline magnitude-squared))
(defun* (magnitude-squared -> single-float) ((dquat dquat))
  (with-components ((d dquat))
    (q:magnitude-squared d.r)))

(declaim (inline magnitude))
(defun* (magnitude -> single-float) ((dquat dquat))
  (sqrt (magnitude-squared dquat)))

(declaim (inline negate!))
(defun* (negate! -> dquat) ((out dquat) (dquat dquat))
  (scale! out dquat -1.0f0))

(declaim (inline negate))
(defun* (negate -> dquat) ((dquat dquat))
  (negate! (id) dquat))

(declaim (inline normalize!))
(defun* (normalize! -> dquat) ((out dquat) (dquat dquat))
  (let ((magnitude (magnitude dquat)))
    (unless (zerop magnitude)
      (scale! out dquat (/ magnitude))))
  out)

(declaim (inline normalize))
(defun* (normalize -> dquat) ((dquat dquat))
  (normalize! (id) dquat))

(defun* (apply! -> dquat) ((out dquat) (dquat1 dquat) (dquat2 dquat))
  (let ((dquat2 (normalize dquat2)))
    (*! out (* dquat2 dquat1) (conjugate-full dquat2))))

(declaim (inline apply))
(defun* (apply -> dquat) ((dquat1 dquat) (dquat2 dquat))
  (apply! (id) dquat1 dquat2))

(declaim (inline dot))
(defun* (dot -> single-float) ((dquat1 dquat) (dquat2 dquat))
  (with-components ((d1 dquat1) (d2 dquat2))
    (q:dot d1.r d2.r)))

(defun* (inverse! -> dquat) ((out dquat) (dquat dquat))
  (with-components ((o out) (d dquat))
    (q:inverse! o.r d.r)
    (q:scale! o.d (q:* o.r (q:* d.d o.r)) -1.0f0))
  out)

(declaim (inline inverse))
(defun* (inverse -> dquat) ((dquat dquat))
  (inverse! (id) dquat))

(declaim (inline translation->v3!))
(defun* (translation->v3! -> v3:vec) ((out v3:vec) (dquat dquat))
  (let ((s (q:zero))
        (c (q:zero)))
    (v3:with-components ((o out))
      (with-components ((d dquat))
        (q:scale! s d.d 2.0f0)
        (q:conjugate! c d.r)
        (q:with-components ((q (q:* s c)))
          (setf ox qx oy qy oz qz)))))
  out)

(declaim (inline translation->v3))
(defun* (translation->v3 -> v3:vec) ((dquat dquat))
  (translation->v3! (v3:zero) dquat))

(declaim (inline v3->translation!))
(defun* (v3->translation! -> dquat) ((out dquat) (vec v3:vec))
  (with-components ((o (id! out)))
    (q:v3->q! o.d vec)
    (q:scale! o.d o.d 0.5f0))
  out)

(declaim (inline v3->translation))
(defun* (v3->translation -> dquat) ((vec v3:vec))
  (v3->translation! (zero) vec))

(declaim (inline translate!))
(defun* (translate! -> dquat) ((out dquat) (dquat dquat) (vec v3:vec))
  (*! out (v3->translation vec) dquat))

(declaim (inline translate))
(defun* (translate -> dquat) ((dquat dquat) (vec v3:vec))
  (translate! (id) dquat vec))

(declaim (inline rotation->q!))
(defun* (rotation->q! -> q:quat) ((out q:quat) (dquat dquat))
  (with-components ((d dquat))
    (q:copy! out d.r))
  out)

(declaim (inline rotation->q))
(defun* (rotation->q -> q:quat) ((dquat dquat))
  (rotation->q! (q:zero) dquat))

(declaim (inline q->rotation!))
(defun* (q->rotation! -> dquat) ((out dquat) (quat q:quat))
  (with-components ((o out))
    (q:copy! o.r quat)
    (q:zero! o.d))
  out)

(declaim (inline q->rotation))
(defun* (q->rotation -> dquat) ((quat q:quat))
  (q->rotation! (id) quat))

(defun* (rotate! -> dquat) ((out dquat) (dquat dquat) (vec v3:vec))
  (with-components ((o out) (d dquat))
    (q:rotate! o.r d.r vec))
  out)

(declaim (inline rotate))
(defun* (rotate -> dquat) ((dquat dquat) (vec v3:vec))
  (rotate! (id) dquat vec))

(defun* (->m4! -> m4:matrix) ((out m4:matrix) (dquat dquat))
  (m4:with-components ((o out))
    (with-components ((d dquat))
      (v3:with-components ((v (translation->v3 dquat)))
        (q:->m4! o d.r)
        (psetf o03 vx o13 vy o23 vz))))
  out)

(declaim (inline ->m4))
(defun* (->m4 -> m4:matrix) ((dquat dquat))
  (->m4! (m4:id) dquat))

(defun* (m4->dq! -> dquat) ((out dquat) (matrix m4:matrix))
  (let ((rot (q->rotation (q:m4->q matrix)))
        (tr (v3->translation (m4:translation->v3 matrix))))
    (*! out tr rot))
  out)

(declaim (inline m4->dq))
(defun* (m4->dq -> dquat) ((matrix m4:matrix))
  (m4->dq! (id) matrix))

(defun* (->screw -> (values single-float single-float v3:vec v3:vec))
    ((dquat dquat))
  (with-components ((d (normalize dquat)))
    (let* ((angle (cl:* 2 (acos (alexandria:clamp d.rw -1 1))))
           (dir (v3:normalize (q:->v3 d.r)))
           (tr (translation->v3 dquat))
           (pitch (v3:dot tr dir))
           (moment (v3:scale
                    (v3:+ (v3:cross tr dir)
                          (v3:scale (v3:- (v3:scale tr (v3:dot dir dir))
                                          (v3:scale dir pitch))
                                    (/ (tan (/ angle 2)))))
                    0.5f0)))
      (values angle pitch dir moment))))

(defun* (screw->dq! -> dquat) ((out dquat) (angle single-float)
                               (pitch single-float) (direction v3:vec)
                               (moment v3:vec))
  (let* ((half-angle (cl:* angle 0.5f0))
         (c (cos half-angle))
         (s (sin half-angle)))
    (v3:with-components ((r (v3:scale direction s))
                         (d (v3:+ (v3:scale moment s)
                                  (v3:scale direction (cl:* pitch c 0.5f0)))))
      (setf (dq-real out) (q:quat c rx ry rz)
            (dq-dual out) (q:quat (cl:- (cl:* pitch s 0.5f0)) dx dy dz))))
  out)

(declaim (inline screw->dq))
(defun* (screw->dq -> dquat) ((angle single-float) (pitch single-float)
                              (direction v3:vec) (moment v3:vec))
  (screw->dq! (id) angle pitch direction moment))

(defun* (sclerp! -> dquat) ((out dquat) (dquat1 dquat) (dquat2 dquat)
                            (factor single-float))
  (let ((diff (* (inverse dquat1) dquat2)))
    (multiple-value-bind (angle pitch direction moment) (->screw diff)
      (*! out dquat1 (screw->dq (cl:* angle factor)
                                (cl:* pitch factor)
                                direction
                                moment))))
  out)

(declaim (inline sclerp))
(defun* (sclerp -> dquat) ((dquat1 dquat) (dquat2 dquat)
                           (factor single-float))
  (sclerp! (id) dquat1 dquat2 factor))

(defun* (nlerp! -> dquat) ((out dquat) (dquat1 dquat) (dquat2 dquat)
                           (factor single-float))
  (+! out dquat1 (scale (- dquat2 dquat1) factor)))

(declaim (inline nlerp))
(defun* (nlerp -> dquat) ((dquat1 dquat) (dquat2 dquat) (factor single-float))
  (nlerp! (id) dquat1 dquat2 factor))
