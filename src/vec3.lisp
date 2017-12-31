(in-package :gamebox-math)

;;; Structure

(deftype vec3 () '(simple-array single-float (3)))

(defstruct (vec3 (:type (vector single-float))
                 (:constructor %vec3 (x y z))
                 (:conc-name %v3)
                 (:copier nil))
  (x 0.0f0 :type single-float)
  (y 0.0f0 :type single-float)
  (z 0.0f0 :type single-float))

(defmacro with-vec3 (binds &body body)
  (if (null binds)
      `(progn ,@body)
      (let ((prefix (caar binds)))
        `(with-accessors ((,prefix identity)
                          (,(make-accessor-symbol prefix '.x) %v3x)
                          (,(make-accessor-symbol prefix '.y) %v3y)
                          (,(make-accessor-symbol prefix '.z) %v3z))
             ,(cadar binds)
           (with-vec3 ,(cdr binds) ,@body)))))

(set-pprint-dispatch
 'vec3
 (lambda (stream object)
   (print-unreadable-object (object stream)
     (with-vec3 ((v object))
       (format stream "~f ~f ~f" v.x v.y v.z))))
 1)

;;; Constants

(define-constant +v3zero+
    (make-array 3 :element-type 'single-float
                  :initial-contents '(0.0f0 0.0f0 0.0f0))
  :test #'equalp)

;;; Operations

(declaim (inline vec3))
(defun* (vec3 -> vec3) ((x real) (y real) (z real))
  (%vec3 (float x 1.0f0) (float y 1.0f0) (float z 1.0f0)))

(declaim (inline v3zero!))
(defun* (v3zero! -> vec3) ((vec vec3))
  (with-vec3 ((v vec))
    (psetf v.x 0.0f0 v.y 0.0f0 v.z 0.0f0))
  vec)

(declaim (inline v3zero))
(defun* (v3zero -> vec3) ()
  (vec3 0 0 0))

(declaim (inline v3cp!))
(defun* (v3cp! -> vec3) ((out-vec vec3) (vec vec3))
  (with-vec3 ((o out-vec) (v vec))
    (psetf o.x v.x o.y v.y o.z v.z))
  out-vec)

(declaim (inline v3cp))
(defun* (v3cp -> vec3) ((vec vec3))
  (v3cp! (v3zero) vec))

(declaim (inline v3clamp!))
(defun* (v3clamp! -> vec3) ((out-vec vec3) (vec vec3)
                            &key
                            ((min single-float) most-negative-single-float)
                            ((max single-float) most-positive-single-float))
  (with-vec3 ((o out-vec) (v vec))
    (psetf o.x (clamp v.x min max)
           o.y (clamp v.y min max)
           o.z (clamp v.z min max)))
  out-vec)

(declaim (inline v3clamp))
(defun* (v3clamp -> vec3) ((vec vec3)
                           &key
                           ((min single-float) most-negative-single-float)
                           ((max single-float) most-positive-single-float))
  (v3clamp! (v3zero) vec :min min :max max))

(declaim (inline v3stab!))
(defun* (v3stab! -> vec3) ((out-vec vec3) (vec vec3)
                           &key
                           ((tolerance single-float) +epsilon+))
  (with-vec3 ((o out-vec) (v vec))
    (macrolet ((stabilize (place)
                 `(if (< (abs ,place) tolerance) 0.0f0 ,place)))
      (psetf o.x (stabilize v.x)
             o.y (stabilize v.y)
             o.z (stabilize v.z))))
  out-vec)

(declaim (inline v3stab))
(defun* (v3stab -> vec3) ((vec vec3) &key ((tolerance single-float) +epsilon+))
  (v3stab! (v3zero) vec :tolerance tolerance))

(declaim (inline v3->list))
(defun* (v3->list -> list) ((vec vec3))
  (with-vec3 ((v vec))
    (list v.x v.y v.z)))

(declaim (inline list->v3))
(defun* (list->v3 -> vec3) ((list list))
  (apply #'vec3 list))

(declaim (inline v3=))
(defun* (v3= -> boolean) ((vec-a vec3) (vec-b vec3))
  (with-vec3 ((v1 vec-a) (v2 vec-b))
    (and (= v1.x v2.x)
         (= v1.y v2.y)
         (= v1.z v2.z))))

(declaim (inline v3~))
(defun* (v3~ -> boolean) ((vec-a vec3) (vec-b vec3)
                          &key
                          ((tolerance single-float) +epsilon+))
  (with-vec3 ((v1 vec-a) (v2 vec-b))
    (and (~ v1.x v2.x tolerance)
         (~ v1.y v2.y tolerance)
         (~ v1.z v2.z tolerance))))

(declaim (inline v3+!))
(defun* (v3+! -> vec3) ((out-vec vec3) (vec-a vec3) (vec-b vec3))
  (with-vec3 ((o out-vec) (v1 vec-a) (v2 vec-b))
    (psetf o.x (+ v1.x v2.x)
           o.y (+ v1.y v2.y)
           o.z (+ v1.z v2.z)))
  out-vec)

(declaim (inline v3+))
(defun* (v3+ -> vec3) ((vec-a vec3) (vec-b vec3))
  (v3+! (v3zero) vec-a vec-b))

(declaim (inline v3-!))
(defun* (v3-! -> vec3) ((out-vec vec3) (vec-a vec3) (vec-b vec3))
  (with-vec3 ((o out-vec) (v1 vec-a) (v2 vec-b))
    (psetf o.x (- v1.x v2.x)
           o.y (- v1.y v2.y)
           o.z (- v1.z v2.z)))
  out-vec)

(declaim (inline v3-))
(defun* (v3- -> vec3) ((vec-a vec3) (vec-b vec3))
  (v3-! (v3zero) vec-a vec-b))

(declaim (inline v3had*!))
(defun* (v3had*! -> vec3) ((out-vec vec3) (vec-a vec3) (vec-b vec3))
  (with-vec3 ((o out-vec) (v1 vec-a) (v2 vec-b))
    (psetf o.x (* v1.x v2.x)
           o.y (* v1.y v2.y)
           o.z (* v1.z v2.z)))
  out-vec)

(declaim (inline v3had*))
(defun* (v3had* -> vec3) ((vec-a vec3) (vec-b vec3))
  (v3had*! (v3zero) vec-a vec-b))

(declaim (inline v3had/!))
(defun* (v3had/! -> vec3) ((out-vec vec3) (vec-a vec3) (vec-b vec3))
  (with-vec3 ((o out-vec) (v1 vec-a) (v2 vec-b))
    (psetf o.x (if (zerop v2.x) 0.0f0 (/ v1.x v2.x))
           o.y (if (zerop v2.y) 0.0f0 (/ v1.y v2.y))
           o.z (if (zerop v2.z) 0.0f0 (/ v1.z v2.z))))
  out-vec)

(declaim (inline v3had/))
(defun* (v3had/ -> vec3) ((vec-a vec3) (vec-b vec3))
  (v3had/! (v3zero) vec-a vec-b))

(declaim (inline v3scale!))
(defun* (v3scale! -> vec3) ((out-vec vec3) (vec vec3) (scalar single-float))
  (with-vec3 ((o out-vec) (v vec))
    (psetf o.x (* v.x scalar)
           o.y (* v.y scalar)
           o.z (* v.z scalar)))
  out-vec)

(declaim (inline v3scale))
(defun* (v3scale -> vec3) ((vec vec3) (scalar single-float))
  (v3scale! (v3zero) vec scalar))

(declaim (inline v3dot))
(defun* (v3dot -> single-float) ((vec-a vec3) (vec-b vec3))
  (with-vec3 ((v1 vec-a) (v2 vec-b))
    (+ (* v1.x v2.x) (* v1.y v2.y) (* v1.z v2.z))))

(declaim (inline v3magsq))
(defun* (v3magsq -> single-float) ((vec vec3))
  (v3dot vec vec))

(declaim (inline v3mag))
(defun* (v3mag -> single-float) ((vec vec3))
  (sqrt (v3magsq vec)))

(declaim (inline v3normalize!))
(defun* (v3normalize! -> vec3) ((out-vec vec3) (vec vec3))
  (let ((magnitude (v3mag vec)))
    (unless (zerop magnitude)
      (v3scale! out-vec vec (/ magnitude))))
  out-vec)

(declaim (inline v3normalize))
(defun* (v3normalize -> vec3) ((vec vec3))
  (v3normalize! (v3zero) vec))

(declaim (inline v3round!))
(defun* (v3round! -> vec3) ((out-vec vec3) (vec vec3))
  (with-vec3 ((o out-vec) (v vec))
    (psetf o.x (fround v.x)
           o.y (fround v.y)
           o.z (fround v.z)))
  out-vec)

(declaim (inline v3round))
(defun* (v3round -> vec3) ((vec vec3))
  (v3round! (v3zero) vec))

(declaim (inline v3abs!))
(defun* (v3abs! -> vec3) ((out-vec vec3) (vec vec3))
  (with-vec3 ((o out-vec) (v vec))
    (psetf o.x (abs v.x)
           o.y (abs v.y)
           o.z (abs v.z)))
  out-vec)

(declaim (inline v3abs))
(defun* (v3abs -> vec3) ((vec vec3))
  (v3abs! (v3zero) vec))

(declaim (inline v3neg!))
(defun* (v3neg! -> vec3) ((out-vec vec3) (vec vec3))
  (v3scale! out-vec vec -1.0f0))

(declaim (inline v3neg))
(defun* (v3neg -> vec3) ((vec vec3))
  (v3neg! (v3zero) vec))

(declaim (inline v3cross!))
(defun* (v3cross! -> vec3) ((out-vec vec3) (vec-a vec3) (vec-b vec3))
  (with-vec3 ((o out-vec) (v1 vec-a) (v2 vec-b))
    (psetf o.x (- (* v1.y v2.z) (* v1.z v2.y))
           o.y (- (* v1.z v2.x) (* v1.x v2.z))
           o.z (- (* v1.x v2.y) (* v1.y v2.x))))
  out-vec)

(declaim (inline v3cross))
(defun* (v3cross -> vec3) ((vec-a vec3) (vec-b vec3))
  (v3cross! (v3zero) vec-a vec-b))

(declaim (inline v3box))
(defun* (v3box -> single-float) ((vec-a vec3) (vec-b vec3) (vec-c vec3))
  (v3dot (v3cross vec-a vec-b) vec-c))

(declaim (inline v3angle))
(defun* (v3angle -> single-float) ((vec-a vec3) (vec-b vec3))
  (let ((dot (v3dot vec-a vec-b))
        (m*m (* (v3mag vec-a) (v3mag vec-b))))
    (if (zerop m*m) 0.0f0 (acos (/ dot m*m)))))

(declaim (inline v3zerop))
(defun* (v3zerop -> boolean) ((vec vec3))
  (with-vec3 ((v vec))
    (and (zerop v.x)
         (zerop v.y)
         (zerop v.z))))

(declaim (inline v3dir=))
(defun* (v3dir= -> boolean) ((vec-a vec3) (vec-b vec3))
  (>= (v3dot (v3normalize vec-a) (v3normalize vec-b)) (- 1 +epsilon+)))

(declaim (inline v3parallelp))
(defun* (v3parallelp -> boolean) ((vec-a vec3) (vec-b vec3))
  (v3~ (v3cross vec-a vec-b) +v3zero+))

(declaim (inline v3lerp!))
(defun* (v3lerp! -> vec3) ((out-vec vec3) (vec-a vec3) (vec-b vec3)
                           (factor single-float))
  (with-vec3 ((o out-vec) (v1 vec-a) (v2 vec-b))
    (psetf o.x (lerp factor v1.x v2.x)
           o.y (lerp factor v1.y v2.y)
           o.z (lerp factor v1.z v2.z)))
  out-vec)

(declaim (inline v3lerp))
(defun* (v3lerp -> vec3) ((vec-a vec3) (vec-b vec3) (factor single-float))
  (v3lerp! (v3zero) vec-a vec-b factor))

(declaim (inline v3<))
(defun* (v3< -> boolean) ((vec-a vec3) (vec-b vec3))
  (with-vec3 ((v1 vec-a) (v2 vec-b))
    (and (< v1.x v2.x)
         (< v1.y v2.y)
         (< v1.z v2.z))))

(declaim (inline v3<=))
(defun* (v3<= -> boolean) ((vec-a vec3) (vec-b vec3))
  (with-vec3 ((v1 vec-a) (v2 vec-b))
    (and (<= v1.x v2.x)
         (<= v1.y v2.y)
         (<= v1.z v2.z))))

(declaim (inline v3>))
(defun* (v3> -> boolean) ((vec-a vec3) (vec-b vec3))
  (with-vec3 ((v1 vec-a) (v2 vec-b))
    (and (> v1.x v2.x)
         (> v1.y v2.y)
         (> v1.z v2.z))))

(declaim (inline v3>=))
(defun* (v3>= -> boolean) ((vec-a vec3) (vec-b vec3))
  (with-vec3 ((v1 vec-a) (v2 vec-b))
    (and (>= v1.x v2.x)
         (>= v1.y v2.y)
         (>= v1.z v2.z))))

(declaim (inline v3min!))
(defun* (v3min! -> vec3) ((out-vec vec3) (vec-a vec3) (vec-b vec3))
  (with-vec3 ((o out-vec) (v1 vec-a) (v2 vec-b))
    (psetf o.x (min v1.x v2.x)
           o.y (min v1.y v2.y)
           o.z (min v1.z v2.z)))
  out-vec)

(declaim (inline v3min))
(defun* (v3min -> vec3) ((vec-a vec3) (vec-b vec3))
  (v3min! (v3zero) vec-a vec-b))

(declaim (inline v3max!))
(defun* (v3max! -> vec3) ((out-vec vec3) (vec-a vec3) (vec-b vec3))
  (with-vec3 ((o out-vec) (v1 vec-a) (v2 vec-b))
    (psetf o.x (max v1.x v2.x)
           o.y (max v1.y v2.y)
           o.z (max v1.z v2.z)))
  out-vec)

(declaim (inline v3max))
(defun* (v3max -> vec3) ((vec-a vec3) (vec-b vec3))
  (v3max! (v3zero) vec-a vec-b))
