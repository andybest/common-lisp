(in-package :gamebox-math)

(deftype dquat () '(simple-array quat (2)))

(defstruct (dquat (:type vector)
                  (:constructor dquat (&optional real dual))
                  (:conc-name dq-)
                  (:copier nil)
                  (:predicate nil))
  (real (quat) :type quat)
  (dual (quat) :type quat))

(defmacro with-dquat ((prefix dquat) &body body)
  "A convenience macro for concisely accessing components of a dual quaternion.
Example: (with-dquat (d dquat) (values drw ddw)) would allow accessing the W
components of the real and dual parts of a dual quaternion as simply the symbols
DRW and DDW."
  `(with-quats ((,prefix ,dquat)
                (,(symbolicate prefix 'r) (dq-real ,dquat))
                (,(symbolicate prefix 'd) (dq-dual ,dquat)))
     ,@body))

(defmacro with-dquats (binds &body body)
  "A convenience macro for concisely accessing components of multiple dual
quaternions.
Example: (with-dquats ((a dquat1) (b dquat2)) (values arw brw)) would access the
W component of dquat1, and the W component of dquaternion."
  (if (null binds)
      `(progn ,@body)
      `(with-dquat ,(car binds)
         (with-dquats ,(cdr binds) ,@body))))
