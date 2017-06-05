(in-package :gamebox-math)

(deftype dquat () '(simple-array quat (2)))

(defstruct (dquat (:constructor dquat (&optional real dual))
                  (:conc-name dq-)
                  (:copier nil)
                  (:predicate nil))
  (real (quat 1 0 0 0) :type quat)
  (dual (quat 0 0 0 0) :type quat))

(defmacro with-dquat ((prefix dquat) &body body)
  `(with-quats ((,prefix ,dquat)
                (,(symbolicate prefix 'r) (dq-real ,dquat))
                (,(symbolicate prefix 'd) (dq-dual ,dquat)))
     ,@body))

(defmacro with-dquats (binds &body body)
  (if (null binds)
      `(progn ,@body)
      `(with-dquat ,(car binds)
         (with-dquats ,(cdr binds) ,@body))))
