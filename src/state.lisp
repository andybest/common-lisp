(in-package :dungen)

(defvar *state*)

(defclass state ()
  ((%rng :reader rng)
   (%current-region :accessor current-region
                    :initform 0)
   (%regions :reader regions
             :initform (au:dict #'eql))
   (%connections :reader connections
                 :initform (au:dict #'equal))
   (%dead-ends :accessor dead-ends
               :initform nil)))

(defmethod initialize-instance :after ((instance state) &key seed)
  (setf (slot-value instance '%rng) (pcg:make-pcg :seed seed)))
