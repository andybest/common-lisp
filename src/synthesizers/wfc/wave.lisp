(in-package #:%syntex.synthesizers.wfc)

(defclass wave ()
  ((%possibilities :reader possibilities
                   :initarg :possibilities
                   :initform (make-array 0 :element-type 'bit))
   (%pattern-counts :reader pattern-counts
                    :initarg :pattern-counts
                    :initform (u:make-b32-array 0))
   (%pattern-count :reader pattern-count
                   :initarg :pattern-count
                   :initform 0)
   (%indices :reader indices
             :initform 0)))

(defun make-wave (pattern-count indices)
  (make-instance 'wave
                 :indices indices
                 :possibilities (make-array (* pattern-count indices)
                                            :element-type 'bit
                                            :initial-element 1)
                 :pattern-counts (make-array indices
                                             :element-type 'u:b32
                                             :initial-element pattern-count)
                 :pattern-count pattern-count))

(defgeneric get-possibility (wave index pattern)
  (:method ((wave wave) (index integer) (pattern integer))
    (plusp (sbit (possibilities wave) (+ (* index (pattern-count wave)) pattern)))))

(defgeneric get-pattern-count (wave index)
  (:method ((wave wave) (index integer))
    (aref (pattern-counts wave) index)))

(defgeneric insert-pattern (wave index pattern)
  (:method ((wave wave) (index integer) (pattern integer))
    (setf (sbit (possibilities wave) (+ (* index (pattern-count wave)) pattern)) 1)
    (incf (aref (pattern-counts wave) index))))

(defgeneric remove-pattern (wave index pattern)
  (:method ((wave wave) (index integer) (pattern integer))
    (setf (sbit (possibilities wave) (+ (* index (pattern-count wave)) pattern)) 0)
    (when (zerop (decf (aref (pattern-counts wave) index)))
      t)))
