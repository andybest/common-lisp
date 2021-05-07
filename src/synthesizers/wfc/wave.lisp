(in-package #:%syntex.synthesizers.wfc.wave)

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

(defun get (wave index pattern)
  (plusp (sbit (possibilities wave) (+ (* index (pattern-count wave)) pattern))))

(defun get-pattern-count (wave index)
  (aref (pattern-counts wave) index))

(defun add-possibility (wave index pattern)
  (setf (sbit (possibilities wave) (+ (* index (pattern-count wave)) pattern)) 1)
  (incf (aref (pattern-counts wave) index)))

(defun remove-possibility (wave index pattern)
  (setf (sbit (possibilities wave) (+ (* index (pattern-count wave)) pattern)) 0)
  (when (zerop (decf (aref (pattern-counts wave) index)))
    t))
