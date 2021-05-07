(in-package #:%syntex.synthesizers.wfc.frequency-set)

(defclass frequency-set ()
  ((%priority-indices :reader priority-indices
                      :initarg :priority-indices)
   (%frequencies :reader frequencies
                 :initarg :frequencies)
   (%plogp :reader plogp
           :initarg :plogp)
   (%groups :reader groups
            :initarg :groups)))

(defclass group ()
  ((%priority :reader priority
              :initarg :priority)
   (%pattern-count :accessor pattern-count
                   :initarg :pattern-count)
   (%weight-sum :accessor weight-sum
                :initarg :weight-sum)
   (%patterns :reader patterns
              :initform (make-array 0
                                    :fill-pointer 0
                                    :adjustable t
                                    :element-type 'fixnum
                                    :initial-element 0))
   (%frequencies :reader frequencies
                 :initform (make-array 0
                                       :fill-pointer 0
                                       :adjustable t
                                       :element-type 'u:f64
                                       :initial-element 0d0))
   (%plogp :reader plogp
           :initform (make-array 0
                                 :fill-pointer 0
                                 :adjustable t
                                 :element-type 'u:f64
                                 :initial-element 0d0))))

(defun to-plogp (frequency)
  (if (plusp frequency)
      (* frequency (log frequency))
      0d0))

(defun make-set (weights &optional priorities)
  (let* ((weight-count (length weights))
         (priorities (or priorities (make-array weight-count :initial-element 0)))
         (groups-by-priority (u:dict #'eql))
         (frequencies (make-array weight-count :element-type 'u:f64))
         (plogp (make-array weight-count :element-type 'u:f64)))
    (dotimes (i weight-count)
      (let* ((priority (aref priorities i))
             (weight (aref weights i))
             (group (or (u:href groups-by-priority priority)
                        (make-instance 'group :priority priority))))
        (incf (pattern-count group))
        (incf (weight-sum group) weight)
        (vector-push-extend i (patterns group))
        (setf (u:href groups-by-priority priority) group)))
    (dotimes (i weight-count)
      (let* ((group (u:href groups-by-priority (aref priorities i)))
             (frequency (/ (aref weights i) (weight-sum group)))
             (p (to-plogp frequency)))
        (setf (aref frequencies i) frequency
              (aref plogp i) p)
        (vector-push-extend frequency (frequencies group))
        (vector-push-extend p (plogp group))))
    (let* ((group-priorities (sort (u:hash-keys groups-by-priority) #'<))
           (groups (make-array (length group-priorities)))
           (priority->index (u:dict #'eql))
           (priority-indices (make-array (length priorities))))
      (loop :for priority :in group-priorities
            :for i :from 0
            :do (setf (aref groups i) (u:href groups-by-priority priority)
                      (u:href priority->index priority) i))
      (loop :for priority :across priorities
            :for i :from 0
            :do (setf (aref priority-indices i) (u:href priority->index priority)))
      (make-instance 'frequency-set
                     :priority-indices priority-indices
                     :frequencies frequencies
                     :plogp plogp
                     :groups groups))))
