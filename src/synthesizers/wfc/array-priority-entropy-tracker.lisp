(in-package #:%syntex.synthesizers.wfc.array-priority-entropy-tracker)

(defclass tracker (tr:tracker rp:picker)
  ((%frequency-sets :reader frequency-sets
                    :initarg :frequency-sets)
   (%entropy-values :reader entropy-values
                    :initarg :entropy-values)
   (%mask :reader mask
          :initarg :mask)
   (%indices :reader indices
             :initarg :indices)
   (%wave :reader wave
          :initarg :wave)))

(defclass entropy-values (etr:entropy-values)
  ((%priority-index :accessor priority-index
                    :initform 0)
   (%count :accessor count
           :initform 0)))

(defun decrement-entropy (entropy-values priority-index p plogp)
  (when (= priority-index (priority-index entropy-values))
    (decf (etr:plogp-sum entropy-values) plogp)
    (decf (etr:sum entropy-values) p)
    (decf (count entropy-values))
    (when (zerop (count entropy-values))
      (incf (count entropy-values))
      (return-from decrement-entropy t))
    (etr:recompute-entropy entropy-values)
    nil))

(defun increment-entropy (entropy-values priority-index p plogp)
  (cond
    ((= priority-index (priority-index entropy-values))
     (incf (etr:plogp-sum entropy-values) plogp)
     (incf (etr:sum entropy-values) p)
     (incf (count entropy-values))
     (etr:recompute-entropy entropy-values)
     nil)
    ((< priority-index (priority-index entropy-values))
     (setf (priority-index entropy-values) priority-index)
     t)))

(defun make-tracker (wave frequency-sets mask)
  (let* ((indices (wave:indices wave))
         (entropy-values (make-array indices)))
    (dotimes (i indices)
      (setf (aref entropy-values i) (make-instance 'entropy-values)))
    (make-instance 'tracker
                   :frequency-sets frequency-sets
                   :entropy-values entropy-values
                   :mask mask
                   :indices indices
                   :wave wave)))

(defun reset-priority (tracker index)
  (let* ((wave (wave tracker))
         (frequency-set (aref (frequency-sets tracker) index))
         (groups (fset:groups frequency-set))
         (v (aref (entropy-values tracker) index)))
    (setf (etr:plogp-sum v) 0d0
          (etr:sum v) 0d0
          (etr:entropy v) 0d0
          (count v) 0)
    (u:while (< (priority-index v) (length groups))
      (let ((g (aref groups (priority-index v))))
        (dotimes (i (fset:pattern-count g))
          (when (wave:get wave index (aref (fset:patterns g) i))
            (incf (etr:sum v) (aref (fset:frequencies g) i))
            (incf (etr:plogp-sum v) (aref (fset:plogp g) i))
            (incf (count v))))
        (cond
          ((zerop (count v))
           (incf (priority-index v)))
          (t
           (etr:recompute-entropy v)
           (return-from reset-priority nil)))))))

(defmethod tr:reset ((tracker tracker))
  (let ((frequency-sets (frequency-sets tracker))
        (entropy-array (entropy-values tracker)))
    (dotimes (i (indices tracker))
      (let ((entropy-values (aref entropy-array i)))
        (setf (priority-index entropy-values) 0
              (count entropy-values) 0
              (etr:plogp-sum entropy-values) 0d0
              (etr:sum entropy-values) 0d0
              (etr:entropy entropy-values) 0d0)
        (when (aref frequency-sets i)
          (reset-priority tracker i))))))

(defmethod tr:ban ((tracker tracker) (index integer) (pattern integer))
  (let ((frequency-set (aref (frequency-sets tracker) index))
        (entropy-values (aref (entropy-values tracker) index)))
    (when (decrement-entropy entropy-values
                             (aref (fset:priority-indices frequency-set) pattern)
                             (aref (fset:frequencies frequency-set) pattern)
                             (aref (fset:plogp frequency-set) pattern))
      (reset-priority tracker index))))

(defmethod tr:unban ((tracker tracker) (index integer) (pattern integer))
  (let ((frequency-set (aref (frequency-sets tracker) index))
        (entropy-values (aref (entropy-values tracker) index)))
    (when (increment-entropy entropy-values
                             (aref (fset:priority-indices frequency-set) pattern)
                             (aref (fset:frequencies frequency-set) pattern)
                             (aref (fset:plogp frequency-set) pattern))
      (reset-priority tracker index))))

(defmethod rp:get-index ((picker tracker) (func function) &optional external-priority)
  (when external-priority
    (error "Not implemented."))
  (let ((indices (indices picker))
        (mask (mask picker))
        (wave (wave picker))
        (entropy-array (entropy-values picker))
        (selected-index -1)
        (min-priority-index #.(1- (expt 2 32)))
        (min-entropy most-positive-double-float)
        (min-entropy-count 0))
    (dotimes (i indices)
      (unless (and mask (plusp (aref mask i)))
        (let* ((entropy-values (aref entropy-array i))
               (c (wave:get-pattern-count wave i))
               (p-i (priority-index entropy-values))
               (e (etr:entropy entropy-values)))
          (unless (<= c 1)
            (cond
              ((or (< p-i min-priority-index)
                   (and (= p-i min-priority-index)
                        (< e min-entropy)))
               (setf min-entropy-count 1
                     min-entropy e
                     min-priority-index p-i))
              ((and (= p-i min-priority-index)
                    (= e min-entropy))
               (incf min-entropy-count)))))))
    (let ((n (truncate (* min-entropy-count (funcall func)))))
      (dotimes (i indices)
        (unless (and mask (plusp (aref mask i)))
          (let* ((entropy-values (aref entropy-array i))
                 (c (wave:get-pattern-count wave i))
                 (p-i (priority-index entropy-values))
                 (e (etr:entropy entropy-values)))
            (when (and (> c 1)
                       (= p-i min-priority-index)
                       (= e min-entropy))
              (when (zerop n)
                (setf selected-index i)
                (return))
              (decf n)))))
      selected-index)))

(defmethod rp:get-pattern ((picker tracker) (index integer) (func function))
  (let* ((s 0d0)
         (wave (wave picker))
         (frequency-set (aref (frequency-sets picker) index))
         (entropy-values (aref (entropy-values picker) index))
         (priority-index (priority-index entropy-values))
         (group (aref (fset:groups frequency-set) priority-index))
         (pattern-count (fset:pattern-count group))
         (patterns (fset:patterns group))
         (frequencies (fset:frequencies group)))
    (dotimes (i pattern-count)
      (let ((pattern (aref patterns i)))
        (when (wave:get wave index pattern)
          (incf s (aref frequencies i)))))
    (let ((r (* (funcall func) s)))
      (dotimes (i pattern-count)
        (let ((pattern (aref patterns i)))
          (when (wave:get wave index pattern)
            (decf r (aref frequencies i)))
          (unless (plusp r)
            (return-from rp:get-pattern pattern))))
      (aref patterns (1- (length patterns))))))
