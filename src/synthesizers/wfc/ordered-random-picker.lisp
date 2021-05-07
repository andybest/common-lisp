(in-package #:%syntex.synthesizers.wfc.ordered-random-picker)

(defclass picker (rp:picker)
  ((%pattern-count :reader pattern-count
                   :initarg :pattern-count)
   (%frequencies :reader frequencies
                 :initarg :frequencies)
   (%mask :reader mask
          :initarg :mask)
   (%indices :reader indices
             :initarg :indices)
   (%wave :reader wave
          :initarg :wave)))

(defun make-picker (wave frequencies mask)
  (make-instance 'picker
                 :pattern-count (length frequencies)
                 :frequencies frequencies
                 :mask mask
                 :indices (wave:indices wave)
                 :wave wave))

(defmethod rp:get-index ((picker picker) (func function))
  (let ((mask (mask picker))
        (wave (wave picker)))
    (dotimes (i (indices picker))
      (unless (or (and mask (plusp (aref mask i)))
                  (<= (wave:get-pattern-count wave i) 1))
        (return-from rp:get-index i)))
    -1))

(defmethod rp:get-pattern ((picker picker) (index integer) (func function))
  (let ((wave (wave picker))
        (pattern-count (pattern-count picker))
        (frequencies (frequencies picker))
        (s 0d0))
    (dotimes (pattern pattern-count)
      (when (wave:get wave index pattern)
        (incf s (aref frequencies pattern))))
    (let ((r (* (funcall func) s)))
      (dotimes (pattern pattern-count)
        (when (wave:get wave index pattern)
          (decf r (aref frequencies pattern)))
        (unless (plusp r)
          (return-from rp:get-pattern pattern)))
      (1- pattern-count))))
