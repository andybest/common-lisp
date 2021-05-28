(in-package #:%syntex.wfc)

(u:eval-always
  (defclass backtracker ()
    ((%timestamp :accessor timestamp
                 :initform 0)
     (%last-progress :accessor last-progress
                     :initform 0)
     (%snapshots :reader snapshots
                 :initform (make-array 32 :fill-pointer 0 :adjustable t :initial-element nil))
     (distance :reader distance
               :initarg :distance
               :initform 1)
     (%max-retries :reader max-retries
                   :initarg :max-retries
                   :initform 10)
     (%try :accessor try
           :initform 0))))

(defun make-backtracker (&key distance retries)
  (let ((backtracker (make-instance 'backtracker :distance distance :max-retries retries)))
    (vector-push-extend 0 (snapshots backtracker))
    backtracker))

(u:fn-> take-snapshot (core function) null)
(declaim (inline take-snapshot))
(defun take-snapshot (core func)
  (declare (optimize speed))
  (when (eq (strategy core) :backtrack)
    (vector-push-extend func (snapshots (backtracker core))))
  nil)

(u:fn-> take-snapshot/modify-tile (core tile simple-bit-vector) null)
(defun take-snapshot/modify-tile (core tile possible-patterns)
  (declare (optimize speed))
  (flet ((%restore ()
           (let ((weight 0)
                 (weight-log-weight 0.0))
             (declare (u:ub32 weight)
                      (u:f32 weight-log-weight))
             (setf (possible-patterns tile) possible-patterns)
             (dotimes (i (length possible-patterns))
               (let ((frequency (get-frequency core i)))
                 (declare (u:ub32 frequency))
                 (when (possible-pattern-p tile i)
                   (incf weight frequency)
                   (incf weight-log-weight (* frequency (log frequency 2))))))
             (setf (weight tile) weight
                   (weight-log-weight tile) weight-log-weight))))
    (take-snapshot core #'%restore)
    nil))

(u:fn-> take-snapshot/collapse-tile (core tile (or simple-bit-vector null)) null)
(defun take-snapshot/collapse-tile (core tile possible-patterns)
  (declare (optimize speed))
  (flet ((%uncollapse ()
           (setf (collapsed-p tile) nil
                 (value tile) #xff00ffff)
           (when possible-patterns
             (setf (possible-patterns tile) possible-patterns
                   (pattern-removal-stack (tile-map core)) nil))))
    (take-snapshot core #'%uncollapse)
    nil))

(u:fn-> advance-time (core) null)
(declaim (inline advance-time))
(defun advance-time (core)
  (declare (optimize speed))
  (let* ((backtracker (backtracker core))
         (snapshots (snapshots backtracker)))
    (incf (the u:non-negative-fixnum (timestamp backtracker)))
    (vector-push-extend (timestamp backtracker) snapshots)
    nil))

(u:fn-> backtrack (core) null)
(defun backtrack (core)
  (declare (optimize speed))
  (let* ((backtracker (backtracker core))
         (tile-map (tile-map core))
         (timestamp (timestamp backtracker))
         (snapshots (snapshots backtracker))
         (target-timestamp (max 0 (- timestamp (the u:non-negative-fixnum (distance backtracker)))))
         (progress (progress core))
         (stop-p nil))
    (declare (u:non-negative-fixnum timestamp target-timestamp)
             (vector snapshots)
             ((integer 0 100) progress))
    (u:until (or stop-p (zerop (length snapshots)))
      (let ((item (vector-pop snapshots)))
        (etypecase item
          (function
           (funcall item))
          (u:non-negative-fixnum
           (incf (the u:non-negative-fixnum (uncollapsed-count tile-map)))
           (setf (timestamp backtracker) item
                 stop-p (< item target-timestamp))))))
    (if (> progress (the (integer 0 100) (last-progress backtracker)))
        (setf (try backtracker) 0)
        (incf (the u:non-negative-fixnum (try backtracker))))
    (when (>= (the u:non-negative-fixnum (try backtracker))
              (the u:non-negative-fixnum (max-retries backtracker)))
      (error 'cond:wfc-max-backtrack-retries-exceeded :value (try backtracker)))
    (setf (last-progress backtracker) progress)
    nil))
