(in-package #:%syntex.wfc)

(u:eval-always
  (defclass history ()
    ((%timestamp :accessor timestamp
                 :initform 0)
     (%last-progress :accessor last-progress
                     :initform 0)
     (%entries :reader entries
               :initform (make-array 32 :fill-pointer 0 :adjustable t :initial-element nil))
     (distance :reader distance
               :initarg :distance
               :initform 1)
     (%max-retries :reader max-retries
                   :initarg :max-retries
                   :initform 10)
     (%try :accessor try
           :initform 0))))

(defun make-history (&key distance retries)
  (let ((history (make-instance 'history :distance distance :max-retries retries)))
    (vector-push-extend 0 (entries history))
    history))

(u:fn-> advance-time (core) null)
(declaim (inline advance-time))
(defun advance-time (core)
  (declare (optimize speed))
  (let* ((history (history core))
         (entries (entries history)))
    (incf (the u:non-negative-fixnum (timestamp history)))
    (vector-push-extend (timestamp history) entries)
    nil))

(u:fn-> record (core function &optional t) null)
(declaim (inline record))
(defun record (core func &optional arg)
  (declare (optimize speed))
  (when (eq (strategy core) :backtrack)
    (vector-push-extend (cons func arg) (entries (history core))))
  nil)

(u:fn-> backtrack (core) null)
(defun backtrack (core)
  (declare (optimize speed))
  (let* ((history (history core))
         (tile-map (tile-map core))
         (timestamp (timestamp history))
         (entries (entries history))
         (target-timestamp (max 0 (- timestamp (the u:non-negative-fixnum (distance history)))))
         (progress (progress core))
         (stop-p nil))
    (declare (u:non-negative-fixnum timestamp target-timestamp)
             (vector entries)
             ((integer 0 100) progress))
    (u:until (or stop-p (zerop (length entries)))
      (let ((item (vector-pop entries)))
        (etypecase item
          (cons
           (destructuring-bind (func . arg) item
             (declare (function func))
             (funcall func arg)))
          (u:non-negative-fixnum
           (incf (the u:non-negative-fixnum (uncollapsed-count tile-map)))
           (setf (timestamp history) item
                 stop-p (< item target-timestamp))))))
    (if (> progress (the (integer 0 100) (last-progress history)))
        (setf (try history) 0)
        (incf (the u:non-negative-fixnum (try history))))
    (when (>= (the u:non-negative-fixnum (try history))
              (the u:non-negative-fixnum (max-retries history)))
      (error 'cond:wfc-max-backtrack-retries-exceeded :value (try history)))
    (setf (last-progress history) progress)
    nil))
