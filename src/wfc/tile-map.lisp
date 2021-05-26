(in-package #:%syntex.wfc)

(u:eval-always
  (defclass tile (cell)
    ((%possible-patterns :accessor possible-patterns
                         :initarg :possible-patterns)
     (%weight :accessor weight
              :initarg :weight)
     (%weight-log-weight :accessor weight-log-weight
                         :initarg :weight-log-weight)
     (%entropy-noise :reader entropy-noise
                     :initarg :entropy-noise)
     (%collapsed-p :accessor collapsed-p
                   :initform nil))))

(u:eval-always
  (defclass tile-map (grid)
    ((%core :accessor core
            :initarg :core)
     (%entropy-queue :accessor entropy-queue
                     :initform (pq:make-queue))
     (%initial-weights :accessor initial-weights)
     (%neighbor-kernel :accessor neighbor-kernel)
     (%pattern-removal-stack :accessor pattern-removal-stack
                             :initform nil)
     (%uncollapsed-count :accessor uncollapsed-count))))

(u:define-printer (tile stream)
  (format stream "~d, ~d" (x tile) (y tile)))

(u:define-printer (tile-map stream)
  (format stream "~dx~d" (width tile-map) (height tile-map)))

(defmethod initialize-instance :after ((instance tile-map) &key core)
  (u:mvlet* ((tile-count (cell-count instance))
             (tiles (make-array tile-count))
             (kernel (make-kernel :grid instance :width 3 :height 3))
             (pattern-count (get-pattern-count core))
             (weight weight-log-weight (calculate-initial-weights core pattern-count)))
    (setf (cells instance) tiles
          (initial-weights instance) (cons weight weight-log-weight)
          (neighbor-kernel instance) kernel
          (uncollapsed-count instance) tile-count)))

(u:fn-> make-tile-map (core &key (:width u:ub16) (:height u:ub16)) tile-map)
(defun make-tile-map (core &key width height)
  (declare (optimize speed))
  (let* ((tile-map (make-instance 'tile-map :core core :width width :height height))
         (cells (cells tile-map)))
    (declare ((simple-array t) cells))
    (setf (tile-map core) tile-map)
    (dotimes (y height)
      (dotimes (x width)
        (setf (aref cells (+ (* y width) x)) (make-tile core x y))))
    tile-map))

(u:fn-> calculate-initial-weights (core u:ub32) (values u:ub32 u:f32))
(defun calculate-initial-weights (core pattern-count)
  (let ((weight 0)
        (weight-log-weight 0.0))
    (declare (u:ub32 weight)
             (u:f32 weight-log-weight))
    (dotimes (pattern-id pattern-count)
      (let ((frequency (get-frequency core pattern-id)))
        (declare (u:ub32 frequency))
        (incf weight frequency)
        (incf weight-log-weight (* frequency (log frequency 2)))))
    (values weight
            weight-log-weight)))

(u:fn-> make-tile (core u:ub16 u:ub16) tile)
(defun make-tile (core x y)
  (declare (optimize speed))
  (u:mvlet* ((initial-weights (initial-weights (tile-map core)))
             (pattern-count (get-pattern-count core)))
    (values
     (make-instance 'tile
                    :x x
                    :y y
                    :possible-patterns (u:make-bit-vector pattern-count 1)
                    :weight (car initial-weights)
                    :weight-log-weight (cdr initial-weights)
                    :entropy-noise (rng:float *rng* 0.0 0.0001)))))

(u:fn-> compute-entropy (tile) u:f32)
(defun compute-entropy (tile)
  (declare (optimize speed))
  (let ((weight (weight tile))
        (weight-log-weight (weight-log-weight tile))
        (noise (entropy-noise tile)))
    (declare (u:non-negative-fixnum weight)
             (u:f32 weight-log-weight noise))
    (+ (- (log weight 2)
          (/ weight-log-weight weight))
       noise)))

(u:fn-> possible-pattern-p (tile u:ub32) boolean)
(declaim (inline possible-pattern-p))
(defun possible-pattern-p (tile pattern-id)
  (declare (optimize speed))
  (plusp (sbit (possible-patterns tile) pattern-id)))

(defun ban-pattern (core tile pattern-id)
  (let ((possible-patterns (possible-patterns tile))
        (frequency (get-frequency core pattern-id)))
    (take-snapshot/ban-pattern core tile pattern-id)
    (setf (sbit possible-patterns pattern-id) 0)
    (when (every #'zerop possible-patterns)
      (return-from ban-pattern nil))
    (decf (weight tile) frequency)
    (decf (weight-log-weight tile) (* frequency (log frequency 2)))
    t))

(u:fn-> choose-tile (core) tile)
(defun choose-tile (core)
  (declare (optimize speed))
  (let* ((tile-map (tile-map core))
         (entropy-queue (entropy-queue tile-map)))
    (u:while (pq:peek entropy-queue)
      (let ((tile (pq:dequeue entropy-queue)))
        (unless (collapsed-p tile)
          (return-from choose-tile tile))))
    (error "Bug: Entropy heap is empty but there are still uncollapsed tiles.")))

(u:fn-> choose-pattern-id (core tile) u:ub32)
(defun choose-pattern-id (core tile)
  (declare (optimize speed))
  (let ((possible-patterns (possible-patterns tile))
        (remaining (rng:int *rng* 0 (weight tile) nil)))
    (declare (fixnum remaining)
             (simple-bit-vector possible-patterns))
    (dotimes (pattern-id (length possible-patterns))
      (when (possible-pattern-p tile pattern-id)
        (let ((weight (get-frequency core pattern-id)))
          (declare (u:non-negative-fixnum weight))
          (if (>= remaining weight)
              (decf remaining weight)
              (return-from choose-pattern-id pattern-id)))))
    (error "Bug: Inconsistency detected with tile frequencies.")))

(defun collapse-tile (core tile)
  (let ((tile-map (tile-map core))
        (possible-patterns (possible-patterns tile))
        (chosen-pattern-id (choose-pattern-id core tile))
        (original-possible-patterns nil))
    (dotimes (pattern-id (get-pattern-count core))
      (when (and (possible-pattern-p tile pattern-id)
                 (/= pattern-id chosen-pattern-id))
        (unless original-possible-patterns
          (setf original-possible-patterns (u:copy-array possible-patterns)))
        (setf (sbit possible-patterns pattern-id) 0)
        (push (cons tile pattern-id) (pattern-removal-stack tile-map))))
    (take-snapshot/collapse-tile core tile original-possible-patterns)
    (setf (collapsed-p tile) t
          (value tile) (get-origin-color core chosen-pattern-id))
    (decf (uncollapsed-count tile-map))
    nil))

(defun pattern-removable-p (core origin neighbor pattern-id direction)
  (let ((adjacencies (u:href (aref (adjacencies core) pattern-id) direction)))
    (when (possible-pattern-p neighbor pattern-id)
      (dolist (i adjacencies)
        (when (possible-pattern-p origin i)
          (return-from pattern-removable-p nil)))
      t)))
