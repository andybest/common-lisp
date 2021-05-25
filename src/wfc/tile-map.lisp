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
                   :initform nil)
     (%enabler-counts :accessor enabler-counts
                      :initarg :enabler-counts))))

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
     (%uncollapsed-count :accessor uncollapsed-count)
     (%snapshots :accessor snapshots
                 :initform nil))))

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

(u:fn-> make-tile-enabler-counts (core &optional u:ub32a) (u:ub32a (* 4)))
(defun make-tile-enabler-counts (core &optional enabler-counts)
  (declare (optimize speed))
  (let* ((adjacencies (adjacencies core))
         (pattern-count (get-pattern-count core))
         (enabler-counts (or enabler-counts
                             (make-array (list pattern-count 4)
                                         :element-type 'u:ub32
                                         :initial-element 0))))
    (declare ((simple-array t) adjacencies))
    (dotimes (pattern-id pattern-count)
      (loop :for direction :in '(:left :right :up :down)
            :for i :from 0
            :for count = (list-length (u:href (aref adjacencies pattern-id) direction))
            :do (setf (aref enabler-counts pattern-id i) count)))
    enabler-counts))

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
                    :entropy-noise (rng:float *rng* 0.0 0.0001)
                    :enabler-counts (make-tile-enabler-counts core)))))

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

(u:fn-> remove-possible-pattern (core tile u:ub32) boolean)
(defun remove-possible-pattern (core tile pattern-id)
  (declare (optimize speed))
  (let ((frequency (get-frequency core pattern-id))
        (possible-patterns (possible-patterns tile)))
    (declare (u:non-negative-fixnum frequency)
             (simple-bit-vector possible-patterns))
    (flet ((backtrack/restore-pattern (data)
             (setf (sbit (possible-patterns tile) pattern-id) data))
           (backtrack/restore-weights (data)
             (destructuring-bind (weight weight-log-weight) data
               (setf (weight tile) weight
                     (weight-log-weight tile) weight-log-weight))))
      (record core
              #'backtrack/restore-pattern
              (sbit possible-patterns pattern-id))
      (record core
              #'backtrack/restore-weights
              (list (weight tile) (weight-log-weight tile)))
      (setf (sbit possible-patterns pattern-id) 0)
      (when (every #'zerop possible-patterns)
        (return-from remove-possible-pattern nil))
      (decf (the u:non-negative-fixnum (weight tile)) frequency)
      (decf (the u:f32 (weight-log-weight tile)) (* frequency (log frequency 2))))
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

(u:fn-> collapse-tile (core tile) null)
(defun collapse-tile (core tile)
  (declare (optimize speed))
  (let* ((tile-map (tile-map core))
         (possible-patterns (possible-patterns tile))
         (chosen-pattern-id (choose-pattern-id core tile))
         (pushed-pattern-count 0)
         (original-possible-patterns nil))
    (declare (u:ub32 pushed-pattern-count)
             (simple-bit-vector possible-patterns))
    (flet ((backtrack/uncollapse-tile (data)
             (declare (ignore data))
             (setf (collapsed-p tile) nil
                   (value tile) 0))
           (backtrack/restore-possible-patterns (data)
             (declare (ignore data))
             (setf (possible-patterns tile) original-possible-patterns
                   (pattern-removal-stack tile-map) nil)))
      (record core #'backtrack/uncollapse-tile)
      (setf (collapsed-p tile) t
            (value tile) (get-origin-color core chosen-pattern-id))
      (dotimes (pattern-id (length possible-patterns))
        (when (and (possible-pattern-p tile pattern-id)
                   (/= pattern-id chosen-pattern-id))
          (unless original-possible-patterns
            (setf original-possible-patterns (u:copy-array possible-patterns)))
          (setf (sbit possible-patterns pattern-id) 0)
          (push (cons tile pattern-id) (pattern-removal-stack tile-map))
          (incf pushed-pattern-count)))
      (when (plusp pushed-pattern-count)
        (record core #'backtrack/restore-possible-patterns))
      (decf (the u:non-negative-fixnum (uncollapsed-count tile-map)))
      nil)))

(u:fn-> get-neighbor (tile-map tile direction &key (:periodic-p boolean)) (or cell null))
(declaim (inline get-neighbor))
(defun get-neighbor (tile-map tile direction &key periodic-p)
  (declare (optimize speed))
  (u:mvlet ((x (x tile))
            (y (y tile))
            (ox oy (direction->offset direction)))
    (declare (u:ub16 x y)
             (u:b16 ox oy))
    (get-cell tile-map (+ x ox) (+ y oy) :periodic-p periodic-p)))

(u:fn-> enabler-count (core tile u:ub32 direction) u:ub32)
(declaim (inline enabler-count))
(defun enabler-count (core tile pattern-id direction)
  (declare (optimize speed)
           (ignore core))
  (let ((direction-index (direction->index direction)))
    (aref (the u:ub32a (enabler-counts tile)) pattern-id direction-index)))

(u:fn-> (setf enabler-count) (u:ub32 core tile u:ub32 direction) u:ub32)
(declaim (inline (setf enabler-count)))
(defun (setf enabler-count) (value core tile pattern-id direction)
  (declare (optimize speed))
  (let ((enabler-counts (enabler-counts tile))
        (direction-index (direction->index direction)))
    (declare (u:ub32a enabler-counts))
    (flet ((backtrack/restore-enabler-count (count)
             (setf (aref enabler-counts pattern-id direction-index) count)))
      (record core
              #'backtrack/restore-enabler-count
              (aref enabler-counts pattern-id direction-index))
      (setf (aref enabler-counts pattern-id direction-index) value))))

(u:fn-> pattern-removable-p (core tile u:ub32 direction) boolean)
(declaim (inline pattern-removable-p))
(defun pattern-removable-p (core tile pattern-id direction)
  (declare (optimize speed))
  (and (possible-pattern-p tile pattern-id)
       (= (enabler-count core tile pattern-id direction) 1)
       (every (lambda (x)
                (plusp (enabler-count core tile pattern-id x)))
              (remove direction '(:left :right :up :down)))))
