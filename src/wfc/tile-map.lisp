(in-package #:%syntex.wfc.tile-map)

(declaim (inline %make-tile-map))
(defstruct (tile-map
            (:constructor %make-tile-map)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (grid nil :type grid:grid)
  (uncollapsed-count 0 :type u:non-negative-fixnum)
  (entropy-queue (pq:make-queue) :type pq:queue)
  (pattern-removal-stack nil :type list))

(declaim (inline %make-tile))
(defstruct (tile
            (:include grid:cell)
            (:constructor %make-tile (x y))
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (possible-patterns (u:make-bit-vector 0) :type simple-bit-vector)
  (total-weight 0 :type u:non-negative-fixnum)
  (total-weight-log-weight 0.0 :type u:f32)
  (entropy-noise 0.0 :type u:f32)
  (collapsed-p nil :type boolean)
  (enabler-counts (make-array (list 0 4) :element-type 'u:ub32) :type (u:ub32a (* 4))))

(u:fn-> make-tile-map (&key (:width u:ub16) (:height u:ub16)) tile-map)
(defun make-tile-map (&key width height)
  (declare (optimize speed))
  (let* ((tiles (make-array (* width height)))
         (grid (grid:make-grid width height tiles))
         (tile-map (%make-tile-map :grid grid :uncollapsed-count (* width height))))
    (dotimes (y height)
      (dotimes (x width)
        (setf (aref tiles (+ (* y width) x)) (%make-tile x y))))
    tile-map))

(u:fn-> calculate-initial-weights (core:core u:ub32) (values u:ub32 u:f32))
(defun calculate-initial-weights (core pattern-count)
  (let* ((frequencies (core:frequencies core))
         (total-weight 0)
         (total-weight-log-weight 0.0))
    (declare (u:ub16 total-weight)
             (u:f32 total-weight-log-weight))
    (dotimes (pattern-id pattern-count)
      (let ((frequency (aref frequencies pattern-id)))
        (declare (u:ub16 frequency))
        (incf total-weight frequency)
        (incf total-weight-log-weight (* frequency (log frequency 2)))))
    (values total-weight
            total-weight-log-weight)))

(u:fn-> make-tile-enabler-counts (core:core) (u:ub32a (* 4)))
(defun make-tile-enabler-counts (core)
  (declare (optimize speed))
  (let* ((adjacencies (core:adjacencies core))
         (pattern-count (pat:get-count (core:patterns core)))
         (enabler-counts (make-array (list pattern-count 4) :element-type 'u:ub32)))
    (dotimes (pattern-id pattern-count)
      (loop :for direction :in '(:left :right :up :down)
            :for i :from 0
            :for count = (list-length (u:href (svref adjacencies pattern-id) direction))
            :do (setf (aref enabler-counts pattern-id i) count)))
    enabler-counts))

(u:fn-> prepare (core:core) null)
(declaim (inline prepare))
(defun prepare (core)
  (declare (optimize speed))
  (u:mvlet* ((rng (core:rng core))
             (grid (grid (core:tile-map core)))
             (pattern-count (pat:get-count (core:patterns core)))
             (total-weight total-weight-log-weight (calculate-initial-weights core pattern-count)))
    (grid:do-cells (grid tile)
      (setf (possible-patterns tile) (u:make-bit-vector pattern-count 1)
            (total-weight tile) total-weight
            (total-weight-log-weight tile) total-weight-log-weight
            (entropy-noise tile) (rng:float rng 0.0 0.0001)
            (enabler-counts tile) (make-tile-enabler-counts core)))))

(u:fn-> compute-entropy (tile) u:f32)
(defun compute-entropy (tile)
  (declare (optimize speed))
  (let ((total-weight (total-weight tile))
        (total-weight-log-weight (total-weight-log-weight tile)))
    (+ (- (log total-weight 2)
          (/ total-weight-log-weight total-weight))
       (entropy-noise tile))))

(u:fn-> possible-pattern-p (tile u:ub32) boolean)
(declaim (inline possible-pattern-p))
(defun possible-pattern-p (tile pattern-id)
  (declare (optimize speed))
  (plusp (sbit (possible-patterns tile) pattern-id)))

(u:fn-> remove-possible-pattern (core:core tile u:ub32) null)
(defun remove-possible-pattern (core tile pattern-id)
  (let ((frequency (aref (core:frequencies core) pattern-id))
        (possible-patterns (possible-patterns tile)))
    (declare (u:non-negative-fixnum frequency))
    (setf (sbit possible-patterns pattern-id) 0)
    (when (every #'zerop possible-patterns)
      (error 'int:wfc-contradiction))
    (decf (total-weight tile) frequency)
    (decf (total-weight-log-weight tile) (* frequency (log frequency 2)))
    nil))

(u:fn-> choose-tile (core:core) tile)
(defun choose-tile (core)
  (declare (optimize speed))
  (let* ((tile-map (core:tile-map core))
         (entropy-queue (entropy-queue tile-map)))
    (u:while (pq:peek entropy-queue)
      (let ((tile (pq:dequeue entropy-queue)))
        (unless (collapsed-p tile)
          (return-from choose-tile tile))))
    (error "Bug: Entropy heap is empty but there are still uncollapsed tiles.")))

(u:fn-> choose-pattern-id (core:core tile) u:ub32)
(defun choose-pattern-id (core tile)
  (let* ((frequencies (core:frequencies core))
         (possible-patterns (possible-patterns tile))
         (remaining (rng:int (core:rng core) 0 (total-weight tile) nil)))
    (declare (fixnum remaining))
    (dotimes (pattern-id (length possible-patterns))
      (when (possible-pattern-p tile pattern-id)
        (let ((weight (aref frequencies pattern-id)))
          (declare (u:non-negative-fixnum weight))
          (if (>= remaining weight)
              (decf remaining weight)
              (return-from choose-pattern-id pattern-id)))))
    (error "Bug: Inconsistency detected with tile frequencies.")))

(u:fn-> collapse-tile (core:core tile) null)
(defun collapse-tile (core tile)
  (declare (optimize speed))
  (let* ((tile-map (core:tile-map core))
         (patterns (core:patterns core))
         (possible-patterns (possible-patterns tile))
         (chosen-pattern-id (choose-pattern-id core tile)))
    (setf (collapsed-p tile) t
          (grid:value tile) (pat:get-origin-color patterns chosen-pattern-id))
    (dotimes (pattern-id (length possible-patterns))
      (when (and (possible-pattern-p tile pattern-id)
                 (/= pattern-id chosen-pattern-id))
        (setf (sbit possible-patterns pattern-id) 0)
        (push (cons tile pattern-id) (pattern-removal-stack tile-map))))))

(u:fn-> get-neighbor (tile-map tile core:direction &key (:periodic-p boolean)) (or grid:cell null))
(declaim (inline get-neighbor))
(defun get-neighbor (tile-map tile direction &key periodic-p)
  (declare (optimize speed))
  (u:mvlet ((grid (grid tile-map))
            (x (grid:x tile))
            (y (grid:y tile))
            (ox oy (core:direction->offset direction)))
    (grid:get-cell grid (+ x ox) (+ y oy) :periodic-p periodic-p)))

(u:fn-> enabler-count (tile u:ub32 core:direction) u:ub16)
(declaim (inline enabler-count))
(defun enabler-count (tile pattern-id direction)
  (declare (optimize speed))
  (let ((direction-index (core:direction->index direction)))
    (aref (enabler-counts tile) pattern-id direction-index)))

(u:fn-> (setf enabler-count) (u:ub16 tile u:ub32 core:direction) u:ub16)
(declaim (inline (setf enabler-count)))
(defun (setf enabler-count) (value tile pattern-id direction)
  (let ((direction-index (core:direction->index direction)))
    (setf (aref (enabler-counts tile) pattern-id direction-index) value)))

(u:fn-> positive-enabler-counts-p (tile u:ub32) boolean)
(declaim (inline positive-enabler-counts-p))
(defun positive-enabler-counts-p (tile pattern-id)
  (declare (optimize speed))
  (and (plusp (enabler-count tile pattern-id :left))
       (plusp (enabler-count tile pattern-id :right))
       (plusp (enabler-count tile pattern-id :up))
       (plusp (enabler-count tile pattern-id :down))))
