(in-package #:%syntex.wfc.tile-map)

(defstruct (tile-map
            (:include grid:grid)
            (:constructor %make-tile-map)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (uncollapsed-count 0 :type u:non-negative-fixnum))

(defstruct (tile
            (:include grid:cell)
            (:constructor %make-tile)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (possible-patterns (u:make-bit-vector 0) :type simple-bit-vector)
  (total-weight 0 :type u:non-negative-fixnum)
  (total-weight-log-weight 0.0 :type u:f32)
  (entropy-noise 0.0 :type u:f32)
  (collapsed-p nil :type boolean))

(defun make-tile-map (&key width height)
  (let* ((tiles (make-array (* width height)))
         (tile-map (%make-tile-map :width width
                                   :height height
                                   :cells tiles
                                   :uncollapsed-count (* width height))))
    (dotimes (y height)
      (dotimes (x width)
        (setf (aref tiles (+ (* y width) x)) (%make-tile :x x :y y))))
    tile-map))

(defun calculate-initial-weights (core pattern-count)
  (let* ((frequencies (core:frequencies core))
         (total-weight 0)
         (total-weight-log-weight 0.0))
    (dotimes (pattern-id pattern-count)
      (let ((frequency (aref frequencies pattern-id)))
        (incf total-weight frequency)
        (incf total-weight-log-weight (* frequency (log frequency 2)))))
    (values total-weight
            total-weight-log-weight)))

(defun prepare (core)
  (u:mvlet* ((rng (core:rng core))
             (tile-map (core:tile-map core))
             (pattern-count (pat:get-count (core:patterns core)))
             (total-weight total-weight-log-weight (calculate-initial-weights core pattern-count)))
    (grid:do-cells (tile-map tile)
      (setf (possible-patterns tile) (u:make-bit-vector pattern-count 1)
            (total-weight tile) total-weight
            (total-weight-log-weight tile) total-weight-log-weight
            (entropy-noise tile) (rng:float rng 0.0 0.0001)))))

(defun compute-entropy (tile)
  (let ((total-weight-log-weight (total-weight-log-weight tile)))
    (+ (- (log total-weight-log-weight 2)
          (/ total-weight-log-weight (total-weight tile)))
       (entropy-noise tile))))

(defun remove-tile (core tile pattern-id)
  (let ((frequency (aref (core:frequencies core) pattern-id)))
    (setf (sbit (possible-patterns tile) pattern-id) 0)
    (decf (total-weight tile) frequency)
    (decf (total-weight-log-weight tile) (* frequency (log frequency 2)))))
