(in-package #:%syntex.wfc.tile-map)

(defstruct (tile-data
            (:constructor %make-tile-data)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (possible-patterns (u:make-bit-vector 0) :type simple-bit-vector)
  (total-weight 0 :type u:non-negative-fixnum)
  (total-weight-log-weight 0.0 :type u:f32)
  (entropy-noise 0.0 :type u:f32)
  (collapsed-p nil :type boolean))

(defun make-tile-data (core &key pattern-count total-weight total-weight-log-weight)
  (u:mvlet* ((entropy-noise (rng:float (core:rng core) 0.0 0.0001))
             (possible-patterns (u:make-bit-vector pattern-count 1)))
    (%make-tile-data :possible-patterns possible-patterns
                     :entropy-noise entropy-noise
                     :total-weight total-weight
                     :total-weight-log-weight total-weight-log-weight)))

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
  (u:mvlet* ((tile-map (core:tile-map core))
             (pattern-count (pat:get-count (core:patterns core)))
             (total-weight total-weight-log-weight (calculate-initial-weights core pattern-count)))
    (grid:do-cells (tile-map tile)
      (let ((data (make-tile-data core
                                  :pattern-count pattern-count
                                  :total-weight total-weight
                                  :total-weight-log-weight total-weight-log-weight)))
        (setf (grid:value tile) data)))))

(defun compute-entropy (tile)
  (let* ((tile-data (grid:value tile))
         (total-weight-log-weight (total-weight-log-weight tile-data)))
    (+ (- (log total-weight-log-weight 2)
          (/ total-weight-log-weight (total-weight tile-data)))
       (entropy-noise tile-data))))

(defun remove-tile (core tile pattern-id)
  (let ((tile-data (grid:value tile))
        (frequency (aref (core:frequencies core) pattern-id)))
    (setf (sbit (possible-patterns tile-data) pattern-id) 0)
    (decf (total-weight tile-data) frequency)
    (decf (total-weight-log-weight tile-data) (* frequency (log frequency 2)))))
