(in-package #:%syntex.synthesizers.wfc.adjacent-model)

(defclass model (tm:model)
  ((%directions :accessor directions)
   (%tiles->patterns :reader tiles->patterns
                     :initform (u:dict #'eq))
   (%frequencies :reader frequencies
                 :initform (make-array 0 :fill-pointer 0 :adjustable t :element-type 'u:f64))
   (%propagator :reader propagator
                :initform (make-array 0 :fill-pointer 0 :adjustable t))))

(defclass adjacency ()
  ((%source :accessor source)
   (%target :accessor target)
   (%direction :accessor direction)))

(defmethod tm:tiles ((object model))
  (u:hash-keys (tiles->patterns object)))

(defgeneric make-model (input))

(defmethod make-model ((directions dir:direction-set))
  (let ((model (make-instance 'model)))
    (set-directions model directions)
    model))

(defmethod make-model ((sample top:data/tiles))
  (let ((model (make-instance 'model)))
    (%add-sample model sample)
    model))

(defmethod make-model ((sample top:data))
  (make-model (top.dat:to-tiles sample)))

(defun from-2d-array (sample &optional periodic-p)
  (let ((topology-data (top.dat:make-data-2d sample :periodic-p periodic-p)))
    (make-model topology-data)))

(defun set-directions (model directions)
  (let ((type (dir:type (directions model))))
    (unless (or (eq type :unknown)
                (eq type (dir:type directions)))
      (error "Cannot set directions to ~s; it has already been set to ~s."
             (dir:type directions)
             type))
    (setf (directions model) directions)))

(defun get-pattern (model tile)
  (let* ((direction-count (length (directions model)))
         (tiles->patterns (tiles->patterns model))
         (propagator (propagator model))
         (pattern (u:href tiles->patterns tile)))
    (or pattern
        (let ((pattern-count (hash-table-count tiles->patterns)))
          (setf (u:href tiles->patterns tile) pattern-count
                pattern pattern-count)
          (vector-push-extend 0 (frequencies model))
          (vector-push-extend (make-array direction-count) propagator)
          (dotimes (i direction-count)
            (setf (aref (aref propagator pattern) i) (u:dict #'eql)))
          pattern))))

(defun %set-frequency/transforms (model tile frequency transforms)
  (let ((transformed-tiles (tfm.tile:transform-all transforms tile))
        (frequencies (frequencies model)))
    (map nil
         (lambda (x)
           (let ((pattern (get-pattern model x)))
             (setf (aref frequencies pattern) 0d0)))
         transformed-tiles)
    (let ((incremental-frequency (/ frequency (length transformed-tiles))))
      (map nil
           (lambda (x)
             (let ((pattern (get-pattern model x)))
               (incf (aref frequencies pattern) incremental-frequency)))
           transformed-tiles))))

(defun set-frequency (model tile frequency &optional transforms)
  (flet ((%set-frequency (model tile frequency)
           (let ((pattern (get-pattern model tile)))
             (setf (aref (frequencies model) pattern) frequency))))
    (if transforms
        (%set-frequency/transforms model tile frequency transforms)
        (%set-frequency model tile frequency))))

(defun set-uniform-frequency (model)
  (dolist (tile (tm:tiles model))
    (set-frequency model tile 1d0)))

(defun require-directions (model)
  (when (eq (dir:type (directions model)) :unknown)
    (error "Directions must be set.")))

(defgeneric add-adjacency/transform (model source target direction/point &optional tile-transforms))

(defmethod add-adjacency/transform ((model model)
                                    (source vector)
                                    (target vector)
                                    (direction integer)
                                    &optional tile-transforms)
  (require-directions model)
  (let* ((directions (directions model))
         (x (aref (dir:x directions) direction))
         (y (aref (dir:y directions) direction))
         (z (aref (dir:z directions) direction))
         (point (point:point x y z)))
    (add-adjacency/transform model source target point tile-transforms)))

(defmethod add-adjacency/transform ((model model)
                                    (source vector)
                                    (target vector)
                                    (point point:point)
                                    &optional tile-transforms)
  (require-directions model)
  (let ((direction-type (dir:type (directions model)))
        (tile-transforms (or tile-transforms (tfm.tile:make-transforms))))
    (map nil
         (lambda (transform)
           (u:mvlet ((x y (top.dat:transform-vector direction-type
                                                    (point:x point)
                                                    (point:y point)
                                                    transform)))
             (add-adjacency/transform model
                                      (tfm.tile:transform-tiles tile-transforms source transform)
                                      (tfm.tile:transform-tiles tile-transforms target transform)
                                      (point:point x y (point:z point)))))
         (tfm.tile:group tile-transforms))))

(defgeneric add-adjacency (model source target direction/point))

(defmethod add-adjacency ((model model) (source vector) (target vector) (point point:point))
  (require-directions model)
  (let ((direction (dir:get (directions model) point)))
    (add-adjacency model source target direction)))

(defmethod add-adjacency ((model model)
                          (source vector)
                          (target vector)
                          (direction integer))
  (require-directions model)
  (map nil
       (lambda (x)
         (map nil
              (lambda (y)
                (add-adjacency model x y direction))
              target))
       source))

(defmethod add-adjacency ((model model) (source tile:tile) (target tile:tile) (point point:point))
  (require-directions model)
  (let ((direction (dir:get (directions model) point)))
    (add-adjacency model source target direction)))

(defmethod add-adjacency ((model model) (source tile:tile) (target tile:tile) (direction integer))
  (let* ((propagator (propagator model))
         (inverse-direction (dir:invert direction))
         (source-pattern (get-pattern model source))
         (target-pattern (get-pattern model target))
         (source-hash-set (aref (aref propagator source-pattern) direction))
         (target-hash-set (aref (aref propagator target-pattern) inverse-direction)))
    (setf (u:href source-hash-set target-pattern) target-pattern
          (u:href target-hash-set source-pattern) source-pattern)))

(defun add-adjacency-from-adjacency (model adjacency)
  (add-adjacency model
                 (source adjacency)
                 (target adjacency)
                 (direction adjacency)))

(defun adjacent-p (model source target direction)
  (let* ((propagator (propagator model))
         (source-pattern (get-pattern model source))
         (target-pattern (get-pattern model target))
         (source-hash-set (aref (aref propagator source-pattern) direction)))
    (when (u:href source-hash-set target-pattern)
      t)))

(defun %add-sample (model sample)
  (let ((topology (top:topology model))
        (frequencies (frequencies model))
        (propagator (propagator model)))
    (unless (typep topology 'grid:grid)
      (error "Expected a grid-based topology."))
    (let* ((directions (grid:directions topology))
           (direction-count (dir:count directions))
           (width (top:width topology))
           (height (top:height topology))
           (depth (top:depth topology)))
      (set-directions model directions)
      (dotimes (z depth)
        (dotimes (y height)
          (dotimes (x width)
            (let* ((point (point:point x y z))
                   (index (top:get-index topology point)))
              (when (top:contains-index-p topology index)
                (let ((pattern (get-pattern model (top:get sample point))))
                  (incf (aref frequencies pattern))
                  (dotimes (d direction-count)
                    (u:when-let* ((point2 (top:try-move topology point d))
                                  (pattern2 (get-pattern model (top:get sample point2))))
                      (vector-push-extend pattern2
                                          (aref (aref propagator pattern) d)))))))))))))

(defun add-sample (model sample &optional tile-transforms)
  (map nil
       (lambda (x)
         (%add-sample model x))
       (oa:get-transformed-samples sample tile-transforms)))

(defmethod tm:get-mapping ((model model) (topology grid:grid))
  (let ((frequencies (frequencies model)))
    (require-directions model)
    (set-directions model (grid:directions topology))
    (when (zerop (reduce #'+ frequencies))
      (error "No tiles have assigned frequencies."))
    (let ((pattern-model (make-instance 'pm:pattern-model
                                        :propagator (propagator model)
                                        :frequencies frequencies))
          (t->pbo (u:dict #'eql))
          (p->tbo (u:dict #'eql))
          (pbo (u:dict #'eq))
          (tbo (u:dict #'eql)))
      (u:do-hash (k v (tiles->patterns model))
        (setf (u:href pbo k) (u:dict #'eql v v)
              (u:href tbo v) k))
      (setf (u:href t->pbo 0) pbo
            (u:href p->tbo 0) tbo)
      (make-instance 'tmm:mapping
                     :pattern-topology topology
                     :pattern-model pattern-model
                     :patterns->tiles-by-offset t->pbo
                     :tiles->patterns-by-offset p->tbo
                     :tile-coord->pattern-coord-index/offset nil))))

(defmethod tm:multiply-frequency ((model model) (tile tile:tile) (multiplier float))
  (let ((pattern (u:href (tiles->patterns model) tile)))
    (incf (aref (frequencies model) pattern) multiplier)))
