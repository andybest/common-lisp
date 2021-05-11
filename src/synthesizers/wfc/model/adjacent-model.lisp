(in-package #:%syntex.synthesizers.wfc.model)

(defclass adjacent-model (tile-model)
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

(defmethod tiles ((object adjacent-model))
  (u:hash-keys (tiles->patterns object)))

(defgeneric make-adjacent-model (input))

(defmethod make-adjacent-model ((directions top:direction-set))
  (let ((model (make-instance 'adjacent-model)))
    (set-directions model directions)
    model))

(defmethod make-adjacent-model ((sample top:data/tiles))
  (let ((model (make-instance 'adjacent-model)))
    (%add-sample model sample)
    model))

(defmethod make-adjacent-model ((sample top:data))
  (make-adjacent-model (top:to-tiles sample)))

(defun from-2d-array (sample &optional periodic-p)
  (let ((topology-data (top:make-data-2d sample :periodic-p periodic-p)))
    (make-adjacent-model topology-data)))

(defun set-directions (model directions)
  (let ((type (top:direction-type (directions model))))
    (unless (or (eq type :unknown)
                (eq type (top:direction-type directions)))
      (error "Cannot set directions to ~s; it has already been set to ~s."
             (top:direction-type directions)
             type))
    (setf (directions model) directions)))

(defmethod get-pattern ((model adjacent-model) (tile base:tile))
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

(defun set-frequency/transform (model tile frequency tile-transform)
  (let ((transformed-tiles (tfm:transform-all tile-transform tile))
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

(defun %set-frequency (model tile frequency)
  (let ((pattern (get-pattern model tile)))
    (setf (aref (frequencies model) pattern) frequency)))

(defmethod set-frequency ((model adjacent-model)
                          (tile base:tile)
                          (frequency float)
                          &optional tile-transform)
  (if tile-transform
      (set-frequency/transform model tile frequency tile-transform)
      (%set-frequency model tile frequency)))

(defun set-uniform-frequency (model)
  (dolist (tile (tiles model))
    (set-frequency model tile 1d0)))

(defun require-directions (model)
  (when (eq (top:direction-type (directions model)) :unknown)
    (error "Directions must be set.")))

(defgeneric add-adjacency/transform (model source target direction/point &optional tile-transform))

(defmethod add-adjacency/transform ((model adjacent-model)
                                    (source vector)
                                    (target vector)
                                    (direction integer)
                                    &optional tile-transform)
  (require-directions model)
  (let* ((directions (directions model))
         (x (aref (top:direction-x directions) direction))
         (y (aref (top:direction-y directions) direction))
         (z (aref (top:direction-z directions) direction))
         (point (base:make-point x y z)))
    (add-adjacency/transform model source target point tile-transform)))

(defmethod add-adjacency/transform ((model adjacent-model)
                                    (source vector)
                                    (target vector)
                                    (point base:point)
                                    &optional tile-transform)
  (require-directions model)
  (let ((direction-type (top:direction-type (directions model)))
        (tile-transform (or tile-transform (tfm:make-tile-transform))))
    (map nil
         (lambda (transform)
           (u:mvlet ((x y (top:transform-vector direction-type
                                                (base:point-x point)
                                                (base:point-y point)
                                                transform)))
             (add-adjacency/transform model
                                      (tfm:transform-tiles tile-transform source transform)
                                      (tfm:transform-tiles tile-transform target transform)
                                      (base:make-point x y (base:point-z point)))))
         (tfm:group tile-transform))))

(defgeneric add-adjacency (model source target direction/point))

(defmethod add-adjacency ((model adjacent-model) (source vector) (target vector) (point base:point))
  (require-directions model)
  (let ((direction (top:get-direction (directions model) point)))
    (add-adjacency model source target direction)))

(defmethod add-adjacency ((model adjacent-model)
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

(defmethod add-adjacency ((model adjacent-model)
                          (source base:tile)
                          (target base:tile)
                          (point base:point))
  (require-directions model)
  (let ((direction (top:get-direction (directions model) point)))
    (add-adjacency model source target direction)))

(defmethod add-adjacency ((model adjacent-model)
                          (source base:tile)
                          (target base:tile)
                          (direction integer))
  (let* ((propagator (propagator model))
         (inverse-direction (top:invert-direction direction))
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

(defgeneric adjacent-p (model source target direction))

(defmethod adjacent-p ((model adjacent-model) source target direction)
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
    (unless (typep topology 'top:grid)
      (error "Expected a grid-based topology."))
    (let* ((directions (top:directions topology))
           (direction-count (top:direction-count directions))
           (width (top:width topology))
           (height (top:height topology))
           (depth (top:depth topology)))
      (set-directions model directions)
      (dotimes (z depth)
        (dotimes (y height)
          (dotimes (x width)
            (let* ((point (base:make-point x y z))
                   (index (top:get-index topology point)))
              (when (top:contains-index-p topology index)
                (let ((pattern (get-pattern model (top:get sample point))))
                  (incf (aref frequencies pattern))
                  (dotimes (d direction-count)
                    (u:when-let* ((point2 (top:try-move topology point d))
                                  (pattern2 (get-pattern model (top:get sample point2))))
                      (vector-push-extend pattern2
                                          (aref (aref propagator pattern) d)))))))))))))

(defmethod add-sample ((model adjacent-model) sample &optional tile-transform)
  (map nil
       (lambda (x)
         (%add-sample model x))
       (get-transformed-samples sample tile-transform)))

(defmethod get-mapping ((model adjacent-model) (topology top:grid))
  (let ((frequencies (frequencies model)))
    (require-directions model)
    (set-directions model (top:directions topology))
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
      (make-instance 'tile-model-mapping
                     :pattern-topology topology
                     :pattern-model pattern-model
                     :patterns->tiles-by-offset p->tbo
                     :tiles->patterns-by-offset t->pbo
                     :tile-coord->pattern-coord-index/offset nil))))

(defmethod multiply-frequency ((model adjacent-model) (tile base:tile) (multiplier float))
  (let ((pattern (u:href (tiles->patterns model) tile)))
    (incf (aref (frequencies model) pattern) multiplier)))
