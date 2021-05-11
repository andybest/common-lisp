(in-package #:%syntex.synthesizers.wfc.model)

(defclass graph-adjacent-model (tile-model)
  ((%directions-count :reader directions-count
                      :initarg :directions-count)
   (%edge-label-count :reader edge-label-count
                      :initarg :edge-label-count)
   (%tiles->patterns :reader tiles->patterns
                     :initform (u:dict #'eq))
   (%frequencies :reader frequencies
                 :initform (make-array 0 :fill-pointer 0 :adjustable t :element-type 'u:f64))
   (%propagator :reader propagator
                :initform (make-array 0 :fill-pointer 0 :adjustable t))
   (%edge-label-info :reader edge-label-info
                     :initform (make-array 0 :fill-pointer 0 :adjustable t))))

(defmethod tiles ((object graph-adjacent-model))
  (u:hash-keys (tiles->patterns object)))

(defun make-graph-adjacent-model (directions-count edge-label-count)
  (make-instance 'graph-adjacent-model
                 :directions-count directions-count
                 :edge-label-count edge-label-count))

(defmethod get-mapping ((model graph-adjacent-model) (topology top:topology))
  (let ((frequencies (frequencies model)))
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
                     :patterns->tiles-by-offset t->pbo
                     :tiles->patterns-by-offset p->tbo
                     :tile-coord->pattern-coord-index/offset nil))))

(defmethod multiply-frequency ((model graph-adjacent-model) (tile base:tile) (multiplier float))
  (let ((pattern (u:href (tiles->patterns model) tile)))
    (incf (aref (frequencies model) pattern) multiplier)))

(defmethod get-pattern ((model graph-adjacent-model) (tile base:tile))
  (let* ((edge-label-count (edge-label-count model))
         (tiles->patterns (tiles->patterns model))
         (propagator (propagator model))
         (pattern (u:href tiles->patterns tile)))
    (or pattern
        (let ((pattern-count (hash-table-count tiles->patterns)))
          (setf (u:href tiles->patterns tile) pattern-count
                pattern pattern-count)
          (vector-push-extend 0 (frequencies model))
          (vector-push-extend (make-array edge-label-count) propagator)
          (dotimes (i edge-label-count)
            (setf (aref (aref propagator pattern) i) (u:dict #'eql)))
          pattern))))

(defmethod set-frequency ((model graph-adjacent-model)
                          (tile base:tile)
                          (frequency float)
                          &optional tile-transform)
  (declare (ignore tile-transform))
  (%set-frequency model tile frequency))

(defmethod add-adjacency/transform ((model graph-adjacent-model)
                                    (source vector)
                                    (target vector)
                                    (direction integer)
                                    &optional tile-transform)
  (map nil
       (lambda (x)
         (map nil
              (lambda (y)
                (add-adjacency/transform model x y direction tile-transform))
              target))
       source))

(defmethod add-adjacency/transform ((model graph-adjacent-model)
                                    (source base:tile)
                                    (target base:tile)
                                    (direction integer)
                                    &optional tile-transform)
  (let ((edge-label-info (edge-label-info model)))
    (when (zerop (length edge-label-info))
      (error "Requires edge label info configured."))
    (let ((inverse-direction-items (make-array 0 :fill-pointer 0 :adjustable t)))
      (map nil
           (lambda (x)
             (destructuring-bind (i1 i2 i3) x
               (declare (ignore i2))
               (when (and (tfm:identity-p i3)
                          (= i1 direction))
                 (vector-push-extend x inverse-direction-items))))
           edge-label-info)
      (when (zerop (length inverse-direction-items))
        (error "Couldn't find identity edge label for direction: ~s." direction))
      (let ((inverse-direction (cadr (aref inverse-direction-items 0))))
        (dotimes (i (length edge-label-info))
          (destructuring-bind (dir idir tfm) (aref edge-label-info i)
            (declare (ignore idir))
            (when (= dir direction)
              (u:mvlet* ((transform (tfm:invert tfm))
                         (rd success-p (tfm:transform-tile tile-transform target transform)))
                (when success-p
                  (add-adjacency model source rd i))))
            (when (= dir inverse-direction)
              (u:mvlet* ((transform (tfm:invert tfm))
                         (rs success-p (tfm:transform-tile tile-transform source transform)))
                (when success-p
                  (add-adjacency model target rs i))))))))))

(defmethod add-adjacency ((model graph-adjacent-model)
                          (source vector)
                          (target vector)
                          (edge-label integer))
  (map nil
       (lambda (x)
         (map nil
              (lambda (y)
                (add-adjacency model x y edge-label))
              target))
       source))

(defmethod add-adjacency ((model graph-adjacent-model)
                          (source base:tile)
                          (target base:tile)
                          (edge-label integer))
  (let* ((propagator (propagator model))
         (source (get-pattern model source))
         (target (get-pattern model target))
         (hash-set (aref (aref propagator source) edge-label)))
    (setf (u:href hash-set target) target)))

(defmethod adjacent-p ((model graph-adjacent-model) source target edge-label)
  (let* ((propagator (propagator model))
         (source-pattern (get-pattern model source))
         (target-pattern (get-pattern model target))
         (source-hash-set (aref (aref propagator source-pattern) edge-label)))
    (when (u:href source-hash-set target-pattern)
      t)))
