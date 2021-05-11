(in-package #:%syntex.synthesizers.wfc.graph-adjacent-model)

(defclass model (tm:model)
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

(defmethod tm:tiles ((object model))
  (u:hash-keys (tiles->patterns object)))

(defun make-model (directions-count edge-label-count)
  (make-instance 'model
                 :directions-count directions-count
                 :edge-label-count edge-label-count))

(defmethod tm:get-mapping ((model model) (topology top:topology))
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
      (make-instance 'tmm:mapping
                     :pattern-topology topology
                     :pattern-model pattern-model
                     :patterns->tiles-by-offset t->pbo
                     :tiles->patterns-by-offset p->tbo
                     :tile-coord->pattern-coord-index/offset nil))))

(defmethod tm:multiply-frequency ((model model) (tile base:tile) (multiplier float))
  (let ((pattern (u:href (tiles->patterns model) tile)))
    (incf (aref (frequencies model) pattern) multiplier)))

(defun get-pattern (model tile)
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

(defun set-frequency/transforms (model tile frequency transforms)
  (let ((transformed-tiles (tfm:transform-all transforms tile))
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

(defun set-frequency (model tile frequency)
  (let ((pattern (get-pattern model tile)))
    (setf (aref (frequencies model) pattern) frequency)))

(defun set-uniform-frequency (model)
  (dolist (tile (tm:tiles model))
    (set-frequency model tile 1d0)))

(defgeneric add-adjacency/transform (model source target direction tile-transform))

(defmethod add-adjacency/transform ((model model)
                                    (source vector)
                                    (target vector)
                                    (direction integer)
                                    (tile-transform tfm:tile-transform))
  (map nil
       (lambda (x)
         (map nil
              (lambda (y)
                (add-adjacency/transform model x y direction tile-transform))
              target))
       source))

(defmethod add-adjacency/transform ((model model)
                                    (source base:tile)
                                    (target base:tile)
                                    (direction integer)
                                    (tile-transform tfm:tile-transform))
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
                  (add-adjacency model target i))))))))))

(defgeneric add-adjacency (model source target edge-label))

(defmethod add-adjacency ((model model) (source vector) (target vector) (edge-label integer))
  (map nil
       (lambda (x)
         (map nil
              (lambda (y)
                (add-adjacency model x y edge-label))
              target))
       source))

(defmethod add-adjacency ((model model) (source base:tile) (target base:tile) (edge-label integer))
  (let* ((propagator (propagator model))
         (source (get-pattern model source))
         (target (get-pattern model target))
         (hash-set (aref (aref propagator source) edge-label)))
    (setf (u:href hash-set target) target)))

(defun adjacent-p (model source target edge-label)
  (let* ((propagator (propagator model))
         (source-pattern (get-pattern model source))
         (target-pattern (get-pattern model target))
         (source-hash-set (aref (aref propagator source-pattern) edge-label)))
    (when (u:href source-hash-set target-pattern)
      t)))
