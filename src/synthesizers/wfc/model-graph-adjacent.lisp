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

(defmethod tm:multiply-frequency ((model model) (tile tile:tile) (multiplier float))
  (let ((pattern (u:href (tiles->patterns model) tile)))
    (incf (aref (frequencies model) pattern) multiplier)))

#++(defun get-pattern ())

#++(defun set-frequency/transforms (model tile frequency transforms)
     (let ((transformed-tiles (tfm.tile:transform-all transforms tile))
           (frequencies (frequencies model)))
       (map nil
            (lambda (x)
              (let ((pattern (get-pattern model x)))
                (setf (aref frequencies pattern) 0d0)))
            transformed-tiles)))
