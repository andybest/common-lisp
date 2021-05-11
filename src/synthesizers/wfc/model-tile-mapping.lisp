(in-package #:%syntex.synthesizers.wfc.tile-model-mapping)

(defclass mapping ()
  ((%empty-pattern-set :reader empty-pattern-set
                       :initform (u:dict #'eql))
   (%pattern-topology :reader pattern-topology
                      :initarg :pattern-topology)
   (%pattern-model :reader pattern-model
                   :initarg :pattern-model)
   (%tiles->patterns-by-offset :reader tiles->patterns-by-offset
                               :initarg :tiles->patterns-by-offset
                               :initform (u:dict #'eql))
   (%patterns->tiles-by-offset :reader patterns->tiles-by-offset
                               :initarg :patterns->tiles-by-offset
                               :initform (u:dict #'eql))
   (%tile-coord->pattern-coord-index/offset :reader tile-coord->pattern-coord-index/offset
                                            :initarg :tile-coord->pattern-coord-index/offset)
   (%pattern-coord->tile-coord-index/offset :reader pattern-coord->tile-coord-index/offset
                                            :initarg :pattern-coord->tile-coord-index/offset)))

(defun get-coord/offset (mapping point)
  (u:if-let ((pcio (tile-coord->pattern-coord-index/offset mapping)))
    (destructuring-bind (point index offset) (top:get pcio point)
      (declare (ignore index))
      (values point offset))
    (values (base:copy-point point) 0)))

(defun get-index/offset (mapping index)
  (u:if-let ((pcio (tile-coord->pattern-coord-index/offset mapping)))
    (destructuring-bind (point index offset) (top:get pcio index)
      (declare (ignore point))
      (values index offset))
    (values index 0)))

(defun make-tile-set (mapping tiles)
  (let* ((tile-set (base:make-tile-propagator-tile-set tiles))
         (tile-set-tiles (base:tiles tile-set))
         (o->p (base:offset->patterns tile-set))
         (pbo (tiles->patterns-by-offset mapping)))
    (when (= (length tile-set-tiles) 1)
      (let ((tile (aref tile-set-tiles 0)))
        (u:do-hash-keys (k pbo)
          (let ((patterns (u:href pbo k tile)))
            (setf (u:href o->p k) (or patterns (empty-pattern-set mapping)))))))
    tile-set))

(defun %get-patterns (tiles->patterns tile)
  (or (u:href tiles->patterns tile)
      (u:dict #'eql)))

(defgeneric get-patterns (mapping tile/tile-set offset))

(defmethod get-patterns ((mapping mapping) (tile base:tile) (offset integer))
  (let ((pbo (tiles->patterns-by-offset mapping)))
    (%get-patterns (u:href pbo offset) tile)))

(defmethod get-patterns ((mapping mapping)
                         (tile-set base:tile-propagator-tile-set)
                         (offset integer))
  (or (u:href (base:offset->patterns tile-set) offset)
      (let ((tiles->patterns (u:href (tiles->patterns-by-offset mapping) offset))
            (patterns (u:dict #'eql)))
        (map nil
             (lambda (x)
               (u:do-hash (k v (%get-patterns tiles->patterns x))
                 (setf (u:href patterns k) v)))
             (base:tiles tile-set))
        patterns)))
