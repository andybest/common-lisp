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

(defun make-model ()
  (make-instance 'model))

(defun from-directions (directions)
  (let ((model (make-model)))
    (set-directions model directions)
    model))

(defgeneric from-topology-data (sample))

(defmethod from-topology-data ((sample top:data/tiles))
  (let ((model (make-model)))
    sample
    model))

(defmethod from-topology-data ((sample top:data))
  (from-topology-data (top.dat:to-tiles sample)))

(defun from-2d-array (sample &optional periodic-p)
  (let ((topology-data (top.dat:make-data-2d sample :periodic-p periodic-p)))
    (from-topology-data topology-data)))

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

(defun %set-frequency-transforms (model tile frequency transforms)
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
        (%set-frequency-transforms model tile frequency transforms)
        (%set-frequency model tile frequency))))

(defun set-uniform-frequency (model)
  (dolist (tile (tm:tiles model))
    (set-frequency model tile 1d0)))

(defun require-directions (model)
  (when (eq (dir:type (directions model)) :unknown)
    (error "Directions must be set.")))

(defgeneric add-adjacency (model source target direction/point &optional transforms))

(defmethod add-adjacency ((model model)
                          (source vector)
                          (target vector)
                          (direction integer)
                          &optional transforms)
  (require-directions model)
  (let* ((directions (directions model))
         (x (aref (dir:x directions) direction))
         (y (aref (dir:y directions) direction))
         (z (aref (dir:z directions) direction))
         (point (point:point x y z)))
    (add-adjacency model source target point transforms)))

#++(defmethod add-adjacency ((model model)
                             (source vector)
                             (target vector)
                             (point point:point)
                             &optional transforms)
     (require-directions model)
     (let ((transforms (or transforms (tfm.tile:make-transforms))))
       ))
