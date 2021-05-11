(in-package #:%syntex.synthesizers.wfc.topology-data)

(defclass data/tiles () ())

(defun make-data (topology values)
  (etypecase values
    ((simple-array t (*))
     (make-instance 'data-1d :topology topology :values values))
    ((simple-array t (* *))
     (make-instance 'data-2d :topology topology :values values))
    ((simple-array t (* * *))
     (make-instance 'data-3d :topology topology :values values))))

(defun make-data-by-coords (topology func)
  (let* ((width (top:width topology))
         (height (top:height topology))
         (depth (top:depth topology))
         (values (make-array (list width height depth))))
    (dotimes (z depth)
      (dotimes (y height)
        (dotimes (x width)
          (let* ((point (point:point x y z))
                 (index (top:get-index topology point)))
            (when (top:contains-index-p topology index)
              (setf (aref values x y z) (funcall func point)))))))
    (make-data topology values)))

(defun make-data-by-index (topology func)
  (let ((values (make-array (top:index-count topology))))
    (cl:map nil
            (lambda (x)
              (setf (aref values x) (funcall func x)))
            (top:get-indices topology))
    (make-data topology values)))

(defun to-2d-array (data)
  (let* ((topology (top:topology data))
         (width (top:width topology))
         (height (top:height topology))
         (values (make-array (list width height))))
    (dotimes (x width)
      (dotimes (y height)
        (let ((point (point:point x y)))
          (setf (aref values x y) (top:get data point)))))
    values))

(defun to-3d-array (data)
  (let* ((topology (top:topology data))
         (width (top:width topology))
         (height (top:height topology))
         (depth (top:depth topology))
         (values (make-array (list width height depth))))
    (dotimes (x width)
      (dotimes (y height)
        (dotimes (z depth)
          (let ((point (point:point x y z)))
            (setf (aref values x y z) (top:get data point))))))
    values))

(defun map (data func)
  (let* ((topology (top:topology data))
         (values (make-array (top:index-count topology))))
    (cl:map nil
            (lambda (x)
              (setf (aref values x) (funcall func (top:get data x))))
            (top:get-indices topology))
    (make-data-1d topology values)))

(defun to-tiles (data)
  (let ((data (map data (lambda (x) (tile:tile x)))))
    (change-class data 'data-1d/tiles)))

(defun transform-vector/square (x y transform)
  (let ((x (if (tfm:reflect-x transform) (- x) x)))
    (ecase (tfm:rotation transform)
      (0 (values x y))
      (90 (values (- y) x))
      (180 (values (- x) (- y)))
      (270 (values y (- x))))))

(defun %transform-vector/hex (x y micro rotate-180-p reflect-p)
  (let* ((x (if reflect-p (+ (- x) y) x))
         (q (- x y))
         (r (- x))
         (s y))
    (case micro
      (1 (psetf q s s r r q))
      (2 (psetf q r r s s q)))
    (when rotate-180-p
      (setf q (- q)
            r (- r)
            s (- s)))
    (values (- r) s)))

(defun transform-vector/hex (x y transform)
  (let* ((rotation (tfm:rotation transform))
         (micro (mod (truncate rotation 60) 3))
         (rotate-180-p (plusp (mod (truncate rotation 60) 2))))
    (%transform-vector/hex x y micro rotate-180-p (tfm:reflect-x transform))))

(defun transform-vector (direction-type x y transform)
  (case direction-type
    ((:cartesian-2d :cartesian-3d)
     (transform-vector/square x y transform))
    (:hexagonal-2d
     (transform-vector/hex x y transform))
    (t
     (error "Unknown direction type: ~s." direction-type))))

(defun transform-direction (directions direction transform)
  (u:mvlet* ((x (aref (dir:direction-x directions) direction))
             (y (aref (dir:direction-y directions) direction))
             (z (aref (dir:direction-z directions) direction))
             (rx ry (transform-vector (dir:direction-type directions) x y transform)))
    (dir:get-direction directions (point:point rx ry z))))

(defgeneric transform (original transform &optional tile-transform))

(defmethod transform ((original top:data/tiles)
                      (transform tfm:transform)
                      &optional tile-transform)
  (let ((topology (top:topology original)))
    (unless (typep topology 'grid:grid)
      (error "Expected a grid-based topology."))
    (let ((type (dir:direction-type (grid:directions topology))))
      (case type
        ((:cartesian-2d :cartesian-3d)
         (transform/square original transform tile-transform))
        (:hexagonal-2d
         (transform/hex original transform tile-transform))
        (t
         (error "Unknown direction type: ~s." type))))))

(defgeneric transform/square (original transform &optional tile-transform))

(defmethod transform/square ((original top:data/tiles)
                             (transform tfm:transform)
                             &optional tile-transform)
  (flet ((%transform-tile (tile)
           (tfm:transform-tile tile-transform tile transform)))
    (transform/square original
                      transform
                      (when tile-transform
                        #'%transform-tile))))

(defmethod transform/square ((original top:data)
                             (transform tfm:transform)
                             &optional tile-transform)
  (flet ((map-coord (x y)
           (transform-vector/square x y transform)))
    (if (tfm:identity-p transform)
        original
        (transform-inner original #'map-coord tile-transform))))

(defgeneric transform/hex (original transform &optional tile-transform))

(defmethod transform/hex ((original top:data/tiles)
                          (transform tfm:transform)
                          &optional tile-transform)
  (flet ((%transform-tile (tile)
           (tfm:transform-tile tile-transform tile transform)))
    (transform/hex original
                   transform
                   (when tile-transform
                     #'%transform-tile))))

(defmethod transform/hex ((original top:data)
                          (transform tfm:transform)
                          &optional tile-transform)
  (when (tfm:identity-p transform)
    (return-from transform/hex original))
  (let* ((rotation (tfm:rotation transform))
         (micro (mod (truncate rotation 60) 3))
         (rotate-180-p (plusp (mod (truncate rotation 60) 2))))
    (flet ((map-coord (x y)
             (%transform-vector/hex x y micro rotate-180-p (tfm:reflect-x transform))))
      (transform-inner original #'map-coord tile-transform))))

(defun transform-inner (original map-coord-func tile-transform-func)
  (let* ((original-topology (top:topology original))
         (width (top:width original-topology))
         (height (top:height original-topology)))
    (unless (typep original-topology 'grid:grid)
      (error "Expected a grid-based topology."))
    (u:mvlet* ((x1 y1 (funcall map-coord-func 0 0))
               (x2 y2 (funcall map-coord-func (1- width) 0))
               (x3 y3 (funcall map-coord-func (1- width) (1- height)))
               (x4 y4 (funcall map-coord-func 0 (1- height)))
               (min-x (min x1 x2 x3 x4))
               (min-y (min y1 y2 y3 y4))
               (max-x (max x1 x2 x3 x4))
               (max-y (max y1 y2 y3 y4))
               (offset-x (- min-x))
               (offset-y (- min-y))
               (width (1+ (- max-x min-x)))
               (height (1+ (- max-y min-y)))
               (depth (top:depth original-topology))
               (mask (make-array (* width height depth) :element-type 'bit :initial-element 0))
               (topology (grid:make-grid :directions (grid:directions original-topology)
                                         :width width
                                         :height height
                                         :depth depth
                                         :mask mask))
               (values (make-array (list width height depth))))
      (dotimes (z depth)
        (dotimes (y height)
          (dotimes (x width)
            (u:mvlet ((new-x new-y (funcall map-coord-func x y)))
              (incf new-x offset-x)
              (incf new-y offset-y)
              (let ((new-index (top:get-index topology (point:point new-x new-y z)))
                    (new-value (top:get original (point:point x y z)))
                    (new-value-bit 1))
                (when tile-transform-func
                  (u:mvlet ((tile success-p (funcall tile-transform-func new-value)))
                    (setf new-value tile
                          new-value-bit (if success-p 1 0))))
                (setf (aref values new-x new-y z) new-value
                      (aref mask new-index) (and new-value-bit
                                                 (top:contains-index-p
                                                  original-topology
                                                  (top:get-index original-topology
                                                                 (point:point x y z))))))))))
      (make-data-3d values :topology topology))))
