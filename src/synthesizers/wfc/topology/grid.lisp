(in-package #:%syntex.synthesizers.wfc.topology)

(defclass grid (topology)
  ((%directions :accessor directions
                :initarg :directions)
   (%periodicity :reader periodicity
                 :initarg :periodicity
                 :initform (make-periodicity))))

(defmethod initialize-instance :after ((instance grid) &key)
  (let* ((width (width instance))
         (height (height instance))
         (depth (depth instance))
         (index-count (* width height depth)))
    (setf (index-count instance) index-count
          (directions-count instance) (direction-count (directions instance)))))

(defun make-grid (&key directions width height depth periodic-x periodic-y periodic-z mask)
  (make-instance 'grid
                 :directions directions
                 :width width
                 :height height
                 :depth depth
                 :periodicity (make-periodicity periodic-x periodic-y periodic-z)
                 :mask mask))

(defun make-grid-2d (width height &key periodic-p)
  (make-grid :directions +cartesian-2d+
             :width width
             :height height
             :depth 1
             :periodic-x periodic-p
             :periodic-y periodic-p))

(defun make-grid-3d (width height depth &key periodic-p)
  (make-grid :directions +cartesian-3d+
             :width width
             :height height
             :depth depth
             :periodic-x periodic-p
             :periodic-y periodic-p
             :periodic-z periodic-p))

(defun same-size-p (grid1 grid2)
  (and (= (width grid1) (width grid2))
       (= (height grid1) (height grid2))
       (= (depth grid1) (depth grid2))))

(defgeneric make-masked-copy (grid mask))

(defmethod make-masked-copy ((grid grid) (mask simple-bit-vector))
  (unless (= (index-count grid) (length mask))
    (error "Mask must be the same size as the grid."))
  (let ((periodicity (periodicity grid)))
    (make-grid :directions (directions grid)
               :width (width grid)
               :height (height grid)
               :depth (depth grid)
               :periodic-x (periodic-x periodicity)
               :periodic-y (periodic-y periodicity)
               :periodic-z (periodic-z periodicity)
               :mask mask)))

(defmethod make-masked-copy :before ((grid grid) (mask data))
  (let ((topology (topology mask)))
    (unless (typep topology 'grid)
      (error "Mask topology must be a grid."))
    (unless (same-size-p grid topology)
      (error "Mask topology must be the same size as the grid."))))

(defmethod make-masked-copy ((grid grid) (mask data))
  (let ((mask-data (make-array (index-count grid) :element-type 'bit)))
    (dotimes (z (depth grid))
      (dotimes (y (height grid))
        (dotimes (x (width grid))
          (let ((point (base:make-point x y z)))
            (setf (aref mask-data (get-index grid point)) (get mask point))))))
    (make-masked-copy grid mask-data)))

(defun make-resized-copy (grid &key width height (depth 1))
  (let ((periodicity (periodicity grid)))
    (make-grid :directions (directions grid)
               :width width
               :height height
               :depth depth
               :periodic-x (periodic-x periodicity)
               :periodic-y (periodic-y periodicity)
               :periodic-z (periodic-z periodicity))))

(defun make-periodic-copy (grid &key x y z)
  (make-grid :directions (directions grid)
             :width (width grid)
             :height (height grid)
             :depth (depth grid)
             :periodic-x x
             :periodic-y y
             :periodic-z z
             :mask (mask grid)))

(defmethod get-index ((topology grid) (point base:point))
  (let ((width (width topology)))
    (+ (base:point-x point)
       (* (base:point-y point) width)
       (* (base:point-z point) width (height topology)))))

(defmethod get-coords ((topology grid) (index integer))
  (let* ((width (width topology))
         (height (height topology))
         (i (truncate index width)))
    (base:make-point (mod index width)
                     (mod i height)
                     (truncate i height))))

(defmethod try-move ((topology grid) (point base:point) (direction integer))
  (macrolet ((modify-coord (coord dimension)
               (u:with-gensyms (point-coord)
                 (u:once-only (dimension)
                   `(symbol-macrolet ((,point-coord (,(u:format-symbol :base "POINT-~a" coord)
                                                     point)))
                      (cond
                        ((,(u:symbolicate '#:periodic- coord) periodicity)
                         (setf ,point-coord (mod ,point-coord ,dimension)))
                        ((or (minusp ,point-coord)
                             (>= ,point-coord ,dimension))
                         (return-from try-move nil))))))))
    (let* ((mask (mask topology))
           (periodicity topology)
           (directions (directions topology))
           (out-point (base:copy-point point)))
      (incf (base:point-x out-point) (aref (direction-x directions) direction))
      (incf (base:point-y out-point) (aref (direction-y directions) direction))
      (incf (base:point-z out-point) (aref (direction-z directions) direction))
      (modify-coord x (width topology))
      (modify-coord y (height topology))
      (modify-coord z (depth topology))
      (let ((index (get-index topology out-point)))
        (when (or (and mask (plusp (bit mask index)))
                  (not mask))
          (values out-point index))))))

(defmethod try-move ((topology grid) (index integer) (direction integer))
  (let ((point (get-coords topology index)))
    (try-move topology point direction)))
