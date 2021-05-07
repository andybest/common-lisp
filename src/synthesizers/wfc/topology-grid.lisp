(in-package #:%syntex.synthesizers.wfc.topology-grid)

(defclass grid (top:topology)
  ((%directions :accessor directions
                :initarg :directions)
   (%periodicity :reader periodicity
                 :initarg :periodicity
                 :initform (per:periodicity))))

(defmethod initialize-instance :after ((instance grid) &key)
  (let* ((width (top:width instance))
         (height (top:height instance))
         (depth (top:depth instance))
         (index-count (* width height depth)))
    (setf (top:index-count instance) index-count
          (top:directions-count instance) (dir:count (directions instance)))))

(defun make-grid (&key directions width height depth periodic-x periodic-y periodic-z mask)
  (make-instance 'grid
                 :directions directions
                 :width width
                 :height height
                 :depth depth
                 :periodicity (per:periodicity periodic-x periodic-y periodic-z)
                 :mask mask))

(defun make-grid-2d (width height &key periodic-p)
  (make-grid :directions dir:+cartesian-2d+
             :width width
             :height height
             :depth 1
             :periodic-x periodic-p
             :periodic-y periodic-p))

(defun make-grid-3d (width height depth &key periodic-p)
  (make-grid :directions dir:+cartesian-3d+
             :width width
             :height height
             :depth depth
             :periodic-x periodic-p
             :periodic-y periodic-p
             :periodic-z periodic-p))

(defun same-size-p (grid1 grid2)
  (and (= (top:width grid1) (top:width grid2))
       (= (top:height grid1) (top:height grid2))
       (= (top:depth grid1) (top:depth grid2))))

(defgeneric make-masked-copy (grid mask))

(defmethod make-masked-copy ((grid grid) (mask simple-bit-vector))
  (unless (= (top:index-count grid) (length mask))
    (error "Mask must be the same size as the grid."))
  (let ((periodicity (periodicity grid)))
    (make-grid :directions (directions grid)
               :width (top:width grid)
               :height (top:height grid)
               :depth (top:depth grid)
               :periodic-x (per:x periodicity)
               :periodic-y (per:y periodicity)
               :periodic-z (per:z periodicity)
               :mask mask)))

(defmethod make-masked-copy :before ((grid grid) (mask top:data))
  (let ((topology (top:topology mask)))
    (unless (typep topology 'grid)
      (error "Mask topology must be a grid."))
    (unless (same-size-p grid topology)
      (error "Mask topology must be the same size as the grid."))))

(defmethod make-masked-copy ((grid grid) (mask top:data))
  (let ((mask-data (make-array (top:index-count grid) :element-type 'bit)))
    (dotimes (z (top:depth grid))
      (dotimes (y (top:height grid))
        (dotimes (x (top:width grid))
          (let ((point (point:point x y z)))
            (setf (aref mask-data (top:get-index grid point)) (top:get-value mask point))))))
    (make-masked-copy grid mask-data)))

(defun make-resized-copy (grid &key width height (depth 1))
  (let ((periodicity (periodicity grid)))
    (make-grid :directions (directions grid)
               :width width
               :height height
               :depth depth
               :periodic-x (per:x periodicity)
               :periodic-y (per:y periodicity)
               :periodic-z (per:z periodicity))))

(defun make-periodic-copy (grid &key x y z)
  (make-grid :directions (directions grid)
             :width (top:width grid)
             :height (top:height grid)
             :depth (top:depth grid)
             :periodic-x x
             :periodic-y y
             :periodic-z z
             :mask (top:mask grid)))

(defmethod top:get-index ((topology grid) (point point:point))
  (let ((width (top:width topology)))
    (+ (point:x point) (* (point:y point) width) (* (point:z point) width (top:height topology)))))

(defmethod top:get-coords ((topology grid) (index integer))
  (let* ((width (top:width topology))
         (height (top:height topology))
         (i (truncate index width)))
    (point:point (mod index width)
                 (mod i height)
                 (truncate i height))))

(defmethod top:try-move ((topology grid) (point point:point) (direction integer))
  (macrolet ((modify-coord (coord dimension)
               (u:with-gensyms (point-coord)
                 (u:once-only (dimension)
                   `(symbol-macrolet ((,point-coord (,(u:format-symbol :point "~a" coord) point)))
                      (cond
                        ((,(u:format-symbol :per "~a" coord) periodicity)
                         (setf ,point-coord (mod ,point-coord ,dimension)))
                        ((or (minusp ,point-coord)
                             (>= ,point-coord ,dimension))
                         (return-from top:try-move nil))))))))
    (let* ((mask (top:mask topology))
           (periodicity topology)
           (directions (directions topology))
           (out-point (point:copy point)))
      (incf (point:x out-point) (aref (dir:x directions) direction))
      (incf (point:y out-point) (aref (dir:y directions) direction))
      (incf (point:z out-point) (aref (dir:z directions) direction))
      (modify-coord x (top:width topology))
      (modify-coord y (top:height topology))
      (modify-coord z (top:depth topology))
      (let ((index (top:get-index topology out-point)))
        (when (or (and mask (plusp (bit mask index)))
                  (not mask))
          (values out-point index))))))

(defmethod top:try-move ((topology grid) (index integer) (direction integer))
  (let ((point (top:get-coords topology index)))
    (top:try-move topology point direction)))
