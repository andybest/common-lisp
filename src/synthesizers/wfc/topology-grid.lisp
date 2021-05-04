(in-package #:%syntex.synthesizers.wfc)

(defclass topology-grid (topology)
  ((%directions :accessor directions
                :initarg :directions)
   (%periodic-x :reader periodic-x
                :initarg :periodic-x
                :initform nil)
   (%periodic-y :reader periodic-y
                :initarg :periodic-y
                :initform nil)
   (%periodic-z :reader periodic-z
                :initarg :periodic-z
                :initform nil)))

(defmethod initialize-instance :after ((instance topology-grid) &key)
  (let* ((width (width instance))
         (height (height instance))
         (depth (depth instance))
         (index-count (* width height depth)))
    (setf (index-count instance) index-count
          (directions-count instance) (direction-set-count (directions instance)))
    (unless (mask instance)
      (setf (mask instance) (make-array index-count :element-type 'bit :initial-element 0)))))

(defun make-topology-grid (&key directions width height depth periodic-x periodic-y periodic-z mask)
  (make-instance 'topology-grid
                 :directions directions
                 :width width
                 :height height
                 :depth depth
                 :periodic-x periodic-x
                 :periodic-y periodic-y
                 :periodic-z periodic-z
                 :mask mask))

(defun make-topology-grid-2d (width height &key periodic-p)
  (make-topology-grid :directions +cartesian-2d+
                      :width width
                      :height height
                      :depth 1
                      :periodic-x periodic-p
                      :periodic-y periodic-p))

(defun make-topology-grid-3d (width height depth &key periodic-p)
  (make-topology-grid :directions +cartesian-3d+
                      :width width
                      :height height
                      :depth depth
                      :periodic-x periodic-p
                      :periodic-y periodic-p
                      :periodic-z periodic-p))

(defmethod mask-topology ((grid topology-grid) mask)
  (let ((width (width grid))
        (height (height grid))
        (depth (depth grid)))
    (if (= (* width height depth) (length mask))
        (make-topology-grid :directions (directions grid)
                            :width width
                            :height height
                            :depth depth
                            :periodic-x (periodic-x grid)
                            :periodic-y (periodic-y grid)
                            :periodic-z (periodic-z grid)
                            :mask mask)
        (error "Mask must be the same size as the grid."))))

(defgeneric make-resized-copy (topology-grid &key width height depth)
  (:method ((grid topology-grid) &key width height (depth 1))
    (make-topology-grid :directions (directions grid)
                        :width width
                        :height height
                        :depth depth
                        :periodic-x (periodic-x grid)
                        :periodic-y (periodic-y grid)
                        :periodic-z (periodic-z grid))))

(defgeneric make-periodic-copy (topology-grid &key x y z)
  (:method ((grid topology-grid) &key x y z)
    (make-topology-grid :directions (directions grid)
                        :width (width grid)
                        :height (height grid)
                        :depth (depth grid)
                        :periodic-x x
                        :periodic-y y
                        :periodic-z z
                        :mask (mask grid))))

(defgeneric same-size-p (grid1 grid2)
  (:method ((grid1 topology-grid) (grid2 topology-grid))
    (and (= (width grid1) (width grid2))
         (= (height grid1) (height grid2))
         (= (depth grid1) (depth grid2)))))

(defmethod get-index ((topology topology-grid) x y z)
  (let ((width (width topology)))
    (+ x (* y width) (* z width (height topology)))))

(defmethod get-coords ((topology topology-grid) index)
  (let* ((width (width topology))
         (height (height topology))
         (i (truncate index width)))
    (values (mod index width)
            (mod i height)
            (truncate i height))))
