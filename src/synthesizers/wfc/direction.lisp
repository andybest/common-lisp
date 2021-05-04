(in-package #:%syntex.synthesizers.wfc)

(deftype axis () '(member :x :y :z :w))

(deftype direction () '(member :x+ :x- :y+ :y- :z+ :z- :w+ :w-))

(deftype direction-type ()
  '(member :unknown :cartesian-2d :hexagonal-2d :cartesian-3d :hexagonal-3d))

(defun axis->index (axis)
  (ecase axis
    (:x 0)
    (:y 1)
    (:z 2)
    (:w 3)))

(defun index->axis (index)
  (ecase index
    (0 :x)
    (1 :y)
    (2 :z)
    (3 :w)))

(defun direction->index (direction)
  (ecase direction
    (:x+ 0)
    (:x- 1)
    (:y+ 2)
    (:y- 3)
    (:z+ 4)
    (:z- 5)))

(defun index->direction (index)
  (ecase index
    (0 :x+)
    (1 :x-)
    (2 :y+)
    (3 :y-)
    (4 :z+)
    (5 :z-)))

(u:eval-always
  (defstruct (direction-set
              (:predicate nil)
              (:copier nil))
    (dx (u:make-b8-array 0) :type u:b8a)
    (dy (u:make-b8-array 0) :type u:b8a)
    (dz (u:make-b8-array 0) :type u:b8a)
    (count 0 :type u:ub8)
    (type 0 :type direction-type)))

(u:define-constant +cartesian-2d+
    (let ((dx (make-array 4 :element-type 'u:b8 :initial-contents '(1 -1 0 0)))
          (dy (make-array 4 :element-type 'u:b8 :initial-contents '(0 0 1 -1)))
          (dz (make-array 4 :element-type 'u:b8 :initial-contents '(0 0 0 0))))
      (make-direction-set :dx dx
                          :dy dy
                          :dz dz
                          :count 4
                          :type :cartesian-2d))
  :test #'equalp)

(u:define-constant +cartesian-3d+
    (let ((dx (make-array 6 :element-type 'u:b8 :initial-contents '(1 -1 0 0 0 0)))
          (dy (make-array 6 :element-type 'u:b8 :initial-contents '(0 0 1 -1 0 0)))
          (dz (make-array 6 :element-type 'u:b8 :initial-contents '(0 0 0 0 1 -1))))
      (make-direction-set :dx dx
                          :dy dy
                          :dz dz
                          :count 6
                          :type :cartesian-3d))
  :test #'equalp)

(u:define-constant +hexagonal-2d+
    (let ((dx (make-array 6 :element-type 'u:b8 :initial-contents '(1 -1 0 0 1 -1)))
          (dy (make-array 6 :element-type 'u:b8 :initial-contents '(0 0 1 -1 1 -1)))
          (dz (make-array 6 :element-type 'u:b8 :initial-contents '(0 0 0 0 0 0))))
      (make-direction-set :dx dx
                          :dy dy
                          :dz dz
                          :count 6
                          :type :hexagonal-2d))
  :test #'equalp)

(u:define-constant +hexagonal-3d+
    (let ((dx (make-array 8 :element-type 'u:b8 :initial-contents '(1 -1 0 0 0 0 1 -1)))
          (dy (make-array 8 :element-type 'u:b8 :initial-contents '(0 0 1 -1 0 0 0 0)))
          (dz (make-array 8 :element-type 'u:b8 :initial-contents '(0 0 0 0 1 -1 1 -1))))
      (make-direction-set :dx dx
                          :dy dy
                          :dz dz
                          :count 8
                          :type :hexagonal-3d))
  :test #'equalp)

(defun inverse (direction)
  (logxor direction 1))

(defun get-direction (direction-set x y &optional (z 0))
  (dotimes (i (direction-set-count direction-set))
    (when (and (= x (aref (direction-set-dx direction-set) i))
               (= y (aref (direction-set-dy direction-set) i))
               (= z (aref (direction-set-dz direction-set) i)))
      (return-from get-direction i)))
  (error "No direction corresponds to ~a, ~a, ~a." x y z))
