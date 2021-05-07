(in-package #:%syntex.synthesizers.wfc.direction)

(deftype axis () '(member :x :y :z :w))

(deftype direction () '(member :x+ :x- :y+ :y- :z+ :z- :w+ :w-))

(deftype type ()
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
              (:conc-name nil)
              (:predicate nil)
              (:copier nil))
    (x (u:make-b8-array 0) :type u:b8a)
    (y (u:make-b8-array 0) :type u:b8a)
    (z (u:make-b8-array 0) :type u:b8a)
    (count 0 :type u:ub8)
    (type 0 :type type)))

(u:define-constant +cartesian-2d+
    (let ((x (make-array 4 :element-type 'u:b8 :initial-contents '(1 -1 0 0)))
          (y (make-array 4 :element-type 'u:b8 :initial-contents '(0 0 1 -1)))
          (z (make-array 4 :element-type 'u:b8 :initial-contents '(0 0 0 0))))
      (make-direction-set :x x :y y :z z :count 4 :type :cartesian-2d))
  :test #'equalp)

(u:define-constant +cartesian-3d+
    (let ((x (make-array 6 :element-type 'u:b8 :initial-contents '(1 -1 0 0 0 0)))
          (y (make-array 6 :element-type 'u:b8 :initial-contents '(0 0 1 -1 0 0)))
          (z (make-array 6 :element-type 'u:b8 :initial-contents '(0 0 0 0 1 -1))))
      (make-direction-set :x x :y y :z z :count 6 :type :cartesian-3d))
  :test #'equalp)

(u:define-constant +hexagonal-2d+
    (let ((x (make-array 6 :element-type 'u:b8 :initial-contents '(1 -1 0 0 1 -1)))
          (y (make-array 6 :element-type 'u:b8 :initial-contents '(0 0 1 -1 1 -1)))
          (z (make-array 6 :element-type 'u:b8 :initial-contents '(0 0 0 0 0 0))))
      (make-direction-set :x x :y y :z z :count 6 :type :hexagonal-2d))
  :test #'equalp)

(u:define-constant +hexagonal-3d+
    (let ((x (make-array 8 :element-type 'u:b8 :initial-contents '(1 -1 0 0 0 0 1 -1)))
          (y (make-array 8 :element-type 'u:b8 :initial-contents '(0 0 1 -1 0 0 0 0)))
          (z (make-array 8 :element-type 'u:b8 :initial-contents '(0 0 0 0 1 -1 1 -1))))
      (make-direction-set :x x :y y :z z :count 8 :type :hexagonal-3d))
  :test #'equalp)

(defun invert (direction)
  (logxor direction 1))

(defun get (direction-set point)
  (dotimes (i (count direction-set))
    (when (and (= (point:x point) (aref (x direction-set) i))
               (= (point:y point) (aref (y direction-set) i))
               (= (point:z point) (aref (z direction-set) i)))
      (return-from get i)))
  (error "No direction corresponds to ~a." point))
