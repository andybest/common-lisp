(in-package #:%syntex.wfc)

(deftype grid-dimension () '(and u:ub16 (integer 2)))

(u:eval-always
  (defclass cell ()
    ((%x :reader x
         :initarg :x
         :initform 0)
     (%y :reader y
         :initarg :y
         :initform 0)
     (%value :accessor value
             :initform #xff00ffff))))

(u:eval-always
  (defclass grid ()
    ((%width :reader width
             :initarg :width
             :initform 0)
     (%height :reader height
              :initarg :height
              :initform 0)
     (%cells :accessor cells
             :initarg :cells
             :initform nil)
     (%cell-count :accessor cell-count
                  :initform 0))))

(u:define-printer (cell stream)
  (format stream "~d, ~d" (x cell) (y cell)))

(u:define-printer (grid stream)
  (format stream "~dx~d" (width grid) (height grid)))

(defmethod initialize-instance :after ((instance grid) &key cells)
  (let* ((width (width instance))
         (height (height instance))
         (cell-count (* width height)))
    (if cells
        (setf (cells instance) cells)
        (let ((cells (make-array cell-count)))
          (dotimes (y height)
            (dotimes (x width)
              (let ((cell (make-instance 'cell :x x :y y)))
                (setf (aref cells (+ (* y width) x)) cell))))
          (setf (cells instance) cells)))
    (setf (cell-count instance) (* width height))))

(u:fn-> make-grid (grid-dimension grid-dimension &optional simple-array) grid)
(defun make-grid (width height &optional cells)
  (declare (optimize speed))
  (values (make-instance 'grid :width width :height height :cells cells)))

(u:fn-> get-cell (grid fixnum fixnum &key (:periodic-p boolean)) (or cell null))
(defun get-cell (grid x y &key periodic-p)
  (declare (optimize speed))
  (let ((width (width grid))
        (height (height grid))
        (cells (cells grid)))
    (declare (u:ub16 width height)
             ((simple-array t) cells))
    (if periodic-p
        (aref cells (+ (* (mod y height) width) (mod x width)))
        (when (and (<= 0 x)
                   (< x width)
                   (<= 0 y)
                   (< y height))
          (aref cells (+ (* y width) x))))))

(defmacro do-cells ((grid cell) &body body)
  (u:with-gensyms (width height cells x y)
    `(let ((,width (width ,grid))
           (,height (height ,grid))
           (,cells (cells ,grid)))
       (declare (u:ub16 ,width ,height)
                ((simple-array t) ,cells))
       (dotimes (,y ,height)
         (dotimes (,x ,width)
           (let ((,cell (aref ,cells (+ (* ,y ,width) ,x))))
             ,@body))))))
