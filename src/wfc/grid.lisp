(in-package #:cl-user)

(defpackage #:%syntex.wfc.grid
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:cell
   #:cells
   #:do-cells
   #:get-cell
   #:grid
   #:height
   #:make-grid
   #:value
   #:width
   #:x
   #:y))

(in-package #:%syntex.wfc.grid)

(defstruct (cell
            (:constructor make-cell (x y))
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (x 0 :type u:non-negative-fixnum)
  (y 0 :type u:non-negative-fixnum)
  (value 0 :type u:ub32))

(u:define-printer (cell stream)
  (format stream "~d,~d" (x cell) (y cell)))

(defstruct (grid
            (:constructor %make-grid)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (width 0 :type u:positive-fixnum)
  (height 0 :type u:positive-fixnum)
  (cells (make-array 0) :type simple-array))

(u:define-printer (grid stream)
  (format stream "~dx~d" (width grid) (height grid)))

(defun make-grid (width height)
  (let* ((cells (make-array (* width height)))
         (grid (%make-grid :width width :height height :cells cells)))
    (dotimes (y height)
      (dotimes (x width)
        (setf (aref cells (+ (* y width) x)) (make-cell x y))))
    grid))

(defun get-cell (grid x y &key periodic-p)
  (let ((width (width grid))
        (height (height grid)))
    (if periodic-p
        (aref (cells grid) (+ (* (mod y height) width) (mod x width)))
        (when (and (<= 0 x)
                   (< x width)
                   (<= 0 y)
                   (< y height))
          (aref (cells grid) (+ (* y width) x))))))

(defmacro do-cells ((grid cell) &body body)
  (u:with-gensyms (width height cells x y)
    `(let ((,width (width ,grid))
           (,height (height ,grid))
           (,cells (cells ,grid)))
       (dotimes (,y ,height)
         (dotimes (,x ,width)
           (let ((,cell (aref ,cells (+ (* ,y ,width) ,x))))
             ,@body))))))
