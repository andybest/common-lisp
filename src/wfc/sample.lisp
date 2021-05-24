(in-package #:%syntex.wfc)

(u:fn-> load-sample ((or pathname string)) grid)
(defun load-sample (file-path)
  (declare (optimize speed))
  (let* ((image (img:make-image file-path))
         (width (img:width image))
         (height (img:height image))
         (data (img:data image))
         (sample (make-grid width height)))
    (declare (u:ub16 width height)
             (u:ub32a data))
    (do-cells (sample cell)
      (let* ((x (x cell))
             (y (y cell))
             (index (+ (* y width) x)))
        (declare (u:ub16 x y))
        (setf (value cell) (aref data index))))
    sample))
