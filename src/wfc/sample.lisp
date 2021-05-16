(in-package #:cl-user)

(defpackage #:%syntex.wfc.sample
  (:local-nicknames
   (#:grid #:%syntex.wfc.grid)
   (#:img #:%syntex.wfc.image)
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow
   #:load)
  (:export
   #:load))

(in-package #:%syntex.wfc.sample)

(defun load (file-path)
  (let* ((image (img:make-image file-path))
         (width (img:width image))
         (height (img:height image))
         (data (img:data image))
         (sample (grid:make-grid width height)))
    (grid:do-cells (sample cell)
      (let ((index (+ (* (grid:y cell) width) (grid:x cell))))
        (setf (grid:value cell) (aref data index))))
    sample))
