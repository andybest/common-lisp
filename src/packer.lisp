(in-package :gamebox-sprite-packer)

(defmacro with-rect ((x y &optional (w (gensym)) (h (gensym))) rect &body body)
  `(destructuring-bind (,x ,y ,w ,h) ,rect
     (declare (ignorable ,x ,y ,w ,h))
     ,@body))

(defun delta-weight (width height rect)
  (with-rect (x y w h) rect
    (min (- w width) (- h height))))

(defun find-free-rect (width height rects)
  (unless rects (error "Cannot pack anymore rectangles."))
  (loop :with min-rect = (first rects)
        :with min-delta = (delta-weight width height min-rect)
        :for rect :in (rest rects)
        :for current-delta = (delta-weight width height rect)
        :when (or (minusp min-delta)
                  (and (not (minusp current-delta))
                       (< current-delta min-delta)))
          :do (setf min-rect rect
                    min-delta current-delta)
        :finally (return (if (minusp min-delta)
                             (error "Cannot pack anymore rectangles.")
                             min-rect))))

(defun intersectsp (rect1 rect2)
  (with-rect (x1 y1 w1 h1) rect1
    (with-rect (x2 y2 w2 h2) rect2
      (and (< x1 (+ x2 w2))
           (> (+ x1 w1) x2)
           (< y1 (+ y2 h2))
           (> (+ y1 h1) y2)))))

(defun subdivide-rect (rect placed)
  (flet ((splitsp (coord from to)
           (> to coord from)))
    (if (intersectsp placed rect)
        (with-rect (x y w h) rect
          (with-rect (px py pw ph) placed
            (let ((result))
              (when (splitsp px x (+ x w))
                (push (list x y (- px x) h) result))
              (when (splitsp (+ px pw) x (+ x w))
                (push (list (+ px pw) y (- (+ x w) (+ px pw)) h) result))
              (when (splitsp py y (+ y h))
                (push (list x y w (- py y)) result))
              (when (splitsp (+ py ph) y (+ y h))
                (push (list x (+ py ph) w (- (+ y h) (+ py ph))) result))
              result)))
        (list rect))))

(defun containsp (outer inner)
  (with-rect (ox oy ow oh) outer
    (with-rect (ix iy iw ih) inner
      (and (>= (+ ox ow) (+ ix iw) ix ox)
           (>= (+ oy oh) (+ iy ih) iy oy)))))

(defun normalize-free-space (rects)
  (remove
   nil
   (loop :with rest-filtered = rects
         :for (rect . rest) = rest-filtered
         :while rect
         :collect (loop :with containedp
                        :for other-rect :in rest
                        :unless (containsp rect other-rect)
                          :collect other-rect :into filtered
                        :when (and (not containedp)
                                   (containsp other-rect rect))
                          :do (setf containedp t)
                        :finally (setf rest-filtered filtered)
                                 (return (unless containedp rect))))))

(defun resolve-free-rects (rect free-rects)
  (normalize-free-space
   (loop :for free-rect :in free-rects
         :append (subdivide-rect free-rect rect))))

(defun place-rect (width height free-rects)
  (with-rect (x y) (find-free-rect width height free-rects)
    (let ((rect (list x y width height)))
      (values rect (resolve-free-rects rect free-rects)))))

(defun sort-rects (rects)
  (labels ((apply-fn (fn rect)
             (destructuring-bind (file id w h) rect
               (declare (ignore file id))
               (funcall fn w h)))
           (sort-by (rects fn)
             (stable-sort rects #'> :key (lambda (x) (apply-fn fn x)))))
    (sort-by (sort-by rects #'min) #'max)))

(defun pack-rects (rects width height)
  (loop :with free-rects = (list (list 0 0 width height))
        :for (file id rect-width rect-height) :in (sort-rects rects)
        :collect (multiple-value-bind (rect new-free-rects)
                     (place-rect rect-width rect-height free-rects)
                   (setf free-rects new-free-rects)
                   (list file id rect))))

(defun collect-files (path &key recursive)
  (let ((files))
    (fs-utils:map-files
     path
     (lambda (x) (push (cons x (make-id path x)) files))
     :recursive recursive)
    files))

(defun make-rects (files)
  (loop :for (file . id) :in files
        :for image = (pngload:load-file file :decode nil)
        :collect (list file id (pngload:height image) (pngload:width image))))

(defun write-metadata (data out-file)
  (let ((out-file (make-pathname :defaults out-file :type "spec"))
        (data (sort data #'string< :key (lambda (x) (getf x :id)))))
    (with-open-file (out out-file
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (write data :stream out))))

(defun make-id (root file)
  (namestring
   (make-pathname
    :defaults
    (uiop/pathname:enough-pathname
     file
     (uiop/pathname:ensure-directory-pathname root))
    :type nil)))

(defun make-coords (rect width height normalize)
  (flet ((%make-coords (x y w h)
           (list :x1 x :y1 y :x2 (+ x w) :y2 (+ y h))))
    (destructuring-bind (x y w h) rect
      (if normalize
          (let ((x (float (/ x width)))
                (y (float (/ y height)))
                (w (float (/ w width)))
                (h (float (/ h height))))
            (%make-coords x y w h))
          (%make-coords x y w h)))))

(defun write-sprite (atlas sprite rect)
  (let ((sprite (opticl:coerce-image sprite 'opticl:rgba-image)))
    (destructuring-bind (x y w h) rect
      (declare (ignore w h))
      (opticl:do-pixels (i j) sprite
        (setf (opticl:pixel atlas (+ i x) (+ j y))
              (opticl:pixel sprite i j))))))

(defun make-atlas (file-specs &key out-file width height normalize)
  (loop :with atlas = (opticl:make-8-bit-rgba-image width height)
        :with rects = (make-rects file-specs)
        :for (file id rect) :in (pack-rects rects width height)
        :for sprite = (load-image file)
        :for coords = (make-coords rect width height normalize)
        :do (write-sprite atlas sprite rect)
        :collect `(:id ,id ,@coords) :into data
        :finally (return
                   (values
                    (write-metadata data out-file)
                    (opticl:write-image-file out-file atlas)))))

(defun make-atlas-from-directory (path &key recursive out-file width height
                                         normalize)
  (let ((file-specs (collect-files path :recursive recursive)))
    (make-atlas file-specs
                :out-file out-file
                :width width
                :height height
                :normalize normalize)))
