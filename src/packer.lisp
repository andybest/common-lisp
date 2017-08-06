(in-package :gamebox-sprite-packer)

(defmacro with-rect ((x y &optional (w (gensym)) (h (gensym))) rect &body body)
  `(destructuring-bind (,x ,y ,w ,h) ,rect
     (declare (ignorable ,x ,y ,w ,h))
     ,@body))

(defun delta-weight (width height rect)
  (with-rect (x y w h) rect
    (min (- w width) (- h height))))

(defun find-free-rect (width height rects)
  (loop :with min-rect = (car rects)
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
  (flet ((sort-by (list fn)
           (stable-sort list #'> :key (lambda (x) (apply fn (rest x))))))
    (sort-by (sort-by rects #'min) #'max)))

(defun pack-rects (rects width height)
  (loop :with free-rects = (list (list 0 0 width height))
        :for (id rect-width rect-height) :in (sort-rects rects)
        :collect (multiple-value-bind (rect new-free-rects)
                     (place-rect rect-width rect-height free-rects)
                   (setf free-rects new-free-rects)
                   (with-rect (x y w h) rect
                     (list id x y w h)))))

(defun load-image (file)
  (opticl:read-image-file file))

(defun collect-rects (path &key recursivep)
  (let ((rects))
    (fs-utils:map-files
     path
     (lambda (x)
       (destructuring-bind (w h &rest ignore) (array-dimensions (load-image x))
         (declare (ignore ignore))
         (push (list x w h) rects)))
     :recursivep recursivep)
    (nreverse rects)))

(defun write-data (data out-file)
  (let ((data-file (make-pathname :defaults out-file :type "sexp"))
        (data (sort data #'string< :key (lambda (x) (getf x :id)))))
    (with-open-file (out data-file
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (write data :stream out))))

(defun make-atlas (rects &key out-file width height)
  (loop :with atlas = (opticl:make-8-bit-rgba-image width height)
        :for (file x y w h) :in (pack-rects rects width height)
        :for id = (pathname-name file)
        :for sprite = (opticl:coerce-image (load-image file) 'opticl:rgba-image)
        :do (opticl:do-pixels (i j) sprite
              (setf (opticl:pixel atlas (+ i x) (+ j y))
                    (opticl:pixel sprite i j)))
        :collect (list :id id :x x :y y :w w :h h) :into data
        :finally (opticl:write-image-file out-file atlas)
                 (return (write-data data out-file))))
