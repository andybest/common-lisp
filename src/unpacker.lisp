(in-package :gamebox-sprite-packer)

(defun make-sprite-path (directory id)
  (uiop/pathname:merge-pathnames*
   (uiop/pathname:ensure-directory-pathname directory)
   (make-pathname :defaults id :type "png")))

(defgeneric make-atlas-coords (atlas data denormalize flip-y)
  (:method (atlas data denormalize flip-y)
    (destructuring-bind (&key id x y w h) data
      (when (and id x y w h)
        (let* ((atlas-height (array-dimension atlas 0))
               (y (if flip-y (- atlas-height y h) y)))
          (apply #'make-instance 'rect :y y data))))))

(defmethod make-atlas-coords (atlas data (denormalize (eql t)) flip-y)
  (destructuring-bind (&key id x y w h) data
    (when (and id x y w h)
      (let* ((atlas-width (array-dimension atlas 1))
             (atlas-height (array-dimension atlas 0))
             (rect (make-instance
                    'rect
                    :id id
                    :x (round (* atlas-width x))
                    :y (round (* atlas-height y))
                    :w (round (* atlas-width w))
                    :h (round (* atlas-height h)))))
        (with-slots (y h) rect
          (when flip-y
            (setf y (- atlas-height y h))))
        rect))))

(defun write-sprite (atlas rect out-file)
  (with-slots (x y w h) rect
    (let ((sprite (opticl:make-8-bit-rgba-image h w)))
      (opticl:do-pixels (i j) sprite
        (setf (opticl:pixel sprite i j)
              (opticl:pixel atlas (+ i y) (+ j x))))
      (opticl:write-image-file out-file sprite))))

(defun unpack-sprite (atlas data denormalize flip-y out-path)
  (let ((rect (make-atlas-coords atlas data denormalize flip-y))
        (out-file (make-sprite-path out-path (getf data :id))))
    (write-sprite atlas rect out-file)))

(defun unpack-atlas (atlas-file &key out-path denormalize flip-y)
  (loop :with atlas = (opticl:read-png-file atlas-file)
        :with spec-file = (make-pathname :defaults atlas-file :type "spec")
        :for data :in (fs-utils:read-file spec-file)
        :do (unpack-sprite atlas data denormalize flip-y out-path)))
