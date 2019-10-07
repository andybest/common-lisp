(in-package #:patchwork)

(defun make-sprite-path (directory id)
  (uiop/pathname:merge-pathnames*
   (uiop/pathname:ensure-directory-pathname directory)
   (make-pathname :defaults id :type "png")))

(defgeneric %make-atlas-coords (atlas data normalized))

(defmethod %make-atlas-coords (atlas data (normalized (eql t)))
  (destructuring-bind (&key id x y w h y-inverted &allow-other-keys) data
    (when (and id x y w h)
      (let* ((atlas-height (array-dimension atlas 0))
             (y (if y-inverted (- atlas-height y h) y)))
        (apply #'make-instance 'rect :y y data)))))

(defmethod %make-atlas-coords (atlas data normalized)
  (destructuring-bind (&key id x y w h y-inverted &allow-other-keys) data
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
        (when y-inverted
          (setf (y rect) (- atlas-height (y rect) (h rect))))
        rect))))

(defun make-atlas-coords (atlas data)
  (destructuring-bind (&key normalized &allow-other-keys) data
    (%make-atlas-coords atlas data normalized)))

(defun write-sprite (atlas rect out-file)
  (binpack:with-rect (nil x y w h) rect
    (let ((sprite (make-array (list* h w
                                     (when (> (array-rank atlas) 2)
                                       (list (array-dimension atlas 2))))
                              :element-type (array-element-type atlas))))
      (opticl:do-pixels (i j) sprite
        (setf (opticl:pixel sprite i j)
              (opticl:pixel atlas (+ i y) (+ j x))))
      (opticl:write-image-file out-file sprite))))

(defun unpack-sprite (atlas data out-path)
  (let ((rect (make-atlas-coords atlas data))
        (out-file (make-sprite-path out-path (getf data :id))))
    (write-sprite atlas rect out-file)))

(defun unpack-atlas (atlas-file &key out-path)
  "Unpack the sprites contained in the image, specified by the filesystem path, ATLAS-FILE. A file
  of the same name with a \"spec\" file extension must also exist in the same directory on the
  filesystem.

OUT-PATH: A pathname specifying a directory to write all the sprite images to."
  (loop :with atlas = (opticl:read-image-file atlas-file)
        :with spec-file = (make-pathname :defaults atlas-file :type "spec")
        :for data :in (uiop/stream:safe-read-file-form spec-file)
        :do (unpack-sprite atlas data out-path)))
