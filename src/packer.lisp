(in-package :box.sprite-packer)

(defclass rect (binpack:rect)
  ((file :reader file
         :initarg :file)))

(defmethod binpack:rect-initargs append ((r rect))
  (list :file (file r)))

(defun rect (file id x y w h)
  (make-instance 'rect :file file :id id :x x :y y :w w :h h))


(defun make-id (root file)
  (namestring
   (make-pathname
    :defaults
    (uiop/pathname:enough-pathname file (uiop/pathname:ensure-directory-pathname root))
    :type nil)))

(defun map-files (path effect &key (filter (constantly t)) (recursive t))
  (labels ((maybe-affect (file)
             (when (funcall filter file)
               (funcall effect file)))
           (process-files (dir)
             (map nil #'maybe-affect (uiop/filesystem:directory-files dir))))
    (uiop/filesystem:collect-sub*directories
     (uiop/pathname:ensure-directory-pathname path)
     t recursive #'process-files)))

(defun collect-files (path &key recursive)
  (let ((files))
    (map-files
     path
     (lambda (x) (push (cons x (make-id path x)) files))
     :recursive recursive)
    (reverse files)))

(defun make-rects (files)
  (loop :for (file . id) :in files
        :for image = (pngload:load-file file :decode nil)
        :for width = (pngload:width image)
        :for height = (pngload:height image)
        :collect (rect file id 0 0 width height)))

(defun add-padding (rects padding)
  (when (and padding (plusp padding))
    (dolist (rect rects)
      (incf (slot-value rect '%w) padding)
      (incf (slot-value rect '%h) padding)))
  rects)

(defun remove-padding (rects padding)
  (when (and padding (plusp padding))
    (loop :with padding/2 = (floor padding 2)
          :for rect :in rects
          :do (incf (slot-value rect '%x) padding/2)
              (incf (slot-value rect '%y) padding/2)
              (decf (slot-value rect '%w) padding)
              (decf (slot-value rect '%h) padding)))
  rects)

(defgeneric make-coords (rect width height normalize flip-y)
  (:method (rect width height normalize flip-y)
    (with-slots (%y %h) rect
      (let ((y (if flip-y (- height %y %h) %y)))
        (list :x (x rect) :y y :w (w rect) :h (h rect)))))
  (:method (rect width height (normalize (eql t)) flip-y)
    (with-slots (%y %h) rect
      (let ((y (if flip-y (- height %y %h) %y)))
        (list :x (float (/ (x rect) width))
              :y (float (/ y height))
              :w (float (/ (w rect) width))
              :h (float (/ (h rect) height)))))))

(defun write-atlas (atlas sprite rect)
  (let ((sprite (opticl:coerce-image sprite 'opticl:rgba-image)))
    (opticl:do-pixels (i j) sprite
      (setf (opticl:pixel atlas (+ i (y rect)) (+ j (x rect)))
            (opticl:pixel sprite i j)))))

(defun write-metadata (data out-file)
  (let ((out-file (make-pathname :defaults out-file :type "spec"))
        (data (sort data #'string< :key (lambda (x) (getf x :id)))))
    (with-open-file (out out-file
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (write data :stream out))))

(defun make-atlas (file-spec &key out-file width height normalize flip-y
                               (padding 0) (optimize-pack nil)
                               (auto-size-granularity-x 1)
                               (auto-size-granularity-y 1))
  "Pack the sprites defined by FILE-SPEC into a spritesheet.

OUT-FILE: A pathname specifying where to write the image file.

WIDTH: The width in pixels of the spritesheet. :AUTO to calculate width automatically.

HEIGHT: The height in pixels of the spritesheet. :AUTO to calculate height automatically.

NORMALIZE: Boolean specifying whether to map the metadata's coordinates to the [0..1] range.

FLIP-Y: Boolean specifying whether to flip the Y axis when writing the metadata.

PADDING: The padding in pixels to use around each sprite in the spritesheet.

OPTIMIZE-PACK: Calculate size automatically, and try multiple sizes to find a better size. (ignores WIDTH, HEIGHT if set)

AUTO-SIZE-GRANULARITY-X,
AUTO-SIZE-GRANULARITY-Y: Automaticallyy generated sizes will be multiples of these.

See MAKE-ATLAS-FROM-DIRECTORY if you want to automatically generate FILE-SPEC from the files under a
given filesystem path.
"
  (loop :with atlas = (opticl:make-8-bit-rgba-image width height)
        :with rects = (add-padding (make-rects file-spec) padding)
        :for rect :in (remove-padding
                       (binpack:auto-pack
                        rects
                        :width width :height height
                        :auto-size-granularity-x auto-size-granularity-x
                        :auto-size-granularity-y auto-size-granularity-y
                        :optimize-pack optimize-pack)
                       padding)
        :for sprite = (opticl:read-png-file (file rect))
        :for coords = (make-coords rect width height normalize flip-y)
        :do (write-atlas atlas sprite rect)
        :collect `(:id ,(id rect) ,@coords) :into data
        :finally (return
                   (values (write-metadata data out-file)
                           (opticl:write-image-file out-file atlas)))))

(defun make-atlas-from-directory (path &key recursive out-file width height normalize flip-y
                                         (padding 0)
                                         (auto-size-granularity-x 1)
                                         (auto-size-granularity-y 1)
                                         (optimize-pack nil))
  "Pack the sprites located under the given filesystem path, PATH.

RECURSIVE: Boolean specifying whether to scan recursively for files.

OUT-FILE: A pathname specifying where to write the image file.

WIDTH: The width in pixels of the spritesheet. :AUTO to calculate width automatically.

HEIGHT: The height in pixels of the spritesheet. :AUTO to calculate height automatically.

NORMALIZE: Boolean specifying whether to normalize the metadata's coordinates in the [0..1] range.

FLIP-Y: Boolean specifying whether to flip the Y axis when writing the metadata.

PADDING: The padding in pixels to use around each sprite in the spritesheet.

OPTIMIZE-PACK: Calculate size automatically, and try multiple sizes to find a better size. (ignores WIDTH, HEIGHT if set)

AUTO-SIZE-GRANULARITY-X,
AUTO-SIZE-GRANULARITY-Y: Automaticallyy generated sizes will be multiples of these.

See MAKE-ATLAS if you want to manually specify a file-spec, in case you want to be in control of the
names chosen to identify the sprites written to the metadata file.
"
  (let ((file-spec (collect-files path :recursive recursive)))
    (make-atlas file-spec
                :out-file out-file
                :width width
                :height height
                :normalize normalize
                :flip-y flip-y
                :padding padding
                :auto-size-granularity-x auto-size-granularity-x
                :auto-size-granularity-y auto-size-granularity-y
                :optimize-pack optimize-pack)))
