(in-package #:%syntex.image)

(declaim (inline %make-image))
(defstruct (image
            (:constructor %make-image)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (width 0 :type u:ub16)
  (height 0 :type u:ub16)
  (data (u:make-ub32-array 0) :type u:ub32a))

(defun make-image (file-path)
  (let ((png (png:load-file file-path :flatten t)))
    (%make-image :width (png:width png)
                 :height (png:height png)
                 :data (pack png (png:color-type png)))))

(defgeneric pack (png color-type)
  (:method (png color-type)
    (error "Unsupported image file.")))

(defmethod pack ((png png:png) (color-type (eql :greyscale)))
  (declare (optimize speed))
  (loop :with width :of-type u:ub16 = (png:width png)
        :with height :of-type u:ub16 = (png:height png)
        :with size = (* width height)
        :with data :of-type u:ub8a = (png:data png)
        :with packed-data = (u:make-ub32-array size)
        :for i :below size
        :for value = (aref data i)
        :for pixel = 0
        :do (setf (ldb (byte 8 16) pixel) value
                  (ldb (byte 8 8) pixel) value
                  (ldb (byte 8 0) pixel) value
                  (ldb (byte 8 24) pixel) 255
                  (aref packed-data i) pixel)
        :finally (return packed-data)))

(defmethod pack (png (color-type (eql :greyscale-alpha)))
  (declare (optimize speed))
  (loop :with width :of-type u:ub16 = (png:width png)
        :with height :of-type u:ub16 = (png:height png)
        :with size = (* width height)
        :with data :of-type u:ub8a = (png:data png)
        :with packed-data = (u:make-ub32-array size)
        :for i :below (* size 2) :by 2
        :for j :from 0
        :for value = (aref data i)
        :for pixel = 0
        :do (setf (ldb (byte 8 16) pixel) value
                  (ldb (byte 8 8) pixel) value
                  (ldb (byte 8 0) pixel) value
                  (ldb (byte 8 24) pixel) (aref data (1+ i))
                  (aref packed-data j) pixel)
        :finally (return packed-data)))

(defmethod pack (png (color-type (eql :truecolour)))
  (declare (optimize speed))
  (loop :with width :of-type u:ub16 = (png:width png)
        :with height :of-type u:ub16 = (png:height png)
        :with size = (* width height)
        :with data :of-type u:ub8a = (png:data png)
        :with packed-data = (u:make-ub32-array size)
        :for i :below (* size 3) :by 3
        :for j :from 0
        :for pixel = 0
        :do (setf (ldb (byte 8 16) pixel) (aref data i)
                  (ldb (byte 8 8) pixel) (aref data (+ i 1))
                  (ldb (byte 8 0) pixel) (aref data (+ i 2))
                  (ldb (byte 8 24) pixel) 255
                  (aref packed-data j) pixel)
        :finally (return packed-data)))

(defmethod pack (png (color-type (eql :truecolour-alpha)))
  (declare (optimize speed))
  (loop :with width :of-type u:ub16 = (png:width png)
        :with height :of-type u:ub16 = (png:height png)
        :with size = (* width height)
        :with data :of-type u:ub8a = (png:data png)
        :with packed-data = (u:make-ub32-array size)
        :for i :below (* size 4) :by 4
        :for j :from 0
        :for pixel = 0
        :do (setf (ldb (byte 8 16) pixel) (aref data i)
                  (ldb (byte 8 8) pixel) (aref data (+ i 1))
                  (ldb (byte 8 0) pixel) (aref data (+ i 2))
                  (ldb (byte 8 24) pixel) (aref data (+ i 3))
                  (aref packed-data j) pixel)
        :finally (return packed-data)))

(u:fn-> from-argb (u:ub32) (values u:ub8 u:ub8 u:ub8 u:ub8))
(declaim (inline from-argb))
(defun from-argb (color)
  (declare (optimize speed))
  (values (ldb (byte 8 16) color)
          (ldb (byte 8 8) color)
          (ldb (byte 8 0) color)
          (ldb (byte 8 24) color)))

(u:fn-> unpack (u:ub32a u:ub16 u:ub16) u:ub8a)
(declaim (inline unpack))
(defun unpack (data width height)
  (declare (optimize speed))
  (let ((unpacked-data (u:make-ub8-array (* width height 4))))
    (dotimes (i (length data))
      (u:mvlet ((r g b a (from-argb (aref data i)))
                (offset (* i 4)))
        (setf (aref unpacked-data offset) r
              (aref unpacked-data (+ offset 1)) g
              (aref unpacked-data (+ offset 2)) b
              (aref unpacked-data (+ offset 3)) a)))
    unpacked-data))

(u:fn-> write (u:ub8a u:ub16 u:ub16 (or pathname string)) (values))
(defun write (data width height file-path)
  (declare (optimize speed))
  (let ((png (make-instance 'zpng:png
                            :color-type :truecolor-alpha
                            :width width
                            :height height
                            :image-data data)))
    (zpng:write-png png file-path)
    (values)))
