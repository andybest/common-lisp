(in-package #:%syntex.image)

(defstruct (image
            (:constructor %make-image)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (width 0 :type u:ub16)
  (height 0 :type u:ub16)
  (data (make-array 0 :element-type 'u:ub32) :type (simple-array u:ub32)))

(defstruct (argb-image
            (:include image)
            (:constructor %make-argb-image)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (argb-data (make-array 0 :element-type 'u:ub32) :type (simple-array u:ub32)))

(defun make-argb-image (file-path)
  (let ((png (pngload:load-file file-path :flatten t)))
    (%make-argb-image :width (png:width png)
                      :height (png:height png)
                      :argb-data (pack-argb png (get-channel-count png)))))

(defun get-channel-count (png)
  (ecase (png:color-type png)
    (:greyscale 1)
    (:greyscale-alpha 2)
    (:truecolour 3)
    (:truecolour-alpha 4)))

(defgeneric pack-argb (png color-type)
  (:method (png color-type)
    (error "Unsupported image file.")))

(defmethod pack-argb (png (channel-count (eql 1)))
  (loop :with size = (* (png:width png) (png:height png))
        :with data = (png:data png)
        :with packed-data = (make-array size :element-type 'u:ub32)
        :for i :below size
        :for value = (aref data i)
        :for pixel = 0
        :do (setf (ldb (byte 8 16) pixel) value
                  (ldb (byte 8 8) pixel) value
                  (ldb (byte 8 0) pixel) value
                  (ldb (byte 8 24) pixel) 255
                  (aref packed-data i) pixel)
        :finally (return packed-data)))

(defmethod pack-argb (png (channel-count (eql 2)))
  (loop :with size = (* (png:width png) (png:height png))
        :with data = (png:data png)
        :with packed-data = (make-array size :element-type 'u:ub32)
        :for i :below (* size channel-count) :by channel-count
        :for j :from 0
        :for value = (aref data i)
        :for pixel = 0
        :do (setf (ldb (byte 8 16) pixel) value
                  (ldb (byte 8 8) pixel) value
                  (ldb (byte 8 0) pixel) value
                  (ldb (byte 8 24) pixel) (aref data (1+ i))
                  (aref packed-data j) pixel)
        :finally (return packed-data)))

(defmethod pack-argb (png (channel-count (eql 3)))
  (loop :with size = (* (png:width png) (png:height png))
        :with data = (png:data png)
        :with packed-data = (make-array size :element-type 'u:ub32)
        :for i :below (* size channel-count) :by channel-count
        :for j :from 0
        :for pixel = 0
        :do (setf (ldb (byte 8 16) pixel) (aref data i)
                  (ldb (byte 8 8) pixel) (aref data (+ i 1))
                  (ldb (byte 8 0) pixel) (aref data (+ i 2))
                  (ldb (byte 8 24) pixel) 255
                  (aref packed-data j) pixel)
        :finally (return packed-data)))

(defmethod pack-argb (png (channel-count (eql 4)))
  (loop :with size = (* (png:width png) (png:height png))
        :with data = (png:data png)
        :with packed-data = (make-array size :element-type 'u:ub32)
        :for i :below (* size channel-count) :by channel-count
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

(defun unpack-argb (data width height)
  (let ((unpacked-data (make-array (* width height 4) :element-type 'u:ub8)))
    (dotimes (i (length data))
      (u:mvlet ((r g b a (from-argb (aref data i)))
                (offset (* i 4)))
        (setf (aref unpacked-data offset) r
              (aref unpacked-data (+ offset 1)) g
              (aref unpacked-data (+ offset 2)) b
              (aref unpacked-data (+ offset 3)) a)))
    unpacked-data))

(defun write-image (data width height file-path)
  (let ((png (make-instance 'zpng:png
                            :color-type :truecolor-alpha
                            :width width
                            :height height
                            :image-data data)))
    (zpng:write-png png file-path)))
