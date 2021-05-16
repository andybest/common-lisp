(in-package #:cl-user)

(defpackage #:%syntex.wfc.image
  (:local-nicknames
   (#:png #:pngload)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:data
   #:image
   #:height
   #:make-image
   #:unpack
   #:width))

(in-package #:%syntex.wfc.image)

(defstruct (image
            (:constructor %make-image)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (width nil :type u:positive-fixnum)
  (height nil :type u:positive-fixnum)
  (data (u:make-ub32-array 0) :type u:ub32a))

(defun make-image (file-path)
  (let ((png (png:load-file file-path :flatten t)))
    (%make-image :width (png:width png)
                 :height (png:height png)
                 :data (pack png (png:color-type png)))))

(defgeneric pack (png color-type))

(defmethod pack ((png png:png) (color-type (eql :greyscale)))
  (loop :with size = (* (png:width png) (png:height png))
        :with data = (png:data png)
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
  (loop :with size = (* (png:width png) (png:height png))
        :with data = (png:data png)
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
  (loop :with size = (* (png:width png) (png:height png))
        :with data = (png:data png)
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
  (loop :with size = (* (png:width png) (png:height png))
        :with data = (png:data png)
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

(defun unpack (image)
  (let ((data (data image))
        (unpacked-data (u:make-ub8-array (* (width image) (height image) 4))))
    (dotimes (i (length data))
      (let ((color (aref data i))
            (offset (* i 4)))
        (setf (aref unpacked-data offset) (ldb (byte 8 16) color)
              (aref unpacked-data (+ offset 1)) (ldb (byte 8 8) color)
              (aref unpacked-data (+ offset 2)) (ldb (byte 8 0) color)
              (aref unpacked-data (+ offset 3)) (ldb (byte 8 24) color))))
    unpacked-data))
