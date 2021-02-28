(in-package #:syntex)

(defgeneric make-image-data (png size color-type)
  (:method (png size color-type)
    (error "Unsupported image file.")))

(defmethod make-image-data (png size (color-type (eql :greyscale)))
  (loop :with png-data = (pngload:data png)
        :with image-data = (make-array size :element-type 'u:ub32)
        :for i :below size
        :for value = (aref png-data i)
        :for pixel = 0
        :do (setf (ldb (byte 8 16) pixel) value
                  (ldb (byte 8 8) pixel) value
                  (ldb (byte 8 0) pixel) value
                  (ldb (byte 8 24) pixel) 255
                  (aref image-data i) pixel)
        :finally (return image-data)))

(defmethod make-image-data (png size (color-type (eql :greyscale-alpha)))
  (loop :with png-data = (pngload:data png)
        :with image-data = (make-array size :element-type 'u:ub32)
        :for i :below (* size 2) :by 2
        :for j :from 0
        :for value = (aref png-data i)
        :for pixel = 0
        :do (setf (ldb (byte 8 16) pixel) value
                  (ldb (byte 8 8) pixel) value
                  (ldb (byte 8 0) pixel) value
                  (ldb (byte 8 24) pixel) (aref png-data (1+ i))
                  (aref image-data j) pixel)
        :finally (return image-data)))

(defmethod make-image-data (png size (color-type (eql :truecolour)))
  (loop :with png-data = (pngload:data png)
        :with image-data = (make-array size :element-type 'u:ub32)
        :for i :below (* size 3) :by 3
        :for j :from 0
        :for pixel = 0
        :do (setf (ldb (byte 8 16) pixel) (aref png-data i)
                  (ldb (byte 8 8) pixel) (aref png-data (+ i 1))
                  (ldb (byte 8 0) pixel) (aref png-data (+ i 2))
                  (ldb (byte 8 24) pixel) 255
                  (aref image-data j) pixel)
        :finally (return image-data)))

(defmethod make-image-data (png size (color-type (eql :truecolour-alpha)))
  (loop :with png-data = (pngload:data png)
        :with image-data = (make-array size :element-type 'u:ub32)
        :for i :below (* size 4) :by 4
        :for j :from 0
        :for pixel = 0
        :do (setf (ldb (byte 8 16) pixel) (aref png-data i)
                  (ldb (byte 8 8) pixel) (aref png-data (+ i 1))
                  (ldb (byte 8 0) pixel) (aref png-data (+ i 2))
                  (ldb (byte 8 24) pixel) (aref png-data (+ i 3))
                  (aref image-data j) pixel)
        :finally (return image-data)))

;;; factored out functions here for now

;; old
(defun a0 (state)
  (dotimes (i (* (state-sample-width state) (state-sample-height state)))
    (let ((colors (state-colors state))
          (color (aref (state-sample-data state) i))
          (j 0))
      (dotimes (k (length colors))
        (let ((c (aref colors k)))
          (when (= c color)
            (return))
          (incf j)))
      (when (= j (length colors))
        (vector-push-extend color colors))
      (setf (aref (state-indexed-sample state) i) j))))

;; old
(declaim (inline a1))
(defun a1 (state)
  (let* ((colors (state-colors state))
         (color-count (length colors))
         (metric nil))
    (when (and (not (state-indexed-p state)) (<= color-count 1024))
      (setf metric (make-array (list color-count color-count) :element-type 'u:f64))
      (dotimes (x color-count)
        (dotimes (y color-count)
          (let ((cx (aref colors x))
                (cy (aref colors y)))
            (setf (aref metric x y) (color-metric cx cy))))))
    metric))

;; new
#++(defun a0 (state)
     (let ((colors (state-colors2 state))
           (color-index 0))
       (dotimes (i (* (state-sample-width state) (state-sample-height state)))
         (let ((color (aref (state-sample-data state) i)))
           (unless (u:href colors color)
             (setf (u:href colors color) color-index))
           (setf (aref (state-indexed-sample state) i) (u:href colors color))))))

#++(defun a1 (state)
     (let* ((colors (state-colors2 state))
            (color-count (hash-table-count colors))
            (metric nil))
       (when (and (not (state-indexed-p state)) (<= color-count 1024))
         (setf metric (make-array (list color-count color-count) :element-type 'u:f64))
         (u:do-hash (cx x (state-colors2 state))
           (u:do-hash (cy y (state-colors2 state))
             (setf (aref metric x y) (color-metric cx cy)))))
       metric))

(defun a2 (state neighbors neighbor-count index)
  (let* ((output-width (state-output-width state))
         (output-height (state-output-height state))
         (x (make-array 4 :element-type 'u:b32))
         (y (make-array 4 :element-type 'u:b32))
         (ix (rem index output-width))
         (iy (floor index output-width)))
    (declare (dynamic-extent x y))
    (loop :with found = 0
          :while (< found neighbor-count)
          :for radius :of-type u:ub32 :from 1
          :do (setf (aref x 0) (- ix radius)
                    (aref y 0) (- iy radius)
                    (aref x 1) (- ix radius)
                    (aref y 1) (+ iy radius)
                    (aref x 2) (+ ix radius)
                    (aref y 2) (+ iy radius)
                    (aref x 3) (+ ix radius)
                    (aref y 3) (- iy radius))
              (dotimes (i (* radius 2))
                (dotimes (j 4)
                  (setf (aref x j) (rem (+ (aref x j) (* output-width 10)) output-width)
                        (aref y j) (rem (+ (aref y j) (* output-height 10)) output-height))
                  (unless (>= found neighbor-count)
                    (let ((point (+ (* (aref y j) output-width) (aref x j))))
                      (unless (minusp (aref (state-origins state) point))
                        (setf (aref neighbors found) point)
                        (incf found)))))
                (incf (aref y 0))
                (incf (aref x 1))
                (decf (aref y 2))
                (decf (aref x 3))))))

(defun a3 (state neighbors neighbor-count index candidates)
  (dotimes (i neighbor-count)
    (let* ((sample-width (state-sample-width state))
           (sample-height (state-sample-height state))
           (output-width (state-output-width state))
           (origins (state-origins state))
           (neighbor (aref neighbors i))
           (cx (rem (+ (aref origins neighbor)
                       (rem (- index neighbor) output-width)
                       (* sample-width 100))
                    sample-width))
           (cy (rem (+ (- (+ (floor (aref origins neighbor) sample-width)
                             (floor index output-width))
                          (floor neighbor output-width))
                       (* sample-height 100))
                    sample-height)))
      (setf (aref candidates i) (+ (* cy sample-width) cx)))))

(declaim (inline a5))
(defun a5 (state candidates index metric)
  (let ((sample-width (state-sample-width state))
        (sample-height (state-sample-height state))
        (sample-data (state-sample-data state))
        (indexed-p (state-indexed-p state))
        (kernel-size (state-kernel-size state))
        (output-width (state-output-width state))
        (output-height (state-output-height state))
        (origins (state-origins state))
        (rng (state-rng state))
        (max -1d10)
        (arg-max -1))
    (dotimes (c (length candidates))
      (let ((sum (* (rng:float rng 0.0 1.0) 1d-6)))
        (declare (u:f64 sum))
        (loop :for dy :from (- kernel-size) :to kernel-size
              :do (loop :with origin = 0
                        :with si = 0
                        :with fi = 0
                        :with ix = (rem (aref candidates c) sample-width)
                        :with iy = (floor (aref candidates c) sample-width)
                        :with jx = (rem index output-width)
                        :with jy = (floor index output-width)
                        :for dx :from (- kernel-size) :to kernel-size
                        :for sx = (+ ix dx)
                        :for sy = (+ iy dy)
                        :for fx = (+ jx dx)
                        :for fy = (+ jy dy)
                        :when (or (/= dx 0) (/= dy 0))
                          :do (cond
                                ((minusp sx)
                                 (incf sx sample-width))
                                ((>= sx sample-width)
                                 (decf sx sample-width)))
                              (cond
                                ((minusp sy)
                                 (incf sy sample-height))
                                ((>= sy sample-height)
                                 (decf sy sample-height)))
                              (cond
                                ((minusp fx)
                                 (incf fx output-width))
                                ((>= fx output-width)
                                 (decf fx output-width)))
                              (cond
                                ((minusp fy)
                                 (incf fy output-height))
                                ((>= fy output-height)
                                 (decf fy output-height)))
                              (setf si (+ (* sy sample-width) sx)
                                    fi (+ (* fy output-width) fx)
                                    origin (aref origins fi))
                              (unless (minusp origin)
                                (cond
                                  (indexed-p
                                   (if (= (aref sample-data origin)
                                          (aref sample-data si))
                                       (incf sum)
                                       (decf sum)))
                                  (metric
                                   (incf sum
                                         (aref metric
                                               (aref (state-indexed-sample state) origin)
                                               (aref (state-indexed-sample state) si))))
                                  (t
                                   (incf sum (color-metric (aref sample-data origin)
                                                           (aref sample-data si))))))))
        (when (>= sum max)
          (setf max sum
                arg-max (aref candidates c)))))
    (setf (aref origins index) arg-max)))

(defun a6 (state)
  (let* ((sample-data (state-sample-data state))
         (width (state-output-width state))
         (height (state-output-height state))
         (output-size (* width height))
         (result (make-array output-size :element-type 'u:ub32)))
    (dotimes (i (length result))
      (setf (aref result i) (aref sample-data (aref (state-origins state) i))))
    (write-image result width height (state-output-path state))))

(u:fn-> make-shuffled-indices (rng:generator u:ub16 u:ub16) (simple-array u:ub32))
(declaim (inline make-shuffled-indices))
(defun make-shuffled-indices (rng width height)
  (let* ((count (* width height))
         (result (make-array count :element-type 'u:ub32)))
    (dotimes (i count)
      (setf (aref result i) i))
    (values (rng:shuffle rng result))))
