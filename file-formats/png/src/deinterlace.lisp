(in-package #:mfiano.file-formats.png)

(alexandria:define-constant +adam7-pattern+
  #2a((1 6 4 6 2 6 4 6)
      (7 7 7 7 7 7 7 7)
      (5 6 5 6 5 6 5 6)
      (7 7 7 7 7 7 7 7)
      (3 6 4 6 3 6 4 6)
      (7 7 7 7 7 7 7 7)
      (5 6 5 6 5 6 5 6)
      (7 7 7 7 7 7 7 7))
  :test #'equalp)

(alexandria:define-constant +adam7-widths+
    #(#(0 1 1 1 1 1 1 1 1)
      #(0 0 0 0 0 1 1 1 1)
      #(0 1 1 1 1 2 2 2 2)
      #(0 0 0 1 1 1 1 2 2)
      #(0 1 1 2 2 3 3 4 4)
      #(0 0 1 1 2 2 3 3 4)
      #(0 1 2 3 4 5 6 7 8))
  :test #'equalp)

(alexandria:define-constant +adam7-heights+
    #(#(0 1 1 1 1 1 1 1 1)
      #(0 1 1 1 1 1 1 1 1)
      #(0 0 0 0 0 1 1 1 1)
      #(0 1 1 1 1 2 2 2 2)
      #(0 0 0 1 1 1 1 2 2)
      #(0 1 1 2 2 3 3 4 4)
      #(0 0 1 1 2 2 3 3 4))
  :test #'equalp)

(defun calculate-sub-image-dimensions (png)
  (loop :for pass :below 7
        :collect
        (flet ((calc (dim array)
                 (multiple-value-bind (w e) (floor dim 8)
                   (+ (* (aref (aref array pass) 8) w)
                      (aref (aref array pass) e)))))
          (list (calc (width png) +adam7-widths+)
                (calc (height png) +adam7-heights+)))))

(defun add-sub-image/sub-byte (png dest source pass w h pixel-bits start)
  (loop :with dx1 = (1- (position 1 (aref +adam7-widths+ pass)))
        :with dy1 = (1- (position 1 (aref +adam7-heights+ pass)))
        :with ddx = (/ 8 (aref (aref +adam7-widths+ pass) 8))
        :with ddy = (/ 8 (aref (aref +adam7-heights+ pass) 8))
        :with ssb = (get-scanline-bytes png w)
        :with dsb = (get-scanline-bytes png (width png))
        :with pixels-per-byte = (/ 8 pixel-bits)
        :for sy :below h
        :for dy :from dy1 :by ddy
        :for dyb = (* dy dsb)
        :for syb = (+ start (* sy ssb))
        :do (loop :for sx :below w
                  :for dx :from dx1 :by ddx
                  :do (multiple-value-bind (sxb sxp) (floor sx pixels-per-byte)
                        (multiple-value-bind (dxb dxp)
                            (floor dx pixels-per-byte)
                          (setf (ldb (byte pixel-bits
                                           (- 8 (* dxp pixel-bits) pixel-bits))
                                     (aref dest (+ dyb dxb)))
                                (ldb (byte pixel-bits
                                           (- 8 (* sxp pixel-bits) pixel-bits))
                                     (aref source (+ syb sxb)))))))))

(defun add-sub-image (png dest source pass w h pixel-bytes start)
  (loop :with x1 = (1- (position 1 (aref +adam7-widths+ pass)))
        :with y1 = (1- (position 1 (aref +adam7-heights+ pass)))
        :with dx = (/ 8 (aref (aref +adam7-widths+ pass) 8))
        :with dy = (/ 8 (aref (aref +adam7-heights+ pass) 8))
        :for sy :below h
        :for y :from y1 :by dy
        :for dyb = (* y (* (width png)) pixel-bytes)
        :for syb = (+ start (* sy w pixel-bytes))
        :do (loop :for sx :below (* pixel-bytes w) :by pixel-bytes
                  :for x :from (* x1 pixel-bytes) :by (* dx pixel-bytes)
                  :do (loop :for i :below pixel-bytes
                            :do (setf (aref dest (+ dyb x i))
                                      (aref source (+ syb sx i)))))))

(defun deinterlace-adam7 (png data)
  (loop :with width = (width png)
        :with height = (height png)
        :with pixel-bits = (* (get-channel-count png) (bit-depth png))
        :with dest = (make-array (* height (ceiling (* width pixel-bits) 8))
                                 :element-type 'ub8
                                 :initial-element #xff)
        :for pass :below 7
        :for start = 0 :then next
        :for (sw sh) :in (calculate-sub-image-dimensions png)
        :for scanline-bytes = (get-scanline-bytes png sw)
        :for next = (+ start sh (* sh (ceiling (* sw pixel-bits) 8)))
        :when (zerop sw)
          :do (setf next start)
        :when (and (plusp sw) (plusp sh))
          :do (unfilter png data sw sh start)
              (if (< pixel-bits 8)
                  (add-sub-image/sub-byte png dest data pass sw sh pixel-bits
                                          start)
                  (add-sub-image png dest data pass sw sh (/ pixel-bits 8)
                                 start))
        :finally (return dest)))
