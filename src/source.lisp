(in-package #:syntex)

(defstruct (image
            (:constructor %make-image)
            (:predicate nil)
            (:copier nil))
  (base-name nil :type string)
  (width 0 :type u:ub16)
  (height 0 :type u:ub16)
  (data (make-array 0 :element-type 'u:ub32) :type (simple-array u:ub32)))

(defun make-image (file-path)
  (let* ((png (pngload:load-file file-path :flatten t))
         (width (pngload:width png))
         (height (pngload:height png))
         (data (make-image-data png (* width height) (pngload:color-type png))))
    (%make-image :base-name (pathname-name file-path)
                 :width width
                 :height height
                 :data data)))

(u:fn-> from-argb (u:ub32) (values u:ub8 u:ub8 u:ub8 u:ub8))
(defun from-argb (color)
  (declare (optimize speed))
  (values (ldb (byte 8 16) color)
          (ldb (byte 8 8) color)
          (ldb (byte 8 0) color)
          (ldb (byte 8 24) color)))

(u:fn-> color-metric (u:ub32 u:ub32) u:f64)
(declaim (inline color-metric))
(defun color-metric (color1 color2)
  (declare (u:ub32 color1 color2))
  (u:mvlet* ((r1 g1 b1 a1 (from-argb color1))
             (r2 g2 b2 a2 (from-argb color2))
             (r (1+ (* (expt (- r1 r2) 2) 7.62939453125d-7)))
             (g (1+ (* (expt (- g1 g2) 2) 7.62939453125d-7)))
             (b (1+ (* (expt (- b1 b2) 2) 7.62939453125d-7))))
    (- (log (* r g b)))))

(defstruct (state
            (:constructor %make-state)
            (:predicate nil)
            (:copier nil))
  (rng nil :type rng:generator)
  (sample-width 0 :type u:ub16)
  (sample-height 0 :type u:ub16)
  (sample-data (make-array 0 :element-type 'u:ub32) :type (simple-array u:ub32))
  (indexed-p nil :type boolean)
  (output-width 0 :type u:ub16)
  (output-height 0 :type u:ub16)
  (output-path nil :type (or pathname string))
  (colors (make-array 0 :element-type 'u:ub32 :fill-pointer 0 :adjustable t) :type vector)
  (colors2 (u:dict #'eql) :type hash-table)
  (indexed-sample (make-array 0 :element-type 'u:ub32) :type (simple-array u:ub32))
  (kernel-size 1 :type (and (integer 1) u:ub8))
  (origins (make-array 0 :element-type 'u:b32) :type (simple-array u:b32))
  (indices (make-array 0 :element-type 'u:ub32) :type (simple-array u:ub32)))

(defun make-state (image &key output-width output-height indexed-p kernel-size output-path seed)
  (let* ((rng (rng:make-generator seed))
         (sample-width (image-width image))
         (sample-height (image-height image))
         (sample-data (image-data image))
         (output-width (or output-width sample-width))
         (output-height (or output-height sample-height)))
    (%make-state :rng rng
                 :sample-width sample-width
                 :sample-height sample-height
                 :sample-data sample-data
                 :indexed-p indexed-p
                 :output-width output-width
                 :output-height output-height
                 :output-path output-path
                 :indexed-sample (make-array (length sample-data) :element-type 'u:ub32)
                 :kernel-size kernel-size
                 :origins (make-array (* output-width output-height)
                                      :element-type 'u:b32
                                      :initial-element -1)
                 :indices (make-shuffled-indices rng output-width output-height))))

(defun resynthesize (file-path &key indexed-p width height (rounds 3) (m 20) (kernel-size 1)
                                 seed output-path)
  (declare (optimize speed)
           (u:ub16 rounds m))
  (unless output-path
    (error "No output path specified."))
  (let* ((state (make-state (make-image file-path)
                            :indexed-p indexed-p
                            :output-width width
                            :output-height height
                            :output-path output-path
                            :kernel-size kernel-size
                            :seed seed))
         (indices (state-indices state))
         (lparallel:*kernel* (lparallel:make-kernel 8)))
    (a0 state)
    (format t "a0~%")
    (let ((metric (a1 state)))
      (format t "a1~%")
      (dotimes (round (1+ rounds))
        (declare (fixnum round))
        (unless (zerop round)
          (format t "Round: ~d~%" round))
        (lparallel:pdotimes (counter (length indices))
          (let* ((index (aref indices counter))
                 (neighbor-count (if (zerop round) (min 8 counter) 8))
                 (candidates (make-array (+ neighbor-count m) :element-type 'u:b32)))
            (when (plusp neighbor-count)
              (let ((neighbors (make-array neighbor-count :element-type 'u:b64)))
                (declare (dynamic-extent neighbors))
                (a2 state neighbors neighbor-count index)
                (a3 state neighbors neighbor-count index candidates)))
            (dotimes (i m)
              (let* ((max (* (state-sample-width state) (state-sample-height state)))
                     (x (rng:int (state-rng state) 0 max nil)))
                (setf (aref candidates (+ neighbor-count i)) x)))
            (a5 state candidates index metric))))
      (a6 state))))

(defun write-image (data width height file-path)
  (let* ((image-data (make-array (* width height 4) :element-type 'u:ub8))
         (png (make-instance 'zpng:png
                             :color-type :truecolor-alpha
                             :width width
                             :height height
                             :image-data image-data)))
    (dotimes (i (length data))
      (u:mvlet ((r g b a (from-argb (aref data i))))
        (setf (aref image-data (* i 4)) r
              (aref image-data (+ (* i 4) 1)) g
              (aref image-data (+ (* i 4) 2)) b
              (aref image-data (+ (* i 4) 3)) a)))
    (zpng:write-png png file-path)))
