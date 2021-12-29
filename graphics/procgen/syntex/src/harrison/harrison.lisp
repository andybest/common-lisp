(in-package #:%mfiano.graphics.procgen.syntex.harrison)

(defstruct (state
            (:constructor %make-state)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (rng nil :type rng:generator)
  (sample-width 0 :type u:ub16)
  (sample-height 0 :type u:ub16)
  (sample-data (u:make-ub32-array 0) :type u:ub32a)
  (indexed-p nil :type boolean)
  (output-width 0 :type u:ub16)
  (output-height 0 :type u:ub16)
  (output-path nil :type (or pathname string))
  (indexed-sample (u:make-ub32-array 0) :type u:ub32a)
  (kernel-size 1 :type (and (integer 1) u:ub8))
  (origins (u:make-b32-array 0) :type u:b32a)
  (indices (u:make-ub32-array 0) :type u:ub32a))

(defun make-state (image &key output-width output-height indexed-p kernel-size output-path seed)
  (let* ((rng (rng:make-generator seed))
         (sample-width (img:width image))
         (sample-height (img:height image))
         (sample-data (img:data image))
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
                 :indexed-sample (u:make-ub32-array (length sample-data))
                 :kernel-size kernel-size
                 :origins (u:make-b32-array (* output-width output-height) -1)
                 :indices (make-shuffled-indices rng output-width output-height))))

(defun make-shuffled-indices (rng width height)
  (let* ((count (* width height))
         (result (u:make-ub32-array count)))
    (dotimes (i count)
      (setf (aref result i) i))
    (values (rng:shuffle rng result))))

(u:fn-> color-metric (u:ub32 u:ub32) u:f64)
(declaim (inline color-metric))
(defun color-metric (color1 color2)
  (declare (u:ub32 color1 color2))
  (u:mvlet* ((r1 g1 b1 (img:from-rgb color1))
             (r2 g2 b2 (img:from-rgb color2))
             (r (1+ (* (expt (- r1 r2) 2) 7.62939453125d-7)))
             (g (1+ (* (expt (- g1 g2) 2) 7.62939453125d-7)))
             (b (1+ (* (expt (- b1 b2) 2) 7.62939453125d-7))))
    (- (log (* r g b)))))

(defun build-metrics (state)
  (declare (optimize speed))
  (let ((sample-data (sample-data state))
        (indexed-p (indexed-p state))
        (indexed-sample (indexed-sample state))
        (colors (u:dict #'eql))
        (color-count 0)
        (metric nil))
    (declare (fixnum color-count))
    (dotimes (i (* (sample-width state) (sample-height state)))
      (let ((color (aref sample-data i)))
        (unless (u:href colors color)
          (setf (u:href colors color) color-count)
          (incf color-count))
        (setf (aref indexed-sample i) (u:href colors color))))
    (when (and (not indexed-p) (<= color-count 1024))
      (setf metric (u:make-f64-array (list color-count color-count)))
      (u:do-hash (cx x colors)
        (u:do-hash (cy y colors)
          (setf (aref metric x y) (color-metric cx cy)))))
    metric))

(u:fn-> update-neighbors (state u:ub32a u:ub8 u:ub32) null)
(defun update-neighbors (state neighbors neighbor-count index)
  (declare (optimize speed))
  (let* ((output-width (output-width state))
         (output-height (output-height state))
         (origins (origins state))
         (x (u:make-b32-array 4))
         (y (u:make-b32-array 4))
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
                  (when (< found neighbor-count)
                    (let ((point (+ (* (aref y j) output-width) (aref x j))))
                      (unless (minusp (aref origins point))
                        (setf (aref neighbors found) point)
                        (incf found)))))
                (incf (aref y 0))
                (incf (aref x 1))
                (decf (aref y 2))
                (decf (aref x 3))))))

(u:fn-> update-candidates (state u:ub32a u:ub8 u:ub32 u:b32a) null)
(defun update-candidates (state neighbors neighbor-count index candidates)
  (declare (optimize speed))
  (let ((sample-width (sample-width state))
        (sample-height (sample-height state))
        (output-width (output-width state))
        (origins (origins state)))
    (dotimes (i neighbor-count)
      (let* ((neighbor (aref neighbors i))
             (cx (rem (+ (aref origins neighbor)
                         (rem (- index neighbor) output-width)
                         (* sample-width 100))
                      sample-width))
             (cy (rem (+ (- (+ (floor (aref origins neighbor) sample-width)
                               (floor index output-width))
                            (floor neighbor output-width))
                         (* sample-height 100))
                      sample-height)))
        (setf (aref candidates i) (+ (* cy sample-width) cx))))))

(u:fn-> choose-random-candidates (state u:b32a u:ub8 u:ub16) null)
(defun choose-random-candidates (state candidates neighbor-count candidate-count)
  (declare (optimize speed))
  (dotimes (i candidate-count)
    (let ((max (* (sample-width state) (sample-height state))))
      (setf (aref candidates (+ neighbor-count i)) (rng:int (rng state) 0 max nil)))))

(u:fn-> update-origins (state u:b32a u:ub32 (or (simple-array u:f64) null)) (values))
(defun update-origins (state candidates index metrics)
  (declare (optimize speed))
  (let ((sample-width (sample-width state))
        (sample-height (sample-height state))
        (sample-data (sample-data state))
        (indexed-sample (indexed-sample state))
        (indexed-p (indexed-p state))
        (kernel-size (kernel-size state))
        (output-width (output-width state))
        (output-height (output-height state))
        (origins (origins state))
        (rng (rng state))
        (max -1d10)
        (arg-max -1))
    (dotimes (c (length candidates))
      (let ((sum (* (rng:float rng 0.0 1.0) 1d-6)))
        (declare (u:f64 sum))
        (loop :for dy :from (- kernel-size) :to kernel-size
              :do (loop :with origin = 0
                        :with si = 0
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
                                    origin (aref origins (+ (* fy output-width) fx)))
                              (unless (minusp origin)
                                (cond
                                  (indexed-p
                                   (if (= (aref sample-data origin)
                                          (aref sample-data si))
                                       (incf sum)
                                       (decf sum)))
                                  (metrics
                                   (incf sum
                                         (aref metrics
                                               (aref indexed-sample origin)
                                               (aref indexed-sample si))))
                                  (t
                                   (incf sum (color-metric (aref sample-data origin)
                                                           (aref sample-data si))))))))
        (when (>= sum max)
          (setf max sum
                arg-max (aref candidates c)))))
    (setf (aref origins index) arg-max)
    (values)))

(defun write-image (state)
  (let* ((sample-data (sample-data state))
         (width (output-width state))
         (height (output-height state))
         (output-size (* width height))
         (origins (origins state))
         (data (u:make-ub32-array output-size)))
    (dotimes (i (length data))
      (setf (aref data i) (aref sample-data (aref origins i))))
    (img:write (img:unpack data width height) width height (output-path state))))

(defun %harrison (state round candidate-count metrics indices)
  (dotimes (counter (length indices))
    (let* ((index (aref indices counter))
           (neighbor-count (if (zerop round) (min 8 counter) 8))
           (candidates (u:make-b32-array (+ neighbor-count candidate-count))))
      (when (plusp neighbor-count)
        (let ((neighbors (u:make-ub32-array neighbor-count)))
          (declare (dynamic-extent neighbors))
          (update-neighbors state neighbors neighbor-count index)
          (update-candidates state neighbors neighbor-count index candidates)))
      (choose-random-candidates state candidates neighbor-count candidate-count)
      (update-origins state candidates index metrics))))

(defun %harrison/parallel (state round candidate-count metrics indices)
  (lparallel:pdotimes (counter (length indices))
    (let* ((index (aref indices counter))
           (neighbor-count (if (zerop round) (min 8 counter) 8))
           (candidates (u:make-b32-array (+ neighbor-count candidate-count))))
      (when (plusp neighbor-count)
        (let ((neighbors (u:make-ub32-array neighbor-count)))
          (declare (dynamic-extent neighbors))
          (update-neighbors state neighbors neighbor-count index)
          (update-candidates state neighbors neighbor-count index candidates)))
      (choose-random-candidates state candidates neighbor-count candidate-count)
      (update-origins state candidates index metrics))))

(defun harrison (sample-path &key width height indexed-p (rounds 3) (candidate-count 20)
                               (kernel-size 1) seed (parallel-p t) output-path)
  (com:check-file-exists sample-path)
  (com:check-image-dimension :width width)
  (com:check-image-dimension :height height)
  (com:check-seed seed)
  (com:check-output-path output-path)
  (unless (typep kernel-size '(integer 1 255))
    (error 'cond:invalid-harrison-kernel-size :value kernel-size))
  (unless (typep rounds '(integer 1 255))
    (error 'cond:invalid-harrison-rounds :value rounds))
  (unless (typep candidate-count '(integer 1 255))
    (error 'cond:invalid-harrison-candidate-count :value candidate-count))
  (let* ((state (make-state (img:make-image sample-path)
                            :indexed-p indexed-p
                            :output-width width
                            :output-height height
                            :output-path output-path
                            :kernel-size kernel-size
                            :seed seed))
         (indices (indices state))
         (metrics (build-metrics state))
         (lp:*kernel* (lp:make-kernel (cl-cpus:get-number-of-processors))))
    (dotimes (round rounds)
      (declare (fixnum round))
      (format t "Round: ~d~%" (1+ round))
      (if parallel-p
          (%harrison/parallel state round candidate-count metrics indices)
          (%harrison state round candidate-count metrics indices)))
    (write-image state)))
