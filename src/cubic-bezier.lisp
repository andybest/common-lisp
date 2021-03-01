(in-package #:cubic-bezier)

(u:define-constant +matrix+ (dm4:mat -1 3 -3 1 3 -6 3 0 -3 3 0 0 1 0 0 0) :test #'equalp)

(defstruct (curve
            (:constructor %make-curve)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (divisions 100 :type fixnum)
  (geometry (make-array 0 :adjustable t :fill-pointer 0) :type vector)
  (arc-lengths (u:make-f32-array 0) :type u:f32a)
  (arc-lengths-update nil :type boolean))

(u:fn-> estimate-arc-lengths (curve) null)
(defun estimate-arc-lengths (curve)
  (declare (optimize speed))
  (setf (aref (arc-lengths curve) 0) 0f0)
  (loop :with max = (divisions curve)
        :for i :from 1 :to max
        :for previous :of-type v3:vec = (evaluate curve 0f0) :then current
        :for current :of-type v3:vec = (evaluate curve (/ i max))
        :sum (point3d:distance previous current) :into length :of-type u:f32
        :do (setf (aref (arc-lengths curve) i) length)))

(defun verify-points (points)
  (unless (every (lambda (x) (typep x 'v3:vec)) points)
    (error "Points must be a list of 3-component vectors.")))

(u:fn-> ensure-point-list (sequence) list)
(declaim (inline ensure-point-list))
(defun ensure-point-list (points)
  (declare (optimize speed))
  (etypecase points
    (list points)
    (vector (map 'list #'identity points))))

(u:fn-> point-count-valid-p (fixnum) boolean)
(declaim (inline point-count-valid-p))
(defun point-count-valid-p (point-count)
  "Check whether the integer `point-count` is a valid number of points for a cubic Bézier curve
path. To be valid, there must be at least 4 points, any increment of 3 thereafter: (4, 7, 10, 13,
...)."
  (declare (optimize speed))
  (and (> point-count 1)
       (= 1 (mod point-count 3))))

(defun point-index-present-p (curve index)
  "Check if `curve` has a point at index `index`."
  (let ((geometry (geometry curve))
        (matrix-index (max 0 (floor (1- index) 3))))
    (<= 0 matrix-index (1- (length geometry)))))

(u:fn-> add-geometry (curve sequence) (values))
(defun add-geometry (curve points)
  (declare (optimize speed))
  (loop :with points = (ensure-point-list points)
        :with segment-count = (1+ (the fixnum (/ (- (list-length points) 4) 3)))
        :for (a b c d) :on points :by #'cdddr
        :for index :of-type fixnum :from 0
        :when (< index segment-count)
          :do (vector-push-extend (dm4:mat (m4:mat (v4:vec a) (v4:vec b) (v4:vec c) (v4:vec d)))
                                  (geometry curve)))
  (setf (arc-lengths-update curve) t)
  (values))

(u:fn-> make-geometry (curve list) (values))
(defun make-geometry (curve points)
  (declare (optimize speed))
  (let ((point-count (list-length points)))
    (unless (point-count-valid-p point-count)
      (error "Invalid number of points: ~s." point-count))
    (verify-points points)
    (add-geometry curve points)
    (values)))

(defun add-points (curve points)
  "Append the specified `points` to `curve`. `points` should be a sequence of 3-dimensional vectors
as constructed with `#'origin.vec3:vec`. NOTE: It is an error to add less than a segment worth of
points. A cubic Bézier curve is defined by 4 points for the first segment, and three points for each
successive segment (since the first point of a segment is shared with the last point of the previous
segment.)"
  (let* ((points (ensure-point-list points))
         (point-count (list-length points)))
    (unless (and (plusp point-count)
                 (zerop (mod point-count 3)))
      (error "Invalid number of points: ~s." point-count))
    (verify-points points)
    (let* ((geometry (geometry curve))
           (last-index (1- (length geometry)))
           (shared-point (v3:vec (dv3:vec (dm4:get-column (aref geometry last-index) 3)))))
      (add-geometry curve (cons shared-point points))
      (values))))

(defun edit-point (curve index value)
  "Edit the point of `curve` at index `index` with `value`. `value` should be a 3-dimensional vector
as constructed with `#'origin.vec3:vec`."
  (flet ((write-column (geometry value matrix-index column-index)
           (let ((matrix (aref geometry matrix-index)))
             (dm4:set-column! matrix matrix value column-index))))
    (u:mvlet* ((geometry (geometry curve))
               (value (dv4:vec (dv3:vec value)))
               (quot rem (floor (1- index) 3))
               (matrix-index (max 0 quot))
               (column-index (if (zerop index) 0 (1+ rem))))
      (unless (< matrix-index (length geometry))
        (error "There is no point to edit at index ~s." index))
      (write-column geometry value matrix-index column-index)
      (when (and (plusp index)
                 (zerop (mod index 3))
                 (< matrix-index (1- (length geometry))))
        (write-column geometry value (1+ matrix-index) 0))
      (values))))

(defun make-curve (points &key (divisions 100))
  "Create a cubic Bézier curve path from the given sequence of `points`. `points` is a sequence of
3-dimensional vectors as constructed with `#'origin.vec3:vec`. `divisions` is the number of
sub-divisions to use when estimating the arc-length of the curve path: see the documentation for
`#'evaluate` for more information."
  (unless (evenp divisions)
    (error "Division count must be even."))
  (let* ((arc-lengths (u:make-f32-array (1+ divisions)))
         (curve (%make-curve :divisions divisions :arc-lengths arc-lengths)))
    (make-geometry curve points)
    curve))

(u:fn-> remap (curve u:f32) u:f32)
(defun remap (curve parameter)
  (declare (optimize speed))
  (flet ((%bisect (arc-lengths arc-length-count target)
           (loop :with high = (1- arc-length-count)
                 :with low = 0
                 :while (< low high)
                 :for index :of-type fixnum = 0 :then (+ low (floor (- high low) 2))
                 :do (if (< (aref arc-lengths index) target)
                         (setf low (1+ index))
                         (setf high index))
                 :finally (return index)))
         (%remap (arc-lengths arc-length-count target index)
           (let* ((before (aref arc-lengths index))
                  (after (aref arc-lengths (1+ index)))
                  (length (- after before))
                  (fraction (/ (- target before) length)))
             (max 0f0
                  (if (= before target)
                      (/ index (float (1- arc-length-count) 1f0))
                      (/ (+ index fraction) (1- arc-length-count)))))))
    (when (arc-lengths-update curve)
      (estimate-arc-lengths curve)
      (setf (arc-lengths-update curve) nil))
    (let* ((arc-lengths (arc-lengths curve))
           (arc-length-count (length arc-lengths))
           (target (* (aref arc-lengths (1- arc-length-count)) parameter))
           (index (%bisect arc-lengths arc-length-count target)))
      (%remap arc-lengths arc-length-count target index))))

(defun evaluate (curve parameter &key even-spacing-p)
  "Evaluate `curve` at parameter `parameter`. If `even-spacing-p` is non-NIL, arc-length
re-parameterization is applied, which evenly spaces points along the curve. The number of points is
defined by the `divisions` argument supplied when constructing the curve with `#'make-curve`.
Arc-length re-parameterization is a remapping of `parameter` before evaluating the curve, in order
to allow for uses such as animation along a curve with a constant velocity."
  (unless (<= 0 parameter 1)
    (error "Parameter must be in the closed range [0..1]."))
  (u:mvlet* ((geometry (geometry curve))
             (geometry-length (length geometry))
             (parameter (if even-spacing-p (remap curve parameter) parameter))
             (index x (floor (* parameter geometry-length))))
    (when (and (= index geometry-length)
               (zerop x))
      (decf index)
      (setf x 1))
    (v3:vec
     (dv3:vec
      (dm4:*v4 (dm4:* (aref geometry index) +matrix+) (dv4:vec (* x x x) (* x x) x 1))))))

(u:fn-> collect-points (curve fixnum &key (:even-spacing-p boolean)) list)
(defun collect-points (curve count &key even-spacing-p)
  "Evaluate `count` points along curve, returning a list of 3-dimensional vectors. If
`even-spacing-p` is supplied, arc-length re-parameterization is applied: see the documentation for
`#'evaluate` for more information."
  (declare (optimize speed))
  (loop :for i :below count
        :collect (evaluate curve (/ i (float (1- count) 1f0)) :even-spacing-p even-spacing-p)))

(u:fn-> collect-segments (curve fixnum &key (:even-spacing-p boolean)) list)
(defun collect-segments (curve count &key even-spacing-p)
  "Collect a list of `count` segments of `curve`. A segment is a list of two 3-dimensional vectors.
If `even-spacing-p` is supplied, arc-length re-parameterization is applied: see the documentation
for `#'evaluate` for more information."
  (declare (optimize speed))
  (loop :for i :from 1 :to count
        :for p1 = (evaluate curve 0f0 :even-spacing-p even-spacing-p) :then p2
        :for p2 = (evaluate curve (/ i (float count 1f0)) :even-spacing-p even-spacing-p)
        :collect (list p1 p2)))
