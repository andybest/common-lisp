(in-package #:net.mfiano.lisp.origin.geometry)

(u:fn-> point-on-line-p (point2d:point line:line &key (:rel u:f32) (:abs u:f32))
        boolean)
(defun point-on-line-p (point line &key (rel 1e-7) (abs rel))
  "Test if a 2D point falls on a 2D line."
  (declare (optimize speed))
  (v2:with-components ((p point)
                       (s (line:start line))
                       (e (line:end line)))
    (assert (/= sx ex))
    (let* ((dy (- ey sy))
           (dx (- ex sx))
           (m (/ dy dx))
           (b (- sy (* m sx))))
      (com:= py (+ (* m px) b) rel abs))))

(u:fn-> point-in-circle-p (point2d:point circle:circle) boolean)
(defun point-in-circle-p (point circle)
  "Test if a point is contained within a circle."
  (declare (optimize speed))
  (let ((line (line:line :start point :end (circle:origin circle))))
    (< (line:length-squared line) (expt (circle:radius circle) 2))))

(u:fn-> point-in-rect-p (point2d:point rect:rect) boolean)
(defun point-in-rect-p (point rect)
  "Test if a point is contained within a rect. See POINT-IN-ORIENTED-RECT-P for
testing against rotated rectangles."
  (declare (optimize speed))
  (v2:with-components ((p point)
                       (min- (rect:min rect))
                       (max- (rect:max rect)))
    (and (<= min-x px max-x)
         (<= min-y py max-y))))

(u:fn-> point-in-oriented-rect-p (point2d:point orect:rect) boolean)
(defun point-in-oriented-rect-p (point rect)
  "Test if a point is contained within an oriented rect. See POINT-IN-RECT-P for
a less expanesive test if the rectangle is axis-aligned."
  (declare (optimize speed))
  (let ((vector (v2:- point (orect:origin rect)))
        (angle (- (orect:angle rect))))
    (m2:*v2! vector (m2:rotation-from-angle angle) vector)
    (point-in-rect-p (v2:+ vector (orect:half-extents rect))
                     (rect:rect :size (v2:scale (orect:half-extents rect)
                                                2f0)))))

(u:fn-> %line/circle (line:line circle:circle) boolean)
(declaim (inline %line/circle))
(defun %line/circle (line circle)
  "Helper function that does the work for LINE/CIRCLE and CIRCLE/LINE."
  (declare (optimize speed))
  ;; Here, we find the closest point to the center of the circle on the line.
  ;; Then we draw a line from the center of the circle to this closest point. If
  ;; the squared length of that line is less than the squared radius of the
  ;; circle, there is an intersection.
  (let* ((line-start (line:start line))
         (circle-origin (circle:origin circle))
         (ab (v2:- (line:end line) line-start))
         (t-param (/ (v2:dot (v2:- circle-origin line-start) ab)
                     (v2:length-squared ab))))
    (when (<= 0.0 t-param 1.0)
      (< (line:length-squared
          (line:line :start circle-origin
                     :end (v2:+ line-start (v2:scale ab t-param))))
         (expt (circle:radius circle) 2)))))

(u:fn-> line/circle (line:line circle:circle) boolean)
(defun line/circle (line circle)
  "Test if a line intersects a circle."
  (declare (optimize speed))
  (%line/circle line circle))

(u:fn-> circle/line (circle:circle line:line) boolean)
(defun circle/line (circle line)
  "Test if a circle intersects a line."
  (declare (optimize speed))
  (%line/circle line circle))

(u:fn-> %line/rect (line:line rect:rect) boolean)
(declaim (inline %line/rect))
(defun %line/rect (line rect)
  "Helper function that does the work for LINE/RECT and RECT/LINE."
  (declare (optimize speed))
  (let ((start (line:start line)))
    ;; First, check if either end point of the line is contained in the rect. If
    ;; yes, we have an intersection and don't need to continue with the more
    ;; expensive test.
    (when (or (point-in-rect-p start rect)
              (point-in-rect-p (line:end line) rect))
      (return-from %line/rect t))
    ;; Perform a raycast against the rect with a ray constructed from the line.
    ;; If the ray hits, and the length of the ray is less than the length of the
    ;; line, there is an interesection.
    (let ((inv-dir (v2:invert (line:direction line))))
      (v2:with-components ((min- (v2:* (v2:- (rect:min rect) start) inv-dir))
                           (max- (v2:* (v2:- (rect:max rect) start) inv-dir)))
        (let ((t-min (max (min min-x max-x) (min min-y max-y)))
              (t-max (min (max min-x max-x) (max min-y max-y))))
          (when (and (plusp t-max)
                     (<= t-min t-max))
            (let ((t-param (if (minusp t-min) t-max t-min)))
              (and (plusp t-param)
                   (< (expt t-param 2) (line:length-squared line))))))))))

(u:fn-> line/rect (line:line rect:rect) boolean)
(defun line/rect (line rect)
  "Test if a line intersects a rect."
  (declare (optimize speed))
  (%line/rect line rect))

(u:fn-> rect/line (rect:rect line:line) boolean)
(defun rect/line (rect line)
  "Test if a rect intersects a line."
  (declare (optimize speed))
  (%line/rect line rect))

(u:fn-> %line/oriented-rect (line:line orect:rect) boolean)
(declaim (inline %line/oriented-rect))
(defun %line/oriented-rect (line rect)
  "Helper function that does the work for LINE/ORIENTED-RECT and
ORIENTED-RECT/LINE."
  (declare (optimize speed))
  ;; Here, we construct a line that is in the local space of the oriented rect.
  ;; In its local space, the oriented rect is just an axis-aligned rect, so we
  ;; can just use the existing LINE/RECT test.
  (let* ((rect-origin (orect:origin rect))
         (half-extents (orect:half-extents rect))
         (vector (v2:- (line:start line) rect-origin))
         (rotation (m2:rotation-from-angle (- (orect:angle rect))))
         (local-line (line:line)))
    (m2:*v2! vector rotation vector)
    (v2:+! (line:start local-line) vector half-extents)
    (v2:-! vector (line:end line) rect-origin)
    (m2:*v2! vector rotation vector)
    (v2:+! (line:end local-line) vector half-extents)
    (line/rect local-line
               (rect:rect :size (v2:scale (orect:half-extents rect) 2f0)))))

(u:fn-> line/oriented-rect (line:line orect:rect) boolean)
(defun line/oriented-rect (line rect)
  "Test if a line intersects an oriented rect."
  (declare (optimize speed))
  (%line/oriented-rect line rect))

(u:fn-> oriented-rect/line (orect:rect line:line) boolean)
(defun oriented-rect/line (rect line)
  "Test if an oriented rect intersects a line."
  (declare (optimize speed))
  (%line/oriented-rect line rect))

(u:fn-> circle/circle (circle:circle circle:circle) boolean)
(defun circle/circle (circle1 circle2)
  "Test if two circles intersect."
  (declare (optimize speed))
  ;; First, construct a line between the two circle centers. Then, if the length
  ;; of the line is less than the sum of the two circle radii, we have an
  ;; intersection. To avoid the square root, we get the squared length of the
  ;; line and compare it to the squared sum of the radii.
  (<= (line:length-squared (line:line :start (circle:origin circle1)
                                      :end (circle:origin circle2)))
      (expt (+ (circle:radius circle1) (circle:radius circle2)) 2)))

(u:fn-> %circle/rect (circle:circle rect:rect) boolean)
(declaim (inline %circle/rect))
(defun %circle/rect (circle rect)
  "Helper function that does the work for CIRCLE/RECT and RECT/CIRCLE."
  (declare (optimize speed))
  ;; First, we clamp the circle's center point to the rect's minimum and maximum
  ;; bounds, which places the closest point to the circle on the surface of the
  ;; rect. Then, we draw a line from the closest point to the center of the
  ;; circle, and if it's less than the squared radius of the circle, they
  ;; intersect.
  (let* ((circle-origin (circle:origin circle))
         (closest-point (v2:copy circle-origin)))
    (v2:clamp! closest-point closest-point (rect:min rect) (rect:max rect))
    (<= (line:length-squared (line:line :start circle-origin
                                        :end closest-point))
        (expt (circle:radius circle) 2))))

(u:fn-> circle/rect (circle:circle rect:rect) boolean)
(defun circle/rect (circle rect)
  "Test if a circle intersects a rect."
  (declare (optimize speed))
  (%circle/rect circle rect))

(u:fn-> rect/circle (rect:rect circle:circle) boolean)
(defun rect/circle (rect circle)
  "Test if a rect intersects a circle."
  (declare (optimize speed))
  (%circle/rect circle rect))

(u:fn-> %circle/oriented-rect (circle:circle orect:rect) boolean)
(declaim (inline %circle/oriented-rect))
(defun %circle/oriented-rect (circle rect)
  "Helper function that does the work for CIRCLE/ORIENTED-RECT and
ORIENTED-RECT/CIRCLE."
  (declare (optimize speed))
  ;; First, we move the circle into the local space of the oriented rect by
  ;; translating the center of the circle relative to the center of the rect.
  ;; Then, we rotate the translated point in the negative orientation of the
  ;; rect. We can then perform a check with the existing CIRCLE/RECT test to see
  ;; if they intersect.
  (let ((half-extents (orect:half-extents rect))
        (vector (v2:- (circle:origin circle) (orect:origin rect)))
        (rotation (m2:rotation-from-angle (- (orect:angle rect)))))
    (m2:*v2! vector rotation vector)
    (circle/rect (circle:circle :origin (v2:+ vector half-extents)
                                :radius (circle:radius circle))
                 (rect:rect :size (v2:scale half-extents 2.0)))))

(u:fn-> circle/oriented-rect (circle:circle orect:rect) boolean)
(defun circle/oriented-rect (circle rect)
  "Test if a circle intersects an oriented rect."
  (declare (optimize speed))
  (%circle/oriented-rect circle rect))

(u:fn-> oriented-rect/circle (orect:rect circle:circle) boolean)
(defun oriented-rect/circle (rect circle)
  "Test if an oriented rect intersects a circle."
  (declare (optimize speed))
  (%circle/oriented-rect circle rect))

(u:fn-> rect/rect (rect:rect rect:rect) boolean)
(defun rect/rect (rect1 rect2)
  "Test if two rects intersect."
  (declare (optimize speed))
  (and (v2:<= (rect:min rect1) (rect:max rect2))
       (v2:<= (rect:min rect2) (rect:max rect1))))

(u:fn-> %rect/oriented-rect (rect:rect orect:rect) boolean)
(declaim (inline %rect/oriented-rect))
(defun %rect/oriented-rect (rect1 rect2)
  "Helper function that does the work for RECT/ORIENTED-RECT and
ORIENTED-RECT/RECT."
  (declare (optimize speed))
  (let ((axes (vector v2:+right+ v2:+up+ (v2:vec) (v2:vec)))
        (rotation (m2:rotation-from-angle (orect:angle rect2))))
    (v2:with-components ((h (orect:half-extents rect2)))
      (m2:*v2! (aref axes 2) rotation (v2:normalize (v2:vec hx 0)))
      (m2:*v2! (aref axes 3) rotation (v2:normalize (v2:vec 0 hy)))
      (map nil
           (lambda (x)
             (v2:with-components ((i1 (rect::interval rect1 x))
                                  (i2 (orect::interval rect2 x)))
               (unless (and (<= i2x i1y) (<= i1x i2y))
                 (return-from %rect/oriented-rect nil))))
           axes)
      t)))

(u:fn-> rect/oriented-rect (rect:rect orect:rect) boolean)
(defun rect/oriented-rect (rect1 rect2)
  "Test if a rect and an oriented rect intersect."
  (declare (optimize speed))
  (%rect/oriented-rect rect1 rect2))

(u:fn-> oriented-rect/rect (orect:rect rect:rect) boolean)
(defun oriented-rect/rect (rect1 rect2)
  "Test if an oriented rect and a rect intersect."
  (declare (optimize speed))
  (%rect/oriented-rect rect2 rect1))

(u:fn-> oriented-rect/oriented-rect (orect:rect orect:rect) boolean)
(defun oriented-rect/oriented-rect (rect1 rect2)
  "Test if two oriented rects intersect."
  (declare (optimize speed))
  (let* ((angle1 (orect:angle rect1))
         (half-extents1 (orect:half-extents rect1))
         (origin2 (v2:copy (orect:origin rect2)))
         (local1 (rect:rect :size (v2:scale half-extents1 2.0)))
         (local2 (orect:rect :origin origin2
                             :half-extents (orect:half-extents rect2)
                             :angle (- (orect:angle rect2) angle1)))
         (vector (v2:- origin2 (orect:origin rect1)))
         (rotation (m2:rotation-from-angle (- angle1))))
    (m2:*v2! vector rotation vector)
    (v2:+! (orect:origin local2) vector half-extents1)
    (rect/oriented-rect local1 local2)))
