(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.origin.primitive-tests.2d
  (:local-nicknames
   (#:box #:net.mfiano.lisp.origin.box2d)
   (#:com #:net.mfiano.lisp.origin.common)
   (#:circle #:net.mfiano.lisp.origin.circle)
   (#:line #:net.mfiano.lisp.origin.line2d)
   (#:m2 #:net.mfiano.lisp.origin.mat2)
   (#:obox #:net.mfiano.lisp.origin.oriented-box-2d)
   (#:point #:net.mfiano.lisp.origin.point2d)
   (#:u #:net.mfiano.lisp.golden-utils)
   (#:v2 #:net.mfiano.lisp.origin.vec2))
  (:use #:cl)
  (:export
   #:box/box
   #:box/circle
   #:box/line
   #:box/oriented-box
   #:circle/box
   #:circle/circle
   #:circle/oriented-box
   #:circle/line
   #:line/box
   #:line/circle
   #:line/oriented-box
   #:oriented-box/box
   #:oriented-box/circle
   #:oriented-box/line
   #:oriented-box/oriented-box
   #:point-in-box-p
   #:point-in-circle-p
   #:point-in-oriented-box-p
   #:point-on-line-p))

(in-package #:net.mfiano.lisp.origin.primitive-tests.2d)

(u:fn-> point-on-line-p (point:point line:line &key (:rel u:f32) (:abs u:f32))
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

(u:fn-> point-in-circle-p (point:point circle:circle) boolean)
(defun point-in-circle-p (point circle)
  "Test if a point is contained within a circle."
  (declare (optimize speed))
  (let ((line (line:line :start point :end (circle:position circle))))
    (< (line:length-squared line) (expt (circle:radius circle) 2))))

(u:fn-> point-in-box-p (point:point box:box) boolean)
(defun point-in-box-p (point box)
  "Test if a point is contained within an axis-aligned rectangle. See
POINT-IN-ORIENTED-BOX-P for testing against rotated rectangles."
  (declare (optimize speed))
  (v2:with-components ((p point)
                       (min- (box:min box))
                       (max- (box:max box)))
    (and (<= min-x px max-x)
         (<= min-y py max-y))))

(u:fn-> point-in-oriented-box-p (point:point obox:box) boolean)
(defun point-in-oriented-box-p (point box)
  "Test if a point is contained within an oriented rectangle. See POINT-IN-BOX-P
for a less expanesive test if the rectangle is axis-aligned."
  (declare (optimize speed))
  (let ((vector (v2:- point (obox:position box)))
        (angle (- (obox:angle box))))
    (m2:*v2! vector (m2:rotation-from-angle angle) vector)
    (point-in-box-p (v2:+ vector (obox:half-extents box))
                    (box:box :size (v2:scale (obox:half-extents box) 2f0)))))

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
         (circle-position (circle:position circle))
         (ab (v2:- (line:end line) line-start))
         (t-param (/ (v2:dot (v2:- circle-position line-start) ab)
                     (v2:length-squared ab))))
    (when (<= 0.0 t-param 1.0)
      (< (line:length-squared
          (line:line :start circle-position
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

(u:fn-> %line/box (line:line box:box) boolean)
(declaim (inline %line/box))
(defun %line/box (line box)
  "Helper function that does the work for LINE/BOX and BOX/LINE."
  (declare (optimize speed))
  (let ((start (line:start line)))
    ;; First, check if either end point of the line is contained in the box. If
    ;; yes, we have an intersection and don't need to continue with the more
    ;; expensive test.
    (when (or (point-in-box-p start box)
              (point-in-box-p (line:end line) box))
      (return-from %line/box t))
    ;; Perform a raycast against the box with a ray constructed from the line.
    ;; If the ray hits, and the length of the ray is less than the length of the
    ;; line, there is an interesection.
    (let ((inv-dir (v2:invert (line:direction line))))
      (v2:with-components ((min- (v2:* (v2:- (box:min box) start) inv-dir))
                           (max- (v2:* (v2:- (box:max box) start) inv-dir)))
        (let ((t-min (max (min min-x max-x) (min min-y max-y)))
              (t-max (min (max min-x max-x) (max min-y max-y))))
          (when (and (plusp t-max)
                     (<= t-min t-max))
            (let ((t-param (if (minusp t-min) t-max t-min)))
              (and (plusp t-param)
                   (< (expt t-param 2) (line:length-squared line))))))))))

(u:fn-> line/box (line:line box:box) boolean)
(defun line/box (line box)
  "Test if a line intersects a box."
  (declare (optimize speed))
  (%line/box line box))

(u:fn-> box/line (box:box line:line) boolean)
(defun box/line (box line)
  "Test if a box intersects a line."
  (declare (optimize speed))
  (%line/box line box))

(u:fn-> %line/oriented-box (line:line obox:box) boolean)
(declaim (inline %line/oriented-box))
(defun %line/oriented-box (line box)
  "Helper function that does the work for LINE/ORIENTED-BOX and
ORIENTED-BOX/LINE."
  (declare (optimize speed))
  ;; Here, we construct a line that is in the local space of the oriented box.
  ;; In its local space, the oriented box is just an axis-aligned box, so we can
  ;; just use the existing LINE/BOX test.
  (let* ((box-position (obox:position box))
         (half-extents (obox:half-extents box))
         (vector (v2:- (line:start line) box-position))
         (rotation (m2:rotation-from-angle (- (obox:angle box))))
         (local-line (line:line)))
    (m2:*v2! vector rotation vector)
    (v2:+! (line:start local-line) vector half-extents)
    (v2:-! vector (line:end line) box-position)
    (m2:*v2! vector rotation vector)
    (v2:+! (line:end local-line) vector half-extents)
    (line/box local-line
              (box:box :size (v2:scale (obox:half-extents box) 2f0)))))

(u:fn-> line/oriented-box (line:line obox:box) boolean)
(defun line/oriented-box (line box)
  "Test if a line intersects an oriented box."
  (declare (optimize speed))
  (%line/oriented-box line box))

(u:fn-> oriented-box/line (obox:box line:line) boolean)
(defun oriented-box/line (box line)
  "Test if an oriented box intersects a line."
  (declare (optimize speed))
  (%line/oriented-box line box))

(u:fn-> circle/circle (circle:circle circle:circle) boolean)
(defun circle/circle (circle1 circle2)
  "Test if two circles intersect."
  (declare (optimize speed))
  ;; First, construct a line between the two circle centers. Then, if the length
  ;; of the line is less than the sum of the two circle radii, we have an
  ;; intersection. To avoid the square root, we get the squared length of the
  ;; line and compare it to the squared sum of the radii.
  (<= (line:length-squared (line:line :start (circle:position circle1)
                                      :end (circle:position circle2)))
      (expt (+ (circle:radius circle1) (circle:radius circle2)) 2)))

(u:fn-> %circle/box (circle:circle box:box) boolean)
(declaim (inline %circle/box))
(defun %circle/box (circle box)
  "Helper function that does the work for CIRCLE/BOX and BOX/CIRCLE."
  (declare (optimize speed))
  ;; First, we clamp the circle's center point to the box's minimum and maximum
  ;; bounds, which places the closest point to the circle on the surface of the
  ;; box. Then, we draw a line from the closest point to the center of the
  ;; circle, and if it's less than the squared radius of the circle, they
  ;; intersect.
  (let* ((circle-position (circle:position circle))
         (closest-point (v2:copy circle-position)))
    (v2:clamp! closest-point closest-point (box:min box) (box:max box))
    (<= (line:length-squared (line:line :start circle-position
                                        :end closest-point))
        (expt (circle:radius circle) 2))))

(u:fn-> circle/box (circle:circle box:box) boolean)
(defun circle/box (circle box)
  "Test if a circle intersects a box."
  (declare (optimize speed))
  (%circle/box circle box))

(u:fn-> box/circle (box:box circle:circle) boolean)
(defun box/circle (box circle)
  "Test if a box intersects a circle."
  (declare (optimize speed))
  (%circle/box circle box))

(u:fn-> %circle/oriented-box (circle:circle obox:box) boolean)
(declaim (inline %circle/oriented-box))
(defun %circle/oriented-box (circle box)
  "Helper function that does the work for CIRCLE/ORIENTED-BOX and
ORIENTED-BOX/CIRCLE."
  (declare (optimize speed))
  ;; First, we move the circle into the local space of the oriented box by
  ;; translating the center of the circle relative to the center of the box.
  ;; Then, we rotate the translated point in the negative orientation of the
  ;; box. We can then perform a check with the existing CIRCLE/BOX test to see
  ;; if they intersect.
  (let ((half-extents (obox:half-extents box))
        (vector (v2:- (circle:position circle) (obox:position box)))
        (rotation (m2:rotation-from-angle (- (obox:angle box)))))
    (m2:*v2! vector rotation vector)
    (circle/box (circle:circle :position (v2:+ vector half-extents)
                               :radius (circle:radius circle))
                (box:box :size (v2:scale half-extents 2.0)))))

(u:fn-> circle/oriented-box (circle:circle obox:box) boolean)
(defun circle/oriented-box (circle box)
  "Test if a circle intersects an oriented box."
  (declare (optimize speed))
  (%circle/oriented-box circle box))

(u:fn-> oriented-box/circle (obox:box circle:circle) boolean)
(defun oriented-box/circle (box circle)
  "Test if an oriented box intersects a circle."
  (declare (optimize speed))
  (%circle/oriented-box circle box))

(u:fn-> box/box (box:box box:box) boolean)
(defun box/box (box1 box2)
  "Test if two boxes intersect."
  (declare (optimize speed))
  (and (v2:<= (box:min box1) (box:max box2))
       (v2:<= (box:min box2) (box:max box1))))

(u:fn-> %box/oriented-box (box:box obox:box) boolean)
(declaim (inline %box/oriented-box))
(defun %box/oriented-box (box1 box2)
  "Helper function that does the work for BOX/ORIENTED-BOX and
ORIENTED-BOX/BOX."
  (declare (optimize speed))
  (let ((axes (vector v2:+right+ v2:+up+ (v2:vec) (v2:vec)))
        (rotation (m2:rotation-from-angle (obox:angle box2))))
    (v2:with-components ((h (obox:half-extents box2)))
      (m2:*v2! (aref axes 2) rotation (v2:normalize (v2:vec hx 0)))
      (m2:*v2! (aref axes 3) rotation (v2:normalize (v2:vec 0 hy)))
      (map nil
           (lambda (x)
             (v2:with-components ((i1 (box::interval box1 x))
                                  (i2 (obox::interval box2 x)))
               (unless (and (<= i2x i1y) (<= i1x i2y))
                 (return-from %box/oriented-box nil))))
           axes)
      t)))

(u:fn-> box/oriented-box (box:box obox:box) boolean)
(defun box/oriented-box (box1 box2)
  "Test if a box and an oriented box intersect."
  (declare (optimize speed))
  (%box/oriented-box box1 box2))

(u:fn-> oriented-box/box (obox:box box:box) boolean)
(defun oriented-box/box (box1 box2)
  "Test if an oriented box and a box intersect."
  (declare (optimize speed))
  (%box/oriented-box box2 box1))

(u:fn-> oriented-box/oriented-box (obox:box obox:box) boolean)
(defun oriented-box/oriented-box (box1 box2)
  "Test if two oriented boxes intersect."
  (declare (optimize speed))
  (let* ((angle1 (obox:angle box1))
         (half-extents1 (obox:half-extents box1))
         (position2 (v2:copy (obox:position box2)))
         (local1 (box:box :size (v2:scale half-extents1 2.0)))
         (local2 (obox:box :position position2
                           :half-extents (obox:half-extents box2)
                           :angle (- (obox:angle box2) angle1)))
         (vector (v2:- position2 (obox:position box1)))
         (rotation (m2:rotation-from-angle (- angle1))))
    (m2:*v2! vector rotation vector)
    (v2:+! (obox:position local2) vector half-extents1)
    (box/oriented-box local1 local2)))
