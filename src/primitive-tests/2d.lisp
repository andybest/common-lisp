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
   #:box/line
   #:circle/line
   #:line/box
   #:line/circle
   #:line/oriented-box
   #:oriented-box/line
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
  "Test if a 2D point is contained within a circle."
  (declare (optimize speed))
  (let ((line (line:line :start point :end (circle:position circle))))
    (< (line:length-squared line) (expt (circle:radius circle) 2))))

(u:fn-> point-in-box-p (point:point box:box) boolean)
(defun point-in-box-p (point box)
  "Test if a 2D point is contained within an axis-aligned rectangle. See
POINT-IN-ORIENTED-BOX-P for testing against rotated rectangles."
  (declare (optimize speed))
  (v2:with-components ((p point)
                       (min- (box:min box))
                       (max- (box:max box)))
    (and (<= min-x px max-x)
         (<= min-y py max-y))))

(u:fn-> point-in-oriented-box-p (point:point obox:box) boolean)
(defun point-in-oriented-box-p (point box)
  "Test if a 2D point is contained within an oriented rectangle. See
POINT-IN-BOX-P for a less expanesive test if the rectangle is axis-aligned."
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
  "Test if a 2D line intersects a circle."
  (declare (optimize speed))
  (%line/circle line circle))

(u:fn-> circle/line (circle:circle line:line) boolean)
(defun circle/line (circle line)
  "Test if a circle intersects a 2D line."
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
  "Test if a 2D line intersects a 2D box."
  (declare (optimize speed))
  (%line/box line box))

(u:fn-> box/line (box:box line:line) boolean)
(defun box/line (box line)
  "Test if a 2D box intersects a 2D line."
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
  "Test if a 2D line intersects a 2D oriented box."
  (declare (optimize speed))
  (%line/oriented-box line box))

(u:fn-> oriented-box/line (obox:box line:line) boolean)
(defun oriented-box/line (box line)
  "Test if a 2D oriented box intersects a 2D line."
  (declare (optimize speed))
  (%line/oriented-box line box))
