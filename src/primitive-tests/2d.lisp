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
   #:point-in-box-p
   #:point-in-circle-p
   #:point-on-line-p))

(in-package #:net.mfiano.lisp.origin.primitive-tests.2d)

(u:fn-> point-on-line-p (point:point line:line &key (:rel u:f32) (:abs u:f32))
        boolean)
(defun point-on-line-p (point line &key (rel 1e-7) (abs rel))
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
  "Test whether a 2D point is contained within a circle."
  (declare (optimize speed))
  (let ((line (line:line point (circle:position circle))))
    (< (line:length-squared line) (expt (circle:radius circle) 2))))

(u:fn-> point-in-box-p (point:point box:box) boolean)
(defun point-in-box-p (point box)
  "Test whether a 2D point is contained within an axis-aligned rectangle. See
POINT-IN-ORIENTED-BOX-P for testing against rotated rectangles."
  (declare (optimize speed))
  (v2:with-components ((p point)
                       (min- (box:min box))
                       (max- (box:max box)))
    (and (<= min-x px max-x)
         (<= min-y py max-y))))

(u:fn-> point-in-oriented-box-p (point:point obox:box) boolean)
(defun point-in-oriented-box-p (point box)
  "Test whether a 2D point is contained within an oriented rectangle. See
POINT-IN-BOX-P for a less expanesive test if the rectangle is axis-aligned."
  (declare (optimize speed))
  (let ((vector (v2:- point (obox:position box)))
        (angle (- (obox:angle box))))
    (m2:*v2! vector (m2:rotation-from-angle angle) vector)
    (point-in-box-p (v2:+ vector (obox:half-extents box))
                    (box:box :position (point:point)
                             :size (v2:scale (obox:half-extents box) 2f0)))))
