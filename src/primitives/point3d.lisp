(in-package #:cl-user)

;;; 3D points are just type aliased to be vec3, with a convenience constructor
;;; function.

(defpackage #:net.mfiano.lisp.origin.point3d
  (:local-nicknames
   (#:u #:net.mfiano.lisp.golden-utils)
   (#:v3 #:net.mfiano.lisp.origin.vec3)
   (#:v4 #:net.mfiano.lisp.origin.vec4)
   (#:m4 #:net.mfiano.lisp.origin.mat4))
  (:use #:cl)
  (:import-from
   #:net.mfiano.lisp.origin.vec3
   #:x
   #:y
   #:z)
  ;; type and accessors
  (:export
   #:point
   #:x
   #:y
   #:z)
  ;; operations
  (:export
   #:translate
   #:unproject))

(in-package #:net.mfiano.lisp.origin.point3d)

(deftype point () 'v3:vec)

(u:fn-> point (&optional u:f32 u:f32 u:f32) point)
(declaim (inline point))
(defun point (&optional (x 0f0) (y 0f0) (z 0f0))
  (declare (optimize speed))
  (v3::%vec x y z))

(u:fn-> translate (point v3:vec u:f32) point)
(declaim (inline translate))
(defun translate (point direction distance)
  (declare (optimize speed))
  (v3:+ point (v3:scale direction distance)))

(u:fn-> unproject (point m4:mat m4:mat v4:vec) point)
(declaim (inline unproject))
(defun unproject (point model projection viewport)
  (declare (optimize speed))
  (u:mvlet ((out (point))
            (inverse-pm success-p (m4:invert (m4:* projection model))))
    (declare (dynamic-extent out))
    (unless success-p
      (return-from unproject out))
    (m4:with-components ((m inverse-pm))
      (v3:with-components ((p point)
                           (out out))
        (v4:with-components ((v viewport)
                             (o (v4:vec))
                             (i (v4:vec (1- (/ (* (- px vx) 2) vz))
                                        (1- (/ (* (- py vy) 2) vw))
                                        (1- (* pz 2))
                                        1f0)))
          (m4:*v4! o m i)
          (when (zerop ow)
            (return-from unproject out))
          (v3:scale! out (v3:vec o) (/ ow)))))
    (values out t)))
