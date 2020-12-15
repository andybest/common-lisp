(in-package #:net.mfiano.lisp.origin)

(u:fn-> unproject! (v3:vec v3:vec m4:mat m4:mat v4:vec) v3:vec)
(u:defun-inline unproject! (out point model projection viewport)
  (declare (optimize speed))
  (u:mvlet ((inverse-pm success-p (m4:invert (m4:* projection model))))
    (unless success-p
      (return-from unproject! out))
    (m4:with-components ((m inverse-pm))
      (v4:with-components ((v viewport))
        (v3:with-components ((p point)
                             (out out))
          (v4:with-elements ((o 0f0 0f0 0f0 0f0)
                             (i (1- (/ (* (- px vx) 2) vz))
                                (1- (/ (* (- py vy) 2) vw))
                                (1- (* pz 2))
                                1f0))
            (m4::%*v4! ox oy oz ow
                       m00 m01 m02 m03 m10 m11 m12 m13
                       m20 m21 m22 m23 m30 m31 m32 m33
                       ix iy iz iw)
            (when (zerop ow)
              (return-from unproject! out))
            (v3::%scale outx outy outz ox oy oz (/ ow)))))))
  (values out t))

(u:fn-> unproject (v3:vec m4:mat m4:mat v4:vec) v3:vec)
(u:defun-inline unproject (point model projection viewport)
  (declare (optimize speed))
  (unproject! (v3:vec) point model projection viewport))

(u:fn-> translate-point (v3:vec v3:vec u:f32) v3:vec)
(u:defun-inline translate-point (point direction distance)
  (declare (optimize speed))
  (v3:+ point (v3:scale direction distance)))

(u:fn-> line-segment-midpoint (v3:vec v3:vec) v3:vec)
(u:defun-inline line-segment-midpoint (point1 point2)
  (declare (optimize speed))
  (v3:lerp point1 point2 0.5))

(u:fn-> line-direction (v3:vec v3:vec) v3:vec)
(u:defun-inline line-direction (point1 point2)
  (declare (optimize speed))
  (v3:normalize (v3:- point2 point1)))

(u:fn-> line-plane-intersect (v3:vec v3:vec v3:vec v3:vec) v3:vec)
(u:defun-inline line-plane-intersect (line-point1
                                      line-point2
                                      plane-point
                                      plane-normal)
  (declare (optimize speed))
  (let* ((dir (line-direction line-point1 line-point2))
         (dir-dot-plane (v3:dot dir plane-normal))
         (plane-line (v3:- line-point1 plane-point)))
    (if (zerop dir-dot-plane)
        (v3:vec)
        (let ((dist (/ (- (v3:dot plane-normal plane-line)) dir-dot-plane)))
          (translate-point line-point1 dir dist)))))

(u:fn-> line-point-distance (v3:vec v3:vec v3:vec) u:f32)
(u:defun-inline line-point-distance (line-point1 line-point2 point)
  (declare (optimize speed))
  (let* ((dir (line-direction line-point1 line-point2))
         (intersect (line-plane-intersect line-point1 line-point2 point dir)))
    (v3:distance point intersect)))
