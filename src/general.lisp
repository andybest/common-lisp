(in-package #:origin)

(define-op unproject! ((out v3:vec) (point v3:vec) (model m4:mat)
                       (projection m4:mat) (viewport v4:vec))
    (:out v3:vec)
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

(define-op unproject ((point v3:vec) (model m4:mat) (projection m4:mat)
                      (viewport v4:vec))
    (:out v3:vec)
  (unproject! (v3:zero) point model projection viewport))

(define-op translate-point ((point v3:vec) (direction v3:vec)
                            (distance single-float))
    (:out v3:vec)
  (v3:+ point (v3:scale direction distance)))

(define-op line-segment-midpoint ((point1 v3:vec) (point2 v3:vec)) (:out v3:vec)
  (v3:lerp point1 point2 0.5))

(define-op line-direction ((point1 v3:vec) (point2 v3:vec)) (:out v3:vec)
  (v3:normalize (v3:- point2 point1)))

(define-op line-plane-intersect ((line-point1 v3:vec) (line-point2 v3:vec)
                                 (plane-point v3:vec) (plane-normal v3:vec))
    (:out v3:vec)
  (let* ((dir (line-direction line-point1 line-point2))
         (dir-dot-plane (v3:dot dir plane-normal))
         (plane-line (v3:- line-point1 plane-point)))
    (if (zerop dir-dot-plane)
        (v3:zero)
        (let ((dist (/ (- (v3:dot plane-normal plane-line)) dir-dot-plane)))
          (translate-point line-point1 dir dist)))))

(define-op line-point-distance ((line-point1 v3:vec) (line-point2 v3:vec)
                                (point v3:vec))
    (:out single-float)
  (let* ((dir (line-direction line-point1 line-point2))
         (intersect (line-plane-intersect line-point1 line-point2 point dir)))
    (v3:distance point intersect)))
