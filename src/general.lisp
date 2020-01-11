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
