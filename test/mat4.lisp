(in-package #:origin.test)

(define-test m4/identity
  (let ((m (m4:mat))
        (r (m4:mat 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1)))
    (is m4:= m4:+id+ r)
    (true (m4:id-p (m4:mat 1)))
    (true (m4:id-p (m4:id! m)))))

(define-test m4/copy
  (let ((m m4:+id+)
        (o (m4:mat)))
    (is m4:= (m4:copy! o m) m4:+id+)
    (is m4:= o m4:+id+)
    (is m4:= (m4:copy m) m4:+id+)
    (isnt eq m (m4:copy m))))

(define-test m4/clamp
  (let ((m (m4:mat 1 -2 3 -4 5 -6 7 -8 9 -10 11 -12 13 -14 15 -16))
        (o (m4:mat))
        (r (m4:mat 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1)))
    (is m4:= (m4:clamp! o m :min -1.0 :max 1.0) r)
    (is m4:= o r)
    (is m4:= (m4:clamp m :min -1.0 :max 1.0) r)
    (is m4:= (m4:clamp m) m)))

(define-test m4/add
  (let ((m1 (m4:mat 8 2 9 1 0 4 8 4 8 3 0 1 1 8 3 8))
        (m2 (m4:mat 8 6 6 1 9 3 8 4 7 0 1 1 8 3 6 7))
        (o (m4:mat))
        (r (m4:mat 16 8 15 2 9 7 16 8 15 3 1 2 9 11 9 15)))
    (is m4:= (m4:+! o m1 m2) r)
    (is m4:= o r)
    (is m4:= (m4:+ m1 m2) r)
    (is m4:= (m4:+ m1 m4:+zero+) m1)
    (is m4:= (m4:+ m2 m4:+zero+) m2)))

(define-test m4/subtract
  (let ((m1 (m4:mat 8 2 9 1 0 4 8 4 8 3 0 1 1 8 3 8))
        (m2 (m4:mat 8 6 6 1 9 3 8 4 7 0 1 1 8 3 6 7))
        (o (m4:mat))
        (r (m4:mat 0 -4 3 0 -9 1 0 0 1 3 -1 0 -7 5 -3 1)))
    (is m4:= (m4:-! o m1 m2) r)
    (is m4:= o r)
    (is m4:= (m4:- m1 m2) r)
    (is m4:= (m4:- m1 m4:+zero+) m1)
    (is m4:= (m4:- m2 m4:+zero+) m2)))

(define-test m4/multiply
  (let ((m1 (m4:mat 1 5 9 13 2 6 10 14 3 7 11 15 4 8 12 16))
        (m2 (m4:mat 10 50 90 130 20 60 100 140 30 70 110 150 40 80 120 160))
        (m3 m4:+id+)
        (r (m4:mat 90 202 314 426 100 228 356 484 110 254 398 542 120 280 440
                   600))
        (rot-x (m4:rotate m4:+id+ (v3:vec origin:pi/3 0 0)))
        (rot-y (m4:rotate m4:+id+ (v3:vec 0 origin:pi/4 0)))
        (rot-xy (m4:rotate m4:+id+ (v3:vec origin:pi/3 origin:pi/4 0)))
        (tr1 (m4:translate m4:+id+ (v3:vec 5 10 15)))
        (tr2 (m4:translate m4:+id+ (v3:vec 10 20 30)))
        (o (m4:mat)))
    (is m4:= (m4:*! o m1 m1) r)
    (is m4:= o r)
    (is m4:= (m4:* m1 m3) m1)
    (is m4:= (m4:* m3 m1) m1)
    (is m4:= (m4:* m1 m2) (m4:* m2 m1))
    (is m4:= (m4:* rot-x rot-y) rot-xy)
    (isnt m4:= (m4:* rot-x rot-y) (m4:* rot-y rot-x))
    (is v3:= (m4:get-translation (m4:* tr1 rot-xy)) (m4:get-translation tr1))
    (is v3:= (m4:get-translation (m4:* tr1 tr2)) (v3:vec 15 30 45))
    (is v3:= (m4:get-translation (m4:* tr2 tr1)) (v3:vec 15 30 45))))

1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16

(define-test m4/translation-convert
  (let ((m (m4:mat 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
        (rm (m4:mat 1 0 0 0 0 1 0 0 0 0 1 0 5 10 15 1))
        (om (m4:mat 1))
        (v (v3:vec 5 10 15))
        (rv (v3:vec 13 14 15))
        (ov (v3:vec)))
    (is m4:= (m4:set-translation! om om v) rm)
    (is m4:= om rm)
    (is m4:= (m4:set-translation om v) rm)
    (is v3:= (m4:get-translation! ov m) rv)
    (is v3:= ov rv)
    (is v3:= (m4:get-translation m) rv)))

(define-test m4/translate
  (let ((m (m4:rotate m4:+id+ (v3:vec origin:pi/3 0 0)))
        (o (m4:mat 1))
        (r (m4:mat 1 0 0 0 0 0.5 0.86602545 0 0 -0.86602545 0.5 0 5 10 15 1))
        (v (v3:vec 5 10 15)))
    (true (m4:~ (m4:translate! o m v) r))
    (true (m4:~ o r))
    (is v3:= (m4:get-translation (m4:translate m4:+id+ v)) v)
    (is v3:= (m4:get-translation (m4:translate m v)) v)))

(define-test m4/rotation-copy
  (let ((m (m4:mat 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
        (r (m4:mat 1 2 3 0 5 6 7 0 9 10 11 0 0 0 0 1))
        (o (m4:mat 1)))
    (is m4:= (m4:copy-rotation! o m) r)
    (is m4:= o r)
    (is m4:= (m4:copy-rotation m) r)))

(define-test m4/rotation-convert
  (let ((m (m4:rotate m4:+id+ (v3:vec origin:pi/3 0 0)))
        (rmx m4:+id+)
        (rmy (m4:mat 1 0 0 0 0 0.5 0.86602545 0 0 0 1 0 0 0 0 1))
        (rmz (m4:mat 1 0 0 0 0 1 0 0 0 -0.86602545 0.5 0 0 0 0 1))
        (omx (m4:mat 1))
        (omy (m4:mat 1))
        (omz (m4:mat 1))
        (rvx (v3:vec 1 0 0))
        (rvy (v3:vec 0 0.5 0.86602545))
        (rvz (v3:vec 0 -0.86602545 0.5))
        (ovx (v3:vec))
        (ovy (v3:vec))
        (ovz (v3:vec)))
    (true (v3:= (m4:rotation-axis-to-vec3! ovx m :x) rvx))
    (true (v3:~ (m4:rotation-axis-to-vec3! ovy m :y) rvy))
    (true (v3:~ (m4:rotation-axis-to-vec3! ovz m :z) rvz))
    (true (v3:~ ovx rvx))
    (true (v3:~ ovy rvy))
    (true (v3:~ ovz rvz))
    (true (v3:~ (m4:rotation-axis-to-vec3 m :x) rvx))
    (true (v3:~ (m4:rotation-axis-to-vec3 m :y) rvy))
    (true (v3:~ (m4:rotation-axis-to-vec3 m :z) rvz))
    (true (m4:~ (m4:rotation-axis-from-vec3! omx rvx :x) rmx))
    (true (m4:~ (m4:rotation-axis-from-vec3! omy rvy :y) rmy))
    (true (m4:~ (m4:rotation-axis-from-vec3! omz rvz :z) rmz))
    (true (m4:~ omx rmx))
    (true (m4:~ omy rmy))
    (true (m4:~ omz rmz))
    (true (m4:~ (m4:rotation-axis-from-vec3 m4:+id+ rvx :x) rmx))
    (true (m4:~ (m4:rotation-axis-from-vec3 m4:+id+ rvy :y) rmy))
    (true (m4:~ (m4:rotation-axis-from-vec3 m4:+id+ rvz :z) rmz))))

(define-test m4/rotation
  (let ((omx (m4:mat 1))
        (omy (m4:mat 1))
        (omz (m4:mat 1))
        (rmx (m4:mat 1 0 0 0 0 0.5 0.86602545 0 0 -0.86602545 0.5 0 0 0 0 1))
        (rmy (m4:mat 0.5 0 -0.86602545 0 0 1 0 0 0.86602545 0 0.5 0 0 0 0 1))
        (rmz (m4:mat 0.5 0.86602545 0 0 -0.86602545 0.5 0 0 0 0 1 0 0 0 0 1))
        (vx (v3:vec origin:pi/3 0 0))
        (vy (v3:vec 0 origin:pi/3 0))
        (vz (v3:vec 0 0 origin:pi/3)))
    (true (m4:~ (m4:rotate! omx m4:+id+ vx) rmx))
    (true (m4:~ (m4:rotate! omy m4:+id+ vy) rmy))
    (true (m4:~ (m4:rotate! omz m4:+id+ vz) rmz))
    (true (m4:~ omx rmx))
    (true (m4:~ omy rmy))
    (true (m4:~ omz rmz))
    (true (m4:~ (m4:rotate m4:+id+ vx) rmx))
    (true (m4:~ (m4:rotate m4:+id+ vy) rmy))
    (true (m4:~ (m4:rotate m4:+id+ vz) rmz))))

(define-test m4/scale
  (let ((m (m4:mat 10 0 0 0 0 20 0 0 0 0 30 0 0 0 0 2))
        (o (m4:mat 1))
        (s (m4:mat 10 0 0 0 0 40 0 0 0 0 90 0 0 0 0 2))
        (v (v3:vec 1 2 3)))
    (true (m4:= (m4:scale! o m v) s))
    (true (m4:= o s))
    (is v3:= (m4:get-scale (m4:scale m4:+id+ v)) v)))

(define-test m4/vec4-multiply
  (let ((m (m4:rotate m4:+id+ (v3:vec origin:pi/3 0 0)))
        (v (v4:vec 1 2 3 4))
        (o (v4:vec))
        (rv (v4:vec 1 -1.5980763 3.232051 4)))
    (is v4:= (m4:*v4! o m v) rv)
    (is v4:= o rv)
    (is v4:= (m4:*v4 m v) rv)
    (is v4:= (m4:*v4 m4:+id+ v) v)
    (is v4:= (m4:*v4 m4:+id+ v4:+zero+) v4:+zero+)))

(define-test m4/transpose
  (let ((m (m4:mat 1 5 9 13 2 6 10 14 3 7 11 15 4 8 12 16))
        (r (m4:mat 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
        (o (m4:mat 1)))
    (is m4:= (m4:transpose! o m) r)
    (is m4:= o r)
    (is m4:= (m4:transpose m) r)
    (is m4:= (m4:transpose m4:+id+) m4:+id+)))

(define-test m4/orthogonal-predicate
  (true (m4:orthogonal-p (m4:rotate m4:+id+ (v3:vec origin:pi 0 0))))
  (true (m4:orthogonal-p (m4:rotate m4:+id+ (v3:vec origin:pi/2 0 0))))
  (true (m4:orthogonal-p (m4:rotate m4:+id+ (v3:vec origin:pi/3 0 0))))
  (true (m4:orthogonal-p (m4:rotate m4:+id+ (v3:vec origin:pi/4 0 0))))
  (true (m4:orthogonal-p (m4:rotate m4:+id+ (v3:vec origin:pi/6 0 0)))))

(define-test m4/orthgonalize
  (let ((m (m4:mat 0 0 1 0 1 0 0 0 -0.12988785 0.3997815 0.5468181 0 1.0139829
                   -0.027215311 0.18567966 0))
        (o (m4:mat 1))
        (r (m4:mat 0 0 1 0 1 0 0 0 0 1 0 0 0 0 0 1)))
    (is m4:= (m4:orthonormalize! o m) r)
    (is m4:= o r)
    (is m4:= (m4:orthonormalize m) r)))

(define-test m4/trace
  (is = (m4:trace (m4:mat)) 0)
  (is = (m4:trace m4:+id+) 4)
  (is = (m4:trace (m4:mat 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)) 34))

(define-test m4/diagonal
  (let ((m (m4:mat 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
        (r1 (v4:vec 1 6 11 16))
        (r2 (v4:vec 13 10 7 4))
        (o (v4:vec)))
    (true (m4:diagonal-p (m4:mat 1)))
    (true (not (m4:diagonal-p m)))
    (is v4:= (m4:main-diagonal! o m) r1)
    (is v4:= o r1)
    (is v4:= (m4:main-diagonal m) r1)
    (is v4:= (m4:anti-diagonal! o m) r2)
    (is v4:= o r2)
    (is v4:= (m4:anti-diagonal m) r2)))

(define-test m4/determinant
  (is = (m4:determinant (m4:mat 1 5 9 13 2 6 10 14 3 7 11 15 4 8 12 16)) 0)
  (is = (m4:determinant m4:+id+) 1)
  (is = (m4:determinant (m4:rotate m4:+id+ (v3:vec origin:pi/3 0 0))) 1)
  (is = (m4:determinant (m4:mat 1 0 0 0 0 0 1 0 0 1 0 0 0 0 0 1)) -1))

(define-test m4/invert
  (let ((m (m4:rotate m4:+id+ (v3:vec origin:pi/3 0 0)))
        (r (m4:rotate m4:+id+ (v3:vec (- origin:pi/3) 0 0)))
        (o (m4:mat 1)))
    (is m4:= (m4:invert! o m) r)
    (is m4:= o r)
    (is m4:= (m4:invert m) r)
    (is m4:= (m4:invert m4:+id+) m4:+id+)
    (is-values (m4:invert (m4:mat 1 5 9 13 2 6 10 14 3 7 11 15 4 8 12 16))
      (values (m4:mat 1 5 9 13 2 6 10 14 3 7 11 15 4 8 12 16)
              nil))))

(define-test m4/set-view
  (let ((o (m4:mat 1))
        (r (m4:mat -0.7071068 0 0.7071068 0 0 1 0 0 -0.7071068 0 -0.7071068 0
                   0.7071068 0 -0.7071068 1))
        (o2 (m4:mat 1))
        (r2 (m4:mat 0.9622504 0.05143445 0.26726124 0 -0.19245008 0.8229512
                    0.5345225 0 -0.19245008 -0.5657789 0.80178374 0
                    -2.3841858e-7 -9.536743e-7 -18.708286 1)))
    (true (m4:~ (m4:set-view! o
                              (v3:vec 1 0 0)
                              (v3:vec 0 0 1)
                              (v3:vec 0 1 0))
                r))
    (true (m4:~ o r))
    (true (m4:~ (m4:set-view (v3:vec 1 0 0)
                             (v3:vec 0 0 1)
                             (v3:vec 0 1 0))
                r))
    (true (m4:~ (m4:set-view! o2
                              (v3:vec 5 10 15)
                              (v3:vec 0 0 0)
                              (v3:vec 0 1 -1))
                r2))
    (true (m4:~ o2 r2))
    (true (m4:~ (m4:set-view (v3:vec 5 10 15)
                             (v3:vec 0 0 0)
                             (v3:vec 0 1 -1))
                r2))))

(define-test m4/set-projection/orthographic
  (let ((r (m4:mat 0.05 0 0 0 0 0.1 0 0 0 0 -0.002 0 0 0 -1 1))
        (o (m4:mat 1)))
    (is m4:= (m4:set-projection/orthographic!
              o -20.0 20.0 -10.0 10.0 0.0 1000.0)
        r)
    (is m4:= o r)
    (is m4:= (m4:set-projection/orthographic -20.0 20.0 -10.0 10.0 0.0 1000.0)
        r)))

(define-test m4/set-projection/perspective
  (let ((r (m4:mat 0.9742786 0 0 0 0 1.7320509 0 0 0 0 -1.002002 -1 0 0
                   -2.002002 0))
        (o (m4:mat 1)))
    (is m4:= (m4:set-projection/perspective!
              o origin:pi/3 (float (/ 16 9) 1f0) 1.0 1000.0)
        r)
    (is m4:= o r)
    (is m4:= (m4:set-projection/perspective origin:pi/3 (/ 16.0 9.0) 1.0 1000.0)
        r)))
