(in-package #:origin.test)

(define-test m2/identity
  (let ((m (m2:id))
        (r (m2:mat 1 0 0 1)))
    (is m2:= m r)
    (is m2:= m2:+id+ r)
    (true (m2:id-p (m2:id)))))

(define-test m2/copy
  (let ((m m2:+id+)
        (o (m2:zero)))
    (is m2:= (m2:copy! o m) m2:+id+)
    (is m2:= o m2:+id+)
    (is m2:= (m2:copy m) m2:+id+)
    (isnt eq m (m2:copy m))))

(define-test m2/clamp
  (let ((m (m2:mat 1 -2 3 -4))
        (o (m2:zero))
        (r (m2:mat 1 -1 1 -1)))
    (is m2:= (m2:clamp! o m :min -1.0 :max 1.0) r)
    (is m2:= o r)
    (is m2:= (m2:clamp m :min -1.0 :max 1.0) r)
    (is m2:= (m2:clamp m) m)))

(define-test m2/add
  (let ((m1 (m2:mat 0.50253403 0.64249194 0.51019014 0.64168155))
        (m2 (m2:mat 0.11914015 0.88014805 0.39815342 0.82533318))
        (o (m2:zero))
        (r (m2:mat 0.6216742 1.52264 0.90834355 1.4670148)))
    (is m2:= (m2:+! o m1 m2) r)
    (is m2:= o r)
    (is m2:= (m2:+ m1 m2) r)
    (is m2:= (m2:+ m1 m2:+zero+) m1)
    (is m2:= (m2:+ m2 m2:+zero+) m2)))

(define-test m2/subtract
  (let ((m1 (m2:mat 0.50253403 0.64249194 0.51019014 0.64168155))
        (m2 (m2:mat 0.11914015 0.88014805 0.39815342 0.82533318))
        (o (m2:zero))
        (r (m2:mat 0.38339388 -0.23765612 0.112036705 -0.18365163)))
    (is m2:= (m2:-! o m1 m2) r)
    (is m2:= o r)
    (is m2:= (m2:- m1 m2) r)
    (is m2:= (m2:- m1 m2:+zero+) m1)
    (is m2:= (m2:- m2 m2:+zero+) m2)))

(define-test m2/multiply
  (let ((m (m2:mat 1 5 2 6))
        (r (m2:mat 11 35 14 46))
        (rot-z (m2:rotate m2:+id+ (/ pi 3)))
        (o (m2:zero)))
    (is m2:= (m2:*! o m m) r)
    (is m2:= o r)
    (is m2:= (m2:* m m2:+id+) m)
    (is m2:= (m2:* m2:+id+ m) m)
    (is m2:= (m2:* m m2:+id+) (m2:* m2:+id+ m))
    (isnt m2:= (m2:* m rot-z) (m2:* rot-z m))))

(define-test m2/rotation-vec)
(let ((rmx m2:+id+)
      (omx (m2:id))
      (rvx (v2:vec 1 0)))
  (is m2:= (m2:rotation-axis-from-vec2! omx rvx :x) rmx)
  (true (m2:~ omx rmx))
  (is m2:= (m2:rotation-axis-from-vec2 m2:+id+ rvx :x) rmx))

(define-test m2/rotation
  (let ((omz (m2:id))
        (rmz (m2:mat 0.5 -0.86602545 0.86602545 0.5)))
    (true (m2:~ (m2:rotate! omz m2:+id+ (/ pi 3)) rmz))
    (true (m2:~ omz rmz))
    (true (m2:~ (m2:rotate m2:+id+ (/ pi 3)) rmz))))

(define-test m2/scale
  (let ((m (m2:mat 10 0 0 20))
        (o (m2:id))
        (s (m2:mat 10 0 0 40))
        (v (v2:vec 1 2)))
    (true (m2:= (m2:scale! o m v) s))
    (true (m2:= o s))
    (is v2:= (m2:get-scale (m2:scale m2:+id+ v)) v)))

(define-test m2/vec2-multiply
  (let ((m (m2:rotate m2:+id+ (/ pi 3)))
        (v (v2:vec 1 2))
        (o (v2:zero))
        (rv (v2:vec -1.2320509 1.8660254)))
    (is v2:= (m2:*v2! o m v) rv)
    (is v2:= o rv)
    (is v2:= (m2:*v2 m v) rv)
    (is v2:= (m2:*v2 m2:+id+ v) v)
    (is v2:= (m2:*v2 m2:+id+ v2:+zero+) v2:+zero+)))

(define-test m2/transpose
  (let ((m (m2:mat 1 5 2 6))
        (r (m2:mat 1 2 5 6))
        (o (m2:id)))
    (is m2:= (m2:transpose! o m) r)
    (is m2:= o r)
    (is m2:= (m2:transpose m) r)
    (is m2:= (m2:transpose m2:+id+) m2:+id+)))

(define-test m2/orthogonal-predicate
  (true (m2:orthogonal-p (m2:rotate m2:+id+ pi)))
  (true (m2:orthogonal-p (m2:rotate m2:+id+ (/ pi 2))))
  (true (m2:orthogonal-p (m2:rotate m2:+id+ (/ pi 3)))))

(define-test m2/trace
  (is = (m2:trace (m2:zero)) 0)
  (is = (m2:trace m2:+id+) 2)
  (is = (m2:trace (m2:mat 1 2 3 4)) 5))

(define-test m2/diagonal
  (let ((m (m2:mat 1 2 3 4))
        (r1 (v2:vec 1 4))
        (r2 (v2:vec 2 3))
        (o (v2:zero)))
    (true (m2:diagonal-p (m2:id)))
    (true (not (m2:diagonal-p m)))
    (is v2:= (m2:main-diagonal! o m) r1)
    (is v2:= o r1)
    (is v2:= (m2:main-diagonal m) r1)
    (is v2:= (m2:anti-diagonal! o m) r2)
    (is v2:= o r2)
    (is v2:= (m2:anti-diagonal m) r2)))
