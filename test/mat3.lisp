(in-package #:box.math.test)

(define-test m3/identity
  (let ((m (m3:id))
        (r (m3:make 1 0 0 0 1 0 0 0 1)))
    (is m3:= m r)
    (is m3:= m3:+id+ r)))

(define-test m3/copy
  (let ((m m3:+id+)
        (o (m3:zero)))
    (is m3:= (m3:copy! o m) m3:+id+)
    (is m3:= o m3:+id+)
    (is m3:= (m3:copy m) m3:+id+)
    (isnt eq m (m3:copy m))))

(define-test m3/clamp
  (let ((m (m3:make 1 -2 3 -4 5 -6 7 -8 9))
        (o (m3:zero))
        (r (m3:make 1 -1 1 -1 1 -1 1 -1 1)))
    (is m3:= (m3:clamp! o m :min -1.0 :max 1.0) r)
    (is m3:= o r)
    (is m3:= (m3:clamp m :min -1.0 :max 1.0) r)
    (is m3:= (m3:clamp m) m)))

(define-test m3/add
  (let ((m1 (m3:make 1 9 4 2 7 9 1 5 2))
        (m2 (m3:make 9 2 3 8 3 8 1 0 1))
        (o (m3:zero))
        (r (m3:make 10 11 7 10 10 17 2 5 3)))
    (is m3:= (m3:+! o m1 m2) r)
    (is m3:= o r)
    (is m3:= (m3:+ m1 m2) r)
    (is m3:= (m3:+ m1 m3:+zero+) m1)
    (is m3:= (m3:+ m2 m3:+zero+) m2)))

(define-test m3/subtract
  (let ((m1 (m3:make 1 9 4 2 7 9 1 5 2))
        (m2 (m3:make 9 2 3 8 3 8 1 0 1))
        (o (m3:zero))
        (r (m3:make -8 7 1 -6 4 1 0 5 1)))
    (is m3:= (m3:-! o m1 m2) r)
    (is m3:= o r)
    (is m3:= (m3:- m1 m2) r)
    (is m3:= (m3:- m1 m3:+zero+) m1)
    (is m3:= (m3:- m2 m3:+zero+) m2)))

(define-test m3/multiply
  (let ((m1 (m3:make 1 5 9 2 6 10 3 7 11))
        (m2 (m3:make 10 50 90 20 60 100 30 70 110))
        (m3 m3:+id+)
        (r (m3:make 38 98 158 44 116 188 50 134 218))
        (rot-z (m3:rotate m3:+id+ (/ pi 3)))
        (tr1 (m3:translate m3:+id+ (v2:make 5 10)))
        (tr2 (m3:translate m3:+id+ (v2:make 10 20)))
        (o (m3:zero)))
    (is m3:= (m3:*! o m1 m1) r)
    (is m3:= o r)
    (is m3:= (m3:* m1 m3) m1)
    (is m3:= (m3:* m3 m1) m1)
    (is m3:= (m3:* m1 m2) (m3:* m2 m1))
    (isnt m3:= (m3:* m1 rot-z) (m3:* rot-z m1))
    (is v2:= (m3:get-translation (m3:* tr1 rot-z)) (m3:get-translation tr1))
    (is v2:= (m3:get-translation (m3:* tr1 tr2)) (v2:make 15 30))
    (is v2:= (m3:get-translation (m3:* tr2 tr1)) (v2:make 15 30))))

(define-test m3/translation-convert
  (let ((m (m3:make 1 5 9 2 6 10 3 7 11))
        (rm (m3:make 1 0 9 0 1 10 0 0 1))
        (om (m3:id))
        (v (v2:make 9 10))
        (rv (v2:make 9 10))
        (ov (v2:zero)))
    (is m3:= (m3:set-translation! om om v) rm)
    (is m3:= om rm)
    (is m3:= (m3:set-translation om v) rm)
    (is v2:= (m3:get-translation! ov m) rv)
    (is v2:= ov rv)
    (is v2:= (m3:get-translation m) rv)))

(define-test m3/translate
  (let ((m (m3:rotate m3:+id+ (/ pi 3)))
        (v (v2:make 5 10)))
    (is v2:= (m3:get-translation (m3:translate m3:+id+ v)) v)
    (is v2:= (m3:get-translation (m3:translate m v)) v)))

(define-test m3/rotation-copy
  (let ((m (m3:make 1 5 0 2 6 0 0 0 1))
        (r (m3:make 1 5 0 2 6 0 0 0 1))
        (o (m3:id)))
    (is m3:= (m3:copy-rotation! o m) r)
    (is m3:= o r)
    (is m3:= (m3:copy-rotation m) r)))

(define-test m3/rotation-convert
  (let ((rmx m3:+id+)
        (omx (m3:id))
        (rvx (v2:make 1 0)))
    (is m3:= (m3:rotation-axis-from-vec2! omx rvx :x) rmx)
    (true (m3:~ omx rmx))
    (is m3:= (m3:rotation-axis-from-vec2 m3:+id+ rvx :x) rmx)))

(define-test m3/rotate
  (let ((omz (m3:id))
        (rmz (m3:make 0.5 -0.86602545 0 0.86602545 0.5 0 0 0 1)))
    (true (m3:~ (m3:rotate! omz m3:+id+ (/ pi 3)) rmz))
    (true (m3:~ omz rmz))
    (true (m3:~ (m3:rotate m3:+id+ (/ pi 3)) rmz))))

(define-test m3/scale
  (let ((m (m3:make 10 0 0 0 20 0 0 0 30))
        (o (m3:id))
        (s (m3:make 10 0 0 0 40 0 0 0 30))
        (v (v2:make 1 2)))
    (true (m3:= (m3:scale! o m v) s))
    (true (m3:= o s))
    (is v2:= (m3:get-scale (m3:scale m3:+id+ v)) v)))

(define-test m3/vec3-multiply
  (let ((m (m3:rotate m3:+id+ (/ pi 3)))
        (v (v3:make 1 2 3))
        (o (v3:zero))
        (rv (v3:make -1.2320509 1.8660254 3)))
    (is v3:= (m3:*v3! o m v) rv)
    (is v3:= o rv)
    (is v3:= (m3:*v3 m v) rv)
    (is v3:= (m3:*v3 m3:+id+ v) v)
    (is v3:= (m3:*v3 m3:+id+ v3:+zero+) v3:+zero+)))

(define-test m3/transpose
  (let ((m (m3:make 1 5 9 2 6 10 3 7 11))
        (r (m3:make 1 2 3 5 6 7 9 10 11))
        (o (m3:id)))
    (is m3:= (m3:transpose! o m) r)
    (is m3:= o r)
    (is m3:= (m3:transpose m) r)
    (is m3:= (m3:transpose m3:+id+) m3:+id+)))

(define-test m3/orthogonal-predicate
  (true (m3:orthogonal-p (m3:rotate m3:+id+ pi)))
  (true (m3:orthogonal-p (m3:rotate m3:+id+ (/ pi 2))))
  (true (m3:orthogonal-p (m3:rotate m3:+id+ (/ pi 3)))))

(define-test m3/trace
  (is = (m3:trace (m3:zero)) 0)
  (is = (m3:trace m3:+id+) 3)
  (is = (m3:trace (m3:make 1 2 3 4 5 6 7 8 9)) 15))

(define-test m3/diagonal
  (let ((m (m3:make 1 2 3 4 5 6 7 8 9))
        (r1 (v3:make 1 5 9))
        (r2 (v3:make 3 5 7))
        (o (v3:zero)))
    (true (m3:diagonal-p (m3:id)))
    (true (not (m3:diagonal-p m)))
    (is v3:= (m3:main-diagonal! o m) r1)
    (is v3:= o r1)
    (is v3:= (m3:main-diagonal m) r1)
    (is v3:= (m3:anti-diagonal! o m) r2)
    (is v3:= o r2)
    (is v3:= (m3:anti-diagonal m) r2)))
