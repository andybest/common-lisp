(in-package #:net.mfiano.lisp.origin.test)

(define-test q/identity
  (let ((q (q:quat))
        (r (q:quat 1 0 0 0)))
    (is q:= q:+id+ r)
    (is q:= (q:id) r)
    (true (q:id-p (q:quat 1)))
    (true (q:id-p (q:id! q)))))

(define-test q/equality
  (let ((q1 (q:quat 0.25889468 -0.4580922 0.6231675 0.34003425))
        (q2 (q:quat 1e-8 1e-8 1e-8 1e-8)))
    (true (q:= q1 q1))
    (true (q:= (q:+ q1 q2) q1))
    (true (q:= q2 (q:quat)))))

(define-test q/copy
  (let ((q (q:quat 0.34003425 -0.4920528 0.8754709 0.6535034))
        (o (q:quat)))
    (is q:= (q:copy! o q) q)
    (is q:= o q)
    (is q:= (q:copy q) q)
    (isnt eq q (q:copy q))))

(define-test q/add
  (let ((q1 (q:quat -0.11586404 -0.47056317 0.23266816 -0.6098385))
        (q2 (q:quat -0.81111765 0.11399269 -0.24647212 -0.812474))
        (r (q:quat -0.9269817 -0.35657048 -0.013803959 -1.4223125))
        (o (q:quat)))
    (is q:= (q:+! o q1 q2) r)
    (is q:= o r)
    (is q:= (q:+ q1 q2) r)))

(define-test q/subtract
  (let ((q1 (q:quat 0.1688292 0.5137224 0.83796954 -0.9853494))
        (q2 (q:quat -0.3770373 0.19171429 -0.8571534 0.4451759))
        (r (q:quat 0.5458665 0.32200813 1.695123 -1.4305253))
        (o (q:quat)))
    (is q:= (q:-! o q1 q2) r)
    (is q:= o r)
    (is q:= (q:- q1 q2) r)))

(define-test q/multiply
  (let ((q1 (q:quat 1 2 3 4))
        (q2 (q:quat 10 20 30 40))
        (q3 q:+id+)
        (r (q:quat -280 40 60 80))
        (rot-x (q:rotate-euler q:+id+ (v3:vec const:pi/3 0 0)))
        (rot-y (q:rotate-euler q:+id+ (v3:vec 0 const:pi/4 0)))
        (rot-xy (q:rotate-euler q:+id+ (v3:vec const:pi/3 const:pi/4 0)))
        (o (q:quat)))
    (is q:= (q:*! o q1 q2) r)
    (is q:= o r)
    (is q:= (q:* q1 q3) q1)
    (is q:= (q:* q3 q1) q1)
    (is q:= (q:* q1 q2) (q:* q2 q1))
    (is q:= (q:* rot-x rot-y) rot-xy)
    (isnt q:= (q:* rot-x rot-y) (q:* rot-y rot-x))))

(define-test q/scalar-product
  (let ((q (q:quat 0.25889468 -0.4580922 0.6231675 0.34003425))
        (r (q:quat -0.12738985 0.22540556 -0.30663133 -0.1673148))
        (o (q:quat)))
    (is q:= (q:scale! o q -0.4920528) r)
    (is q:= o r)
    (is q:= (q:scale q -0.4920528) r)))

(define-test q/cross-product
  (let ((q1 (q:quat 0.8660254 0.5 0 0))
        (q2 (q:quat 0.8660254 0 0.5 0))
        (r (q:quat 0.75 0 0.4330127 0.25))
        (o (q:quat)))
    (is q:= (q:cross! o q1 q2) r)
    (is q:= o r)
    (is q:= (q:cross q1 q2) r)))

(define-test q/dot-product
  (let ((q1 (q:quat -0.55014205 0.66294193 -0.44094658 0.1688292))
        (q2 (q:quat 0.5137224 0.83796954 -0.9853494 -0.3770373)))
    (is = (q:dot q1 q2) 0.64373636)))

(define-test q/conjugate
  (let ((q (q:quat 0.8754709 0.6535034 -0.11586404 -0.47056317))
        (r (q:quat 0.8754709 -0.6535034 0.11586404 0.47056317))
        (o (q:quat)))
    (is q:= (q:conjugate! o q) r)
    (is q:= o r)
    (is q:= (q:conjugate q) r)))

(define-test q/length
  (is = (q:length q:+id+) 1)
  (is = (q:length (q:quat 0.23266816 -0.6098385 -0.81111765 0.11399269))
      1.0473508))

(define-test q/normalize
  (let ((q (q:quat -0.24647212 -0.812474 0.9715252 0.8300271))
        (r (q:quat -0.16065533 -0.52958643 0.6332591 0.5410279))
        (o (q:quat)))
    (is q:= (q:normalize! o q) r)
    (is q:= o r)
    (is q:= (q:normalize q) r)
    (is q:= (q:normalize (q:quat 2 0 0 0)) q:+id+)))

(define-test q/negate
  (let ((q (q:quat 0.9858451 0.85955405 0.8707795 -0.36954784))
        (r (q:quat -0.9858451 -0.85955405 -0.8707795 0.36954784))
        (o (q:quat)))
    (is q:= (q:negate! o q) r)
    (is q:= o r)
    (is q:= (q:negate q) r)))

(define-test q/inverse)
(let ((q (q:quat 0.19171429 -0.8571534 0.4451759 0.39651704))
      (r (q:quat 0.17012934 0.76064724 -0.39505392 -0.35187355))
      (o (q:quat)))
  (is q:= (q:inverse! o q) r)
  (is q:= o r)
  (is q:= (q:inverse q) r))

(define-test q/rotate-euler
  (let ((oqx (q:quat 1))
        (oqy (q:quat 1))
        (oqz (q:quat 1))
        (rqx (q:quat 0.86602545 0.5 0 0))
        (rqy (q:quat 0.86602545 0 0.5 0))
        (rqz (q:quat 0.86602545 0 0 0.5))
        (vx (v3:vec const:pi/3 0 0))
        (vy (v3:vec 0 const:pi/3 0))
        (vz (v3:vec 0 0 const:pi/3)))
    (true (q:= (q:rotate-euler! oqx q:+id+ vx) rqx))
    (true (q:= (q:rotate-euler! oqy q:+id+ vy) rqy))
    (true (q:= (q:rotate-euler! oqz q:+id+ vz) rqz))
    (true (q:= oqx rqx))
    (true (q:= oqy rqy))
    (true (q:= oqz rqz))
    (true (q:= (q:rotate-euler q:+id+ vx) rqx))
    (true (q:= (q:rotate-euler q:+id+ vy) rqy))
    (true (q:= (q:rotate-euler q:+id+ vz) rqz))))

(define-test q/mat4-convert
  (let ((q (q:rotate-euler q:+id+ (v3:vec const:pi/3 0 0)))
        (qo (q:quat))
        (r (m4:mat 1 0 0 0 0 0.5 0.86602545 0 0 -0.86602545 0.5 0 0 0 0 1))
        (mo (m4:mat 1)))
    (true (m4:= (q:to-mat4! mo q) r))
    (true (m4:= mo r))
    (true (m4:= (q:to-mat4 q) r))
    (true (q:= (q:from-mat4! qo r) q))
    (true (q:= qo q))
    (true (q:= (q:from-mat4 r) q))))

(define-test q/slerp
  (let ((q1 (q:quat -0.15230274 0.7359729 -0.27456188 -0.28505945))
        (q2 (q:quat 0.594954 0.030960321 -0.037411213 -0.02747035))
        (r (q:quat -0.5157237 0.4865686 -0.16367096 -0.17777666))
        (o (q:quat)))
    (is q:= (q:slerp! o q1 q2 0.5) r)
    (is q:= o r)
    (is q:= (q:slerp q1 q2 0.5) r)))
