(in-package #:origin.test)

(define-test q/identity
  (let ((q (q:id))
        (r (q:quat 1f0 0f0 0f0 0f0)))
    (is q:= q r)
    (is q:= q:+id+ r)))

(define-test q/equality
  (let ((q1 (q:quat 0.25889468f0 -0.4580922f0 0.6231675f0 0.34003425f0))
        (q2 (q:quat 1e-8 1e-8 1e-8 1e-8)))
    (true (q:= q1 q1))
    (true (q:~ (q:+ q1 q2) q1))
    (true (q:~ q2 (q:zero)))))

(define-test q/copy
  (let ((q (q:quat 0.34003425 -0.4920528 0.8754709 0.6535034))
        (o (q:zero)))
    (is q:= (q:copy! o q) q)
    (is q:= o q)
    (is q:= (q:copy q) q)
    #++(isnt eq q (q:copy q))))

(define-test q/add
  (let ((q1 (q:quat -0.11586404 -0.47056317 0.23266816 -0.6098385))
        (q2 (q:quat -0.81111765 0.11399269 -0.24647212 -0.812474))
        (r (q:quat -0.9269817 -0.35657048 -0.013803959 -1.4223125))
        (o (q:zero)))
    (is q:= (q:+! o q1 q2) r)
    (is q:= o r)
    (is q:= (q:+ q1 q2) r)))

(define-test q/subtract
  (let ((q1 (q:quat 0.1688292 0.5137224 0.83796954 -0.9853494))
        (q2 (q:quat -0.3770373 0.19171429 -0.8571534 0.4451759))
        (r (q:quat 0.5458665 0.32200813 1.695123 -1.4305253))
        (o (q:zero)))
    (is q:= (q:-! o q1 q2) r)
    (is q:= o r)
    (is q:= (q:- q1 q2) r)))

(define-test q/multiply
  (let ((q1 (q:quat 1f0 2f0 3f0 4f0))
        (q2 (q:quat 10f0 20f0 30f0 40f0))
        (q3 q:+id+)
        (r (q:quat -280f0 40f0 60f0 80f0))
        (rot-x (q:rotate-euler q:+id+ (v3:vec origin:pi/3 0f0 0f0)))
        (rot-y (q:rotate-euler q:+id+ (v3:vec 0f0 origin:pi/4 0f0)))
        (rot-xy (q:rotate-euler q:+id+ (v3:vec origin:pi/3 origin:pi/4 0f0)))
        (o (q:zero)))
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
        (o (q:zero)))
    (is q:= (q:scale! o q -0.4920528) r)
    (is q:= o r)
    (is q:= (q:scale q -0.4920528) r)))

(define-test q/cross-product
  (let ((q1 (q:quat 0.8660254f0 0.5f0 0f0 0f0))
        (q2 (q:quat 0.8660254f0 0f0 0.5f0 0f0))
        (r (q:quat 0.75 0f0 0.4330127 0.25))
        (o (q:zero)))
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
        (o (q:zero)))
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
        (o (q:zero)))
    (is q:= (q:normalize! o q) r)
    (is q:= o r)
    (is q:= (q:normalize q) r)
    (is q:= (q:normalize (q:quat 2f0 0f0 0f0 0f0)) q:+id+)))

(define-test q/negate
  (let ((q (q:quat 0.9858451 0.85955405 0.8707795 -0.36954784))
        (r (q:quat -0.9858451 -0.85955405 -0.8707795 0.36954784))
        (o (q:zero)))
    (is q:= (q:negate! o q) r)
    (is q:= o r)
    (is q:= (q:negate q) r)))

(define-test q/inverse)
(let ((q (q:quat 0.19171429 -0.8571534 0.4451759 0.39651704))
      (r (q:quat 0.17012934 0.76064724 -0.39505392 -0.35187355))
      (o (q:zero)))
  (is q:= (q:inverse! o q) r)
  (is q:= o r)
  (is q:= (q:inverse q) r))

(define-test q/rotate-euler
  (let ((oqx (q:id))
        (oqy (q:id))
        (oqz (q:id))
        (rqx (q:quat 0.86602545f0 0.5f0 0f0 0f0))
        (rqy (q:quat 0.86602545f0 0f0 0.5f0 0f0))
        (rqz (q:quat 0.86602545f0 0f0 0f0 0.5f0))
        (vx (v3:vec origin:pi/3 0f0 0f0))
        (vy (v3:vec 0f0 origin:pi/3 0f0))
        (vz (v3:vec 0f0 0f0 origin:pi/3)))
    (true (q:~ (q:rotate-euler! oqx q:+id+ vx) rqx))
    (true (q:~ (q:rotate-euler! oqy q:+id+ vy) rqy))
    (true (q:~ (q:rotate-euler! oqz q:+id+ vz) rqz))
    (true (q:~ oqx rqx))
    (true (q:~ oqy rqy))
    (true (q:~ oqz rqz))
    (true (q:~ (q:rotate-euler q:+id+ vx) rqx))
    (true (q:~ (q:rotate-euler q:+id+ vy) rqy))
    (true (q:~ (q:rotate-euler q:+id+ vz) rqz))))

(define-test q/vec3-convert
  (let* ((q (q:quat 0.3628688 0.9540863 0.017128706 0.32979298))
         (r (v3:vec (aref q 1) (aref q 2) (aref q 3)))
         (o (v3:zero)))
    (is v3:= (q:to-vec3! o q) r)
    (is v3:= o r)
    (is v3:= (q:to-vec3 q) r))
  (let* ((v (v3:vec 0.2571392 0.19932675 -0.025900126))
         (r (q:quat 0f0 (aref v 0) (aref v 1) (aref v 2)))
         (o (q:zero)))
    (is q:= (q:from-vec3! o v) r)
    (is q:= o r)
    (is q:= (q:from-vec3 v) r)))

(define-test q/vec4-convert
  (let* ((q (q:quat 0.3628688 0.9540863 0.017128706 0.32979298))
         (r (v4:vec (aref q 0) (aref q 1) (aref q 2) (aref q 3)))
         (o (v4:zero)))
    (is v4:= (q:to-vec4! o q) r)
    (is v4:= o r)
    (is v4:= (q:to-vec4 q) r))
  (let* ((v (v4:vec 0.2571392 0.19932675 -0.025900126 0.8267517))
         (r (q:quat (aref v 0) (aref v 1) (aref v 2) (aref v 3)))
         (o (q:zero)))
    (is q:= (q:from-vec4! o v) r)
    (is q:= o r)
    (is q:= (q:from-vec4 v) r)))

(define-test q/mat4-convert
  (let ((q (q:rotate-euler q:+id+ (v3:vec origin:pi/3 0f0 0f0)))
        (qo (q:zero))
        (r (m4:mat 1f0 0f0 0f0 0f0 0f0 0.5f0 -0.86602545f0 0f0 0f0 0.86602545f0
                   0.5f0 0f0 0f0 0f0 0f0 1f0))
        (mo (m4:id)))
    (true (m4:~ (q:to-mat4! mo q) r))
    (true (m4:~ mo r))
    (true (m4:~ (q:to-mat4 q) r))
    (true (q:~ (q:from-mat4! qo r) q))
    (true (q:~ qo q))
    (true (q:~ (q:from-mat4 r) q))))

(define-test q/slerp
  (let ((q1 (q:quat -0.15230274 0.7359729 -0.27456188 -0.28505945))
        (q2 (q:quat 0.594954 0.030960321 -0.037411213 -0.02747035))
        (r (q:quat -0.5157237 0.4865686 -0.16367096 -0.17777666))
        (o (q:zero)))
    (is q:= (q:slerp! o q1 q2 0.5) r)
    (is q:= o r)
    (is q:= (q:slerp q1 q2 0.5) r)))
