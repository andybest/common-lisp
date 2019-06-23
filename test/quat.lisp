(in-package #:origin.test)

(define-test q/accessors
  (let ((q (q:make 1 2 3 4)))
    (is = (q:w q) 1)
    (is = (q:x q) 2)
    (is = (q:y q) 3)
    (is = (q:z q) 4)
    (psetf (q:w q) 10.0 (q:x q) 20.0 (q:y q) 30.0 (q:z q) 40.0)
    (is = (q:w q) 10)
    (is = (q:x q) 20)
    (is = (q:y q) 30)
    (is = (q:z q) 40)))

(define-test q/identity
  (let ((q (q:id))
        (r (q:make 1 0 0 0)))
    (is q:= q r)
    (is q:= q:+id+ r)))

(define-test q/equality
  (let ((q1 (q:make 0.25889468 -0.4580922 0.6231675 0.34003425))
        (q2 (q:make 1e-8 1e-8 1e-8 1e-8)))
    (true (q:= q1 q1))
    (true (q:~ (q:+ q1 q2) q1))
    (true (q:~ q2 (q:zero)))))

(define-test q/copy
  (let ((q (q:make 0.34003425 -0.4920528 0.8754709 0.6535034))
        (o (q:zero)))
    (is q:= (q:copy! o q) q)
    (is q:= o q)
    (is q:= (q:copy q) q)
    (isnt eq q (q:copy q))))

(define-test q/add
  (let ((q1 (q:make -0.11586404 -0.47056317 0.23266816 -0.6098385))
        (q2 (q:make -0.81111765 0.11399269 -0.24647212 -0.812474))
        (r (q:make -0.9269817 -0.35657048 -0.013803959 -1.4223125))
        (o (q:zero)))
    (is q:= (q:+! o q1 q2) r)
    (is q:= o r)
    (is q:= (q:+ q1 q2) r)))

(define-test q/subtract
  (let ((q1 (q:make 0.1688292 0.5137224 0.83796954 -0.9853494))
        (q2 (q:make -0.3770373 0.19171429 -0.8571534 0.4451759))
        (r (q:make 0.5458665 0.32200813 1.695123 -1.4305253))
        (o (q:zero)))
    (is q:= (q:-! o q1 q2) r)
    (is q:= o r)
    (is q:= (q:- q1 q2) r)))

(define-test q/multiply
  (let ((q1 (q:make 1 2 3 4))
        (q2 (q:make 10 20 30 40))
        (q3 q:+id+)
        (r (q:make -280 40 60 80))
        (rot-x (q:rotate-euler q:+id+ (v3:make (/ pi 3) 0 0)))
        (rot-y (q:rotate-euler q:+id+ (v3:make 0 (/ pi 4) 0)))
        (rot-xy (q:rotate-euler q:+id+ (v3:make (/ pi 3) (/ pi 4) 0)))
        (o (q:zero)))
    (is q:= (q:*! o q1 q2) r)
    (is q:= o r)
    (is q:= (q:* q1 q3) q1)
    (is q:= (q:* q3 q1) q1)
    (is q:= (q:* q1 q2) (q:* q2 q1))
    (is q:= (q:* rot-x rot-y) rot-xy)
    (isnt q:= (q:* rot-x rot-y) (q:* rot-y rot-x))))

(define-test q/scalar-product
  (let ((q (q:make 0.25889468 -0.4580922 0.6231675 0.34003425))
        (r (q:make -0.12738985 0.22540556 -0.30663133 -0.1673148))
        (o (q:zero)))
    (is q:= (q:scale! o q -0.4920528) r)
    (is q:= o r)
    (is q:= (q:scale q -0.4920528) r)))

(define-test q/cross-product
  (let ((q1 (q:make 0.8660254 0.5 0 0))
        (q2 (q:make 0.8660254 0 0.5 0))
        (r (q:make 0.75 0 0.4330127 0.25))
        (o (q:zero)))
    (is q:= (q:cross! o q1 q2) r)
    (is q:= o r)
    (is q:= (q:cross q1 q2) r)))

(define-test q/dot-product
  (let ((q1 (q:make -0.55014205 0.66294193 -0.44094658 0.1688292))
        (q2 (q:make 0.5137224 0.83796954 -0.9853494 -0.3770373)))
    (is = (q:dot q1 q2) 0.64373636)))

(define-test q/conjugate
  (let ((q (q:make 0.8754709 0.6535034 -0.11586404 -0.47056317))
        (r (q:make 0.8754709 -0.6535034 0.11586404 0.47056317))
        (o (q:zero)))
    (is q:= (q:conjugate! o q) r)
    (is q:= o r)
    (is q:= (q:conjugate q) r)))

(define-test q/length
  (is = (q:length q:+id+) 1)
  (is = (q:length (q:make 0.23266816 -0.6098385 -0.81111765 0.11399269))
      1.0473508))

(define-test q/normalize
  (let ((q (q:make -0.24647212 -0.812474 0.9715252 0.8300271))
        (r (q:make -0.16065533 -0.52958643 0.6332591 0.5410279))
        (o (q:zero)))
    (is q:= (q:normalize! o q) r)
    (is q:= o r)
    (is q:= (q:normalize q) r)
    (is q:= (q:normalize (q:make 2 0 0 0)) q:+id+)))

(define-test q/negate
  (let ((q (q:make 0.9858451 0.85955405 0.8707795 -0.36954784))
        (r (q:make -0.9858451 -0.85955405 -0.8707795 0.36954784))
        (o (q:zero)))
    (is q:= (q:negate! o q) r)
    (is q:= o r)
    (is q:= (q:negate q) r)))

(define-test q/inverse)
(let ((q (q:make 0.19171429 -0.8571534 0.4451759 0.39651704))
      (r (q:make 0.17012934 0.76064724 -0.39505392 -0.35187355))
      (o (q:zero)))
  (is q:= (q:inverse! o q) r)
  (is q:= o r)
  (is q:= (q:inverse q) r))

(define-test q/rotate-euler
  (let ((oqx (q:id))
        (oqy (q:id))
        (oqz (q:id))
        (rqx (q:make 0.86602545 0.5 0 0))
        (rqy (q:make 0.86602545 0 0.5 0))
        (rqz (q:make 0.86602545 0 0 0.5))
        (vx (v3:make (/ pi 3) 0 0))
        (vy (v3:make 0 (/ pi 3) 0))
        (vz (v3:make 0 0 (/ pi 3))))
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
  (let* ((q (q:make 0.3628688 0.9540863 0.017128706 0.32979298))
         (r (v3:make (q:x q) (q:y q) (q:z q)))
         (o (v3:zero)))
    (is v3:= (q:to-vec3! o q) r)
    (is v3:= o r)
    (is v3:= (q:to-vec3 q) r))
  (let* ((v (v3:make 0.2571392 0.19932675 -0.025900126))
         (r (q:make 0 (v3:x v) (v3:y v) (v3:z v)))
         (o (q:zero)))
    (is q:= (q:from-vec3! o v) r)
    (is q:= o r)
    (is q:= (q:from-vec3 v) r)))

(define-test q/vec4-convert
  (let* ((q (q:make 0.3628688 0.9540863 0.017128706 0.32979298))
         (r (v4:make (q:w q) (q:x q) (q:y q) (q:z q)))
         (o (v4:zero)))
    (is v4:= (q:to-vec4! o q) r)
    (is v4:= o r)
    (is v4:= (q:to-vec4 q) r))
  (let* ((v (v4:make 0.2571392 0.19932675 -0.025900126 0.8267517))
         (r (q:make (v4:x v) (v4:y v) (v4:z v) (v4:w v)))
         (o (q:zero)))
    (is q:= (q:from-vec4! o v) r)
    (is q:= o r)
    (is q:= (q:from-vec4 v) r)))

(define-test q/mat4-convert
  (let ((q (q:rotate-euler q:+id+ (v3:make (/ pi 3) 0 0)))
        (qo (q:zero))
        (r (m4:make 1 0 0 0 0 0.5 -0.86602545 0 0 0.86602545 0.5 0 0 0 0 1))
        (mo (m4:id)))
    (true (m4:~ (q:to-mat4! mo q) r))
    (true (m4:~ mo r))
    (true (m4:~ (q:to-mat4 q) r))
    (true (q:~ (q:from-mat4! qo r) q))
    (true (q:~ qo q))
    (true (q:~ (q:from-mat4 r) q))))

(define-test q/slerp
  (let ((q1 (q:make -0.15230274 0.7359729 -0.27456188 -0.28505945))
        (q2 (q:make 0.594954 0.030960321 -0.037411213 -0.02747035))
        (r (q:make -0.5157237 0.4865686 -0.16367096 -0.17777666))
        (o (q:zero)))
    (is q:= (q:slerp! o q1 q2 0.5) r)
    (is q:= o r)
    (is q:= (q:slerp q1 q2 0.5) r)))
