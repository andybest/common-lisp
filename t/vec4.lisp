(in-package :gamebox-math.test)

(setf *default-test-function* #'equalp)

(plan 118)

(diag "structure")
(is-type +zero-vec4+ '(simple-array single-float (4)))
(is-type (vec4 0 0 0 0) '(simple-array single-float (4)))

(diag "accessors")
(is (v4ref (v4zero) 0) 0)
(is (v4ref (v4zero) 1) 0)
(is (v4ref (v4zero) 2) 0)
(is (v4ref (v4zero) 3) 0)
(let ((v (v4zero)))
  (psetf (v4ref v 0) 10.0 (v4ref v 1) 20.0 (v4ref v 2) 30.0 (v4ref v 3) 40.0)
  (is (v4ref v 0) 10)
  (is (v4ref v 1) 20)
  (is (v4ref v 2) 30)
  (is (v4ref v 3) 40))
(with-vec4 (v (vec4 1 2 3 4))
  (is vx 1)
  (is vy 2)
  (is vz 3)
  (is vw 4)
  (psetf vx 10.0 vy 20.0 vz 30.0 vw 40.0)
  (is vx 10)
  (is vy 20)
  (is vz 30)
  (is vw 40))
(with-vec4s ((v1 (vec4 1 2 3 4)) (v2 (vec4 5 6 7 8)))
  (is v1x 1)
  (is v1y 2)
  (is v1z 3)
  (is v1w 4)
  (is v2x 5)
  (is v2y 6)
  (is v2z 7)
  (is v2w 8)
  (psetf v1x 10.0 v1y 20.0 v1z 30.0 v1w 40.0 v2x 50.0 v2y 60.0 v2z 70.0 v2w 80.0)
  (is v1x 10)
  (is v1y 20)
  (is v1z 30)
  (is v1w 40)
  (is v2x 50)
  (is v2y 60)
  (is v2z 70)
  (is v2w 80))

(diag "copy")
(with-vec4s ((v (vec4 1 2 3 4))
             (o (v4zero)))
  (is (v4cp! o v) v)
  (is o v)
  (is (v4cp v) v)
  (isnt v (v4cp v) :test #'eq))

(diag "clamp")
(with-vec4s ((v (vec4 -1.5185602 0.3374052 1.5218115 1.8188539))
             (r (vec4 -1 0.3374052 1 1))
             (o (v4zero)))
  (is (v4clamp! o v :min -1.0 :max 1.0) r)
  (is o r)
  (is (v4clamp v :min -1.0 :max 1.0) r)
  (is (v4clamp v) v))

(diag "stabilize")
(with-vec4s ((v (vec4 1e-8 1e-8 1e-8 1e-8))
             (o (v4zero)))
  (is (v4stab! o v) +zero-vec4+)
  (is o +zero-vec4+)
  (is (v4stab v) +zero-vec4+))

(diag "zero")
(with-vec4 (v (vec4 -0.72470546 0.57963276 0.8775625 0.44206798))
  (is (v4zero! v) +zero-vec4+)
  (is v +zero-vec4+)
  (is (v4zero) +zero-vec4+))

(diag "list conversion")
(is (v4->list (vec4 1 2 3 4)) '(1 2 3 4))
(is (list->v4 '(1 2 3 4)) (vec4 1 2 3 4))

(diag "equality")
(with-vec4s ((v1 (vec4 0.8598654 -0.4803753 -0.3822465 0.2647184))
             (v2 (vec4 1e-8 1e-8 1e-8 1e-8)))
  (ok (v4= v1 v1))
  (ok (v4~ (v4+ v1 v2) v1))
  (ok (v4~ v2 +zero-vec4+)))

(diag "addition")
(with-vec4s ((v1 (vec4 0.4110496 -0.87680984 -0.62870455 0.6163341))
             (v2 (vec4 0.1166687 0.42538047 0.7360425 0.19508076))
             (r (vec4 0.5277183 -0.45142937 0.10733795 0.81141484))
             (o (v4zero)))
  (is (v4+! o v1 v2) r)
  (is o r)
  (is (v4+ v1 v2) r)
  (is (v4+ v1 +zero-vec4+) v1)
  (is (v4+ +zero-vec4+ v2) v2))

(diag "subtraction")
(with-vec4s ((v1 (vec4 -0.16772795 -0.7287135 -0.8905144 0.55699535))
             (v2 (vec4 -0.69658303 0.6168339 -0.7841997 0.094441175))
             (r (vec4 0.5288551 -1.3455474 -0.10631466 0.46255416))
             (o (v4zero)))
  (is (v4-! o v1 v2) r)
  (is o r)
  (is (v4- v1 v2) r)
  (is (v4- v1 +zero-vec4+) v1))

(diag "hadamard product")
(with-vec4s ((v1 (vec4 -0.6219859 -0.80110574 -0.06880522 0.37676394))
             (v2 (vec4 0.6687746 -0.21906853 0.14335585 0.093762994))
             (r (vec4 -0.4159684 0.17549706 -0.00986363 0.035326514))
             (o (v4zero)))
  (is (v4had*! o v1 v2) r)
  (is o r)
  (is (v4had* v1 v2) r)
  (is (v4had* v1 +zero-vec4+) +zero-vec4+)
  (is (v4had* +zero-vec4+ v2) +zero-vec4+))

(diag "hadamard quotient")
(with-vec4s ((v1 (vec4 0.9498384 0.4066379 -0.72961855 0.9857626))
             (v2 (vec4 0.32331443 0.17439032 -0.65894365 0.91501355))
             (r (vec4 2.9378164 2.3317688 1.1072549 1.0773202))
             (o (v4zero)))
  (is (v4had/! o v1 v2) r)
  (is o r)
  (is (v4had/ v1 v2) r)
  (is (v4had/ v1 +zero-vec4+) +zero-vec4+)
  (is (v4had/ +zero-vec4+ v2) +zero-vec4+))

(diag "scalar product")
(with-vec4s ((v (vec4 0.82007027 -0.53582144 0.11559081 0.31522608))
             (r (vec4 0.7762602 -0.5071966 0.10941568 0.29838598))
             (o (v4zero)))
  (is (v4scale! o v 0.94657767) r)
  (is o r)
  (is (v4scale v 0.94657767) r))

(diag "dot product")
(is (v4dot (vec4 -0.21361923 0.39387107 0.0043354034 0.8267517)
           (vec4 -0.13104868 0.399935 0.62945867 0.44206798))
    0.55372673)
(is (v4dot (vec4 1 0 0 0) (vec4 0 1 0 0)) 0)
(is (v4dot (vec4 1 0 0 0) (vec4 0 0 1 0)) 0)
(is (v4dot (vec4 0 1 0 0) (vec4 0 0 1 0)) 0)
(is (v4dot (vec4 1 0 0 0) (vec4 1 0 0 0)) 1)
(is (v4dot (vec4 1 0 0 0) (vec4 -1 0 0 0)) -1)

(diag "magnitude")
(is (v4mag +zero-vec4+) 0)
(is (v4mag (vec4 1 0 0 0)) 1)
(is (v4mag (vec4 0.32979298 0.2571392 0.19932675 0.2647184)) 0.5335644)

(diag "normalize")
(with-vec4s ((v (vec4 -0.6589291 0.23270178 -0.1047523 0.6163341))
             (r (vec4 -0.70274895 0.24817683 -0.1117185 0.6573213))
             (o (v4zero)))
  (is (v4normalize! o v) r)
  (is o r)
  (is (v4normalize v) r)
  (is (v4normalize (vec4 2 0 0 0)) (vec4 1 0 0 0))
  (is (v4normalize (vec4 0 2 0 0)) (vec4 0 1 0 0))
  (is (v4normalize (vec4 0 0 2 0)) (vec4 0 0 1 0)))

(diag "round")
(with-vec4s ((v (vec4 -0.70498157 0.3615427 0.50702953 0.19508076))
             (r (vec4 -1 0 1 0))
             (o (v4zero)))
  (is (v4round! o v) r)
  (is o r)
  (is (v4round v) r))

(diag "abs")
(with-vec4s ((v (vec4 -0.4241562 -0.52400947 0.82413125 -0.094441175))
             (r (vec4 0.4241562 0.52400947 0.82413125 0.094441175))
             (o (v4zero)))
  (is (v4abs! o v) r)
  (is o r)
  (is (v4abs v) r))

(diag "negate")
(with-vec4s ((v (vec4 0.7823446 0.95027566 -0.4147482 0.55699635))
             (r (vec4 -0.7823446 -0.95027566 0.4147482 -0.55699635))
             (o (v4zero)))
  (is (v4neg! o v) r)
  (is o r)
  (is (v4neg v) r))

(diag "zero vector predicate")
(ok (v4zerop +zero-vec4+))
(ok (v4zerop (vec4 0 0 0 0)))

(diag "linear interpolation")
(with-vec4s ((v1 (vec4 0.74485755 0.092342734 0.2982279 0.093762994))
             (v2 (vec4 0.19426346 0.9881369 0.64691556 0.9857626))
             (r (vec4 0.4695605 0.5402398 0.47257173 0.5397628))
             (o (v4zero)))
  (is (v4lerp! o v1 v2 0.5) r)
  (is o r)
  (is (v4lerp v1 v2 0.5) r)
  (is (v4lerp v1 v2 0.0) v1)
  (is (v4lerp v1 v2 1.0) v2))

(diag "comparators")
(with-vec4s ((v1 (vec4 0.34003425 -0.4920528 0.8754709 0.91501355))
             (v2 (vec4 0.6535034 -0.11586404 -0.47056317 0.91292254))
             (v3 (vec4 0.9715252 0.8300271 0.9858451 0.929777))
             (v4 (vec4 1 2 3 4))
             (v5 (vec4 2 3 4 5)))
  (ok (v4< v2 v3))
  (ok (v4<= v4 v4))
  (ok (v4<= v4 v5))
  (ok (v4> v3 v1))
  (ok (v4>= v4 v4))
  (ok (v4>= v5 v4)))

(diag "component-wise minimum")
(with-vec4s ((v1 (vec4 0.98117805 0.06889212 0.32721102 0.93538976))
             (v2 (vec4 0.8774886 0.25179327 0.76311684 0.31522608))
             (r (vec4 v2x v1y v1z v2w))
             (o (v4zero)))
  (is (v4min! o v1 v2) r)
  (is o r)
  (is (v4min v1 v2) r))

(diag "component-wise maximum")
(with-vec4s ((v1 (vec4 0.64380646 0.38965714 0.2503655 0.45167792))
             (v2 (vec4 0.6341989 0.5274999 0.90044403 0.9411855))
             (r (vec4 v1x v2y v2z v2w))
             (o (v4zero)))
  (is (v4max! o v1 v2) r)
  (is o r)
  (is (v4max v1 v2) r))

(finalize)
