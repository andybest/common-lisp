(in-package #:mfiano.math.gfxmath.test)

(plan nil)

(progn
  (oa= (m:vec 1 2) #(1 2) "vector2: vec")
  (oa= (m:vec 1 2 3) #(1 2 3) "vector3: vec")
  (oa= (m:vec 1 2 3 4) #(1 2 3 4) "vector4: vec")
  (o= (m:vec/zero 2) (v 0 0) "vector2: vec/zero")
  (o= (m:vec/zero 3) (v 0 0 0) "vector3: vec/zero")
  (o= (m:vec/zero 4) (v 0 0 0 0) "vector4: vec/zero")
  (o= (m:vec/from-vec 2 (v 1 2)) (v 1 2) "vector2: vec/from-vec: vector2")
  (o= (m:vec/from-vec 2 (v 1 2 3)) (v 1 2) "vector2: vec/from-vec: vector3")
  (o= (m:vec/from-vec 2 (v 1 2 3 4)) (v 1 2) "vector2: vec/from-vec: vector4")
  (o= (m:vec/from-vec 3 (v 1 2)) (v 1 2 0) "vector3: vec/from-vec: vector2")
  (o= (m:vec/from-vec 3 (v 1 2 3)) (v 1 2 3) "vector3: vec/from-vec: vector3")
  (o= (m:vec/from-vec 3 (v 1 2 3 4))
      (v 1 2 3)
      "vector3: vec/from-vec: vector4")
  (o= (m:vec/from-vec 4 (v 1 2))
      (v 1 2 0 0)
      "vector4: vec/from-vec: vector2")
  (o= (m:vec/from-vec 4 (v 1 2 3))
      (v 1 2 3 0)
      "vector4: vec/from-vec: vector3")
  (o= (m:vec/from-vec 4 (v 1 2 3 4))
      (v 1 2 3 4)
      "vector4: vec/from-vec: vector4")
  (o/= (m:vec/random 2) (m:vec/random 2) "vector2: vec/random")
  (o/= (m:vec/random 3) (m:vec/random 3) "vector3: vec/random")
  (o/= (m:vec/random 4) (m:vec/random 4) "vector4: vec/random")
  (o= (m:vec/velocity (v (/ pi 2) (/ pi 3)) 0.4)
      (v 0.3328201 0.22188)
      "vector2: vec/velocity")
  (o= (m:vec/velocity (v (/ pi 2) (/ pi 3) (/ pi 4)) 0.4)
      (v 0.3072885 0.2048590 0.1536442)
      "vector3: vec/velocity")
  (o= (m:vec/velocity (v (/ pi 2) (/ pi 3) (/ pi 4) pi) 0.4)
      (v 0.1676232 0.1117488 0.0838116 0.3352465)
      "vector4: vec/velocity"))

(let ((v2 (v 1 2))
      (v3 (v 1 2 3))
      (v4 (v 1 2 3 4)))
  (s= (m:x v2) 1 "vector2: read x")
  (s= (m:y v2) 2 "vector2: read y")
  (s= (m:x v3) 1 "vector3: read x")
  (s= (m:y v3) 2 "vector3: read y")
  (s= (m:z v3) 3 "vector3: read z")
  (s= (m:x v4) 1 "vector4: read x")
  (s= (m:y v4) 2 "vector4: read y")
  (s= (m:z v4) 3 "vector4: read z")
  (s= (m:w v4) 4 "vector4: read w")
  (setf (m:x v2) 5)
  (setf (m:y v2) 6)
  (setf (m:x v3) 5)
  (setf (m:y v3) 6)
  (setf (m:z v3) 7)
  (setf (m:x v4) 5)
  (setf (m:y v4) 6)
  (setf (m:z v4) 7)
  (setf (m:w v4) 8)
  (s= (m:x v2) 5 "vector2: write x")
  (s= (m:y v2) 6 "vector2: write y")
  (s= (m:x v3) 5 "vector3: write x")
  (s= (m:y v3) 6 "vector3: write y")
  (s= (m:z v3) 7 "vector3: write z")
  (s= (m:x v4) 5 "vector4: write x")
  (s= (m:y v4) 6 "vector4: write y")
  (s= (m:z v4) 7 "vector4: write z")
  (s= (m:w v4) 8 "vector4: write w"))

(let ((v2 (v 1 2))
      (v3 (v 1 2 3))
      (v4 (v 1 2 3 4)))
  (s= (m:ref v2 0) 1 "vector2: read index 0")
  (s= (m:ref v2 1) 2 "vector2: read index 1")
  (s= (m:ref v3 0) 1 "vector3: read index 0")
  (s= (m:ref v3 1) 2 "vector3: read index 1")
  (s= (m:ref v3 2) 3 "vector3: read index 2")
  (s= (m:ref v4 0) 1 "vector4: read index 0")
  (s= (m:ref v4 1) 2 "vector4: read index 1")
  (s= (m:ref v4 2) 3 "vector4: read index 2")
  (s= (m:ref v4 3) 4 "vector4: read index 3")
  (setf (m:ref v2 0) 5)
  (setf (m:ref v2 1) 6)
  (setf (m:ref v3 0) 5)
  (setf (m:ref v3 1) 6)
  (setf (m:ref v3 2) 7)
  (setf (m:ref v4 0) 5)
  (setf (m:ref v4 1) 6)
  (setf (m:ref v4 2) 7)
  (setf (m:ref v4 3) 8)
  (s= (m:ref v2 0) 5 "vector2: write index 0")
  (s= (m:ref v2 1) 6 "vector2: write index 1")
  (s= (m:ref v3 0) 5 "vector3: write index 0")
  (s= (m:ref v3 1) 6 "vector3: write index 1")
  (s= (m:ref v3 2) 7 "vector3: write index 2")
  (s= (m:ref v4 0) 5 "vector4: write index 0")
  (s= (m:ref v4 1) 6 "vector4: write index 1")
  (s= (m:ref v4 2) 7 "vector4: write index 2")
  (s= (m:ref v4 3) 8 "vector4: write index 2"))

(let ((v2 (v 1 2))
      (v3 (v 1 2 3))
      (v4 (v 1 2 3 4)))
  (s= (m:mref v2 0 0) 1 "vector2: read row 0, column 0")
  (s= (m:mref v2 1 0) 2 "vector2: read row 1, column 0")
  (s= (m:mref v3 0 0) 1 "vector3: read row 0, column 0")
  (s= (m:mref v3 1 0) 2 "vector3: read row 1, column 0")
  (s= (m:mref v3 2 0) 3 "vector3: read row 2, column 0")
  (s= (m:mref v4 0 0) 1 "vector4: read row 0, column 0")
  (s= (m:mref v4 1 0) 2 "vector4: read row 1, column 0")
  (s= (m:mref v4 2 0) 3 "vector4: read row 2, column 0")
  (s= (m:mref v4 3 0) 4 "vector4: read row 3, column 0")
  (setf (m:mref v2 0 0) 5)
  (setf (m:mref v2 1 0) 6)
  (setf (m:mref v3 0 0) 5)
  (setf (m:mref v3 1 0) 6)
  (setf (m:mref v3 2 0) 7)
  (setf (m:mref v4 0 0) 5)
  (setf (m:mref v4 1 0) 6)
  (setf (m:mref v4 2 0) 7)
  (setf (m:mref v4 3 0) 8)
  (s= (m:mref v2 0 0) 5 "vector2: write row 0, column 0")
  (s= (m:mref v2 1 0) 6 "vector2: write row 1, column 0")
  (s= (m:mref v3 0 0) 5 "vector3: write row 0, column 0")
  (s= (m:mref v3 1 0) 6 "vector3: write row 1, column 0")
  (s= (m:mref v3 2 0) 7 "vector3: write row 2, column 0")
  (s= (m:mref v4 0 0) 5 "vector4: write row 0, column 0")
  (s= (m:mref v4 1 0) 6 "vector4: write row 1, column 0")
  (s= (m:mref v4 2 0) 7 "vector4: write row 2, column 0")
  (s= (m:mref v4 3 0) 8 "vector4: write row 3, column 0"))

(progn
  (o= m:+v2-zero+ (v0 2) "vector2: constant: zero")
  (o= m:+v3-zero+ (v0 3) "vector3: constant: zero")
  (o= m:+v4-zero+ (v0 4) "vector4: constant: zero")
  (o= m:+v2-ones+ (v 1 1) "vector2: constant: ones")
  (o= m:+v3-ones+ (v 1 1 1) "vector3: constant: ones")
  (o= m:+v4-ones+ (v 1 1 1 1) "vector4: constant: ones")
  (o= m:+v2+x+ (v 1 0) "vector2: constant: +x")
  (o= m:+v2-x+ (v -1 0) "vector2: constant: -x")
  (o= m:+v3+x+ (v 1 0 0) "vector3: constant: +x")
  (o= m:+v3-x+ (v -1 0 0) "vector3: constant: -x")
  (o= m:+v4+x+ (v 1 0 0 0) "vector4: constant: +x")
  (o= m:+v4-x+ (v -1 0 0 0) "vector4: constant: -x")
  (o= m:+v2+y+ (v 0 1) "vector2: constant: +y")
  (o= m:+v2-y+ (v 0 -1) "vector2: constant: -y")
  (o= m:+v3+y+ (v 0 1 0) "vector3: constant: +y")
  (o= m:+v3-y+ (v 0 -1 0) "vector3: constant: -y")
  (o= m:+v4+y+ (v 0 1 0 0) "vector4: constant: +y")
  (o= m:+v4-y+ (v 0 -1 0 0) "vector4: constant: -y")
  (o= m:+v3+z+ (v 0 0 1) "vector3: constant: +z")
  (o= m:+v3-z+ (v 0 0 -1) "vector3: constant: -z")
  (o= m:+v4+z+ (v 0 0 1 0) "vector4: constant: +z")
  (o= m:+v4-z+ (v 0 0 -1 0) "vector4: constant: -z")
  (o= m:+v4+w+ (v 0 0 0 1) "vector4: constant: +w")
  (o= m:+v4-w+ (v 0 0 0 -1) "vector4: constant: -w"))

(let ((v2a (v 0.4110496 -0.87680984))
      (v2b (v 0.1166687 0.42538047))
      (v2-expected1 (v 0.5277183 -0.45142937))
      (v2-expected2 (v 42.4110496 41.12319016))
      (v2-out (v0 2))
      (v3a (v 0.4110496 -0.87680984 -0.62870455))
      (v3b (v 0.1166687 0.42538047 0.7360425))
      (v3-expected1 (v 0.5277183 -0.45142937 0.10733795))
      (v3-expected2 (v 42.4110496 41.12319016 41.3712954))
      (v3-out (v0 3))
      (v4a (v 0.4110496 -0.87680984 -0.62870455 0.6163341))
      (v4b (v 0.1166687 0.42538047 0.7360425 0.19508076))
      (v4-expected1 (v 0.5277183 -0.45142937 0.10733795 0.81141484))
      (v4-expected2 (v 42.4110496 41.12319016 41.3712954 42.6163341))
      (v4-out (v0 4)))
  (o= (m:+ v2a v2b) v2-expected1 "vector2 + vector2 (allocating)")
  (o= (m:+ v3a v3b) v3-expected1 "vector3 + vector3 (allocating)")
  (o= (m:+ v4a v4b) v4-expected1 "vector4 + vector4 (allocating)")
  (o= (m:+ v2a 42) v2-expected2 "vector2 + scalar (allocating)")
  (o= (m:+ v3a 42) v3-expected2 "vector3 + scalar (allocating)")
  (o= (m:+ v4a 42) v4-expected2 "vector4 + scalar (allocating)")
  (m:+! v2a v2b v2-out)
  (m:+! v3a v3b v3-out)
  (m:+! v4a v4b v4-out)
  (o= v2-out v2-expected1 "vector2 + vector2 (in-place)")
  (o= v3-out v3-expected1 "vector3 + vector3 (in-place)")
  (o= v4-out v4-expected1 "vector4 + vector4 (in-place)")
  (m:+! v2a 42 v2-out)
  (m:+! v3a 42 v3-out)
  (m:+! v4a 42 v4-out)
  (o= v2-out v2-expected2 "vector2 + scalar (in-place)")
  (o= v3-out v3-expected2 "vector3 + scalar (in-place)")
  (o= v4-out v4-expected2 "vector4 + scalar (in-place)"))

(let ((v2a (v -0.16772795 -0.7287135))
      (v2b (v -0.69658303 0.6168339))
      (v2-expected1 (v 0.5288551 -1.3455474))
      (v2-expected2 (v -0.5877279 -1.1487135))
      (v2-out (v0 2))
      (v3a (v -0.16772795 -0.7287135 -0.8905144))
      (v3b (v -0.69658303 0.6168339 -0.7841997))
      (v3-expected1 (v 0.5288551 -1.3455474 -0.1063147))
      (v3-expected2 (v -0.5877279 -1.1487135 -1.3105144))
      (v3-out (v0 3))
      (v4a (v -0.16772795 -0.7287135 -0.8905144 0.55699535))
      (v4b (v -0.69658303 0.6168339 -0.7841997 0.094441175))
      (v4-expected1 (v 0.5288551 -1.3455474 -0.1063147 0.46255416))
      (v4-expected2 (v -0.5877279 -1.1487135 -1.3105144 0.1369953))
      (v4-out (v0 4)))
  (o= (m:- v2a v2b) v2-expected1 "vector2 - vector2 (allocating)")
  (o= (m:- v3a v3b) v3-expected1 "vector3 - vector3 (allocating)")
  (o= (m:- v4a v4b) v4-expected1 "vector4 - vector4 (allocating)")
  (o= (m:- v2a 0.42) v2-expected2 "vector2 - scalar (allocating)")
  (o= (m:- v3a 0.42) v3-expected2 "vector3 - scalar (allocating)")
  (o= (m:- v4a 0.42) v4-expected2 "vector4 - scalar (allocating)")
  (m:-! v2a v2b v2-out)
  (m:-! v3a v3b v3-out)
  (m:-! v4a v4b v4-out)
  (o= v2-out v2-expected1 "vector2 - vector2 (in-place)")
  (o= v3-out v3-expected1 "vector3 - vector3 (in-place)")
  (o= v4-out v4-expected1 "vector4 - vector4 (in-place)")
  (m:-! v2a 0.42 v2-out)
  (m:-! v3a 0.42 v3-out)
  (m:-! v4a 0.42 v4-out)
  (o= v2-out v2-expected2 "vector2 - scalar (in-place)")
  (o= v3-out v3-expected2 "vector3 - scalar (in-place)")
  (o= v4-out v4-expected2 "vector4 - scalar (in-place)"))

(let ((v2a (v -0.6219859 -0.80110574))
      (v2b (v 0.6687746 -0.21906853))
      (v2-expected1 (v -0.4159684 0.17549706))
      (v2-expected2 (v -0.5887580 -0.7583088))
      (v2-out (v0 2))
      (v3a (v -0.6219859 -0.80110574 -0.06880522))
      (v3b (v 0.6687746 -0.21906853 0.14335585))
      (v3-expected1 (v -0.4159684 0.17549706 -0.00986363))
      (v3-expected2 (v -0.5887580 -0.7583088 -0.0651295))
      (v3-out (v0 3))
      (v4a (v -0.6219859 -0.80110574 -0.06880522 0.37676394))
      (v4b (v 0.6687746 -0.21906853 0.14335585 0.093762994))
      (v4-expected1 (v -0.4159684 0.17549706 -0.00986363 0.035326514))
      (v4-expected2 (v -0.5887580 -0.7583088 -0.0651295 0.3566363))
      (v4-out (v0 4)))
  (o= (m:* v2a v2b) v2-expected1 "vector2 * vector2 (allocating)")
  (o= (m:* v3a v3b) v3-expected1 "vector3 * vector3 (allocating)")
  (o= (m:* v4a v4b) v4-expected1 "vector4 * vector4 (allocating)")
  (o= (m:* v2a 0.94657767) v2-expected2 "vector2 * scalar (allocating)")
  (o= (m:* v3a 0.94657767) v3-expected2 "vector3 * scalar (allocating)")
  (o= (m:* v4a 0.94657767) v4-expected2 "vector4 * scalar (allocating)")
  (m:*! v2a v2b v2-out)
  (m:*! v3a v3b v3-out)
  (m:*! v4a v4b v4-out)
  (o= v2-out v2-expected1 "vector2 * vector2 (in-place)")
  (o= v3-out v3-expected1 "vector3 * vector3 (in-place)")
  (o= v4-out v4-expected1 "vector4 * vector4 (in-place)")
  (m:*! v2a 0.94657767 v2-out)
  (m:*! v3a 0.94657767 v3-out)
  (m:*! v4a 0.94657767 v4-out)
  (o= v2-out v2-expected2 "vector2 * scalar (in-place)")
  (o= v3-out v3-expected2 "vector3 * scalar (in-place)")
  (o= v4-out v4-expected2 "vector4 * scalar (in-place)"))

(let ((v2a (v 0.9498384 0.4066379))
      (v2b (v 0.32331443 0.17439032))
      (v2-expected1 (v 2.9378164 2.3317688))
      (v2-expected2 (v 1.0034447 0.4295874))
      (v2-out (v0 2))
      (v3a (v 0.9498384 0.4066379 -0.72961855))
      (v3b (v 0.32331443 0.17439032 -0.65894365))
      (v3-expected1 (v 2.9378164 2.3317688 1.1072549))
      (v3-expected2 (v 1.0034448 0.4295875 -0.7707963))
      (v3-out (v0 3))
      (v4a (v 0.9498384 0.4066379 -0.72961855 0.9857626))
      (v4b (v 0.32331443 0.17439032 -0.65894365 0.91501355))
      (v4-expected1 (v 2.9378164 2.3317688 1.1072549 1.0773202))
      (v4-expected2 (v 1.0034447 0.4295874 -0.7707963 1.0413964))
      (v4-out (v0 4)))
  (o= (m:/ v2a v2b) v2-expected1 "vector2 / vector2 (allocating)")
  (o= (m:/ v3a v3b) v3-expected1 "vector3 / vector3 (allocating)")
  (o= (m:/ v4a v4b) v4-expected1 "vector4 / vector4 (allocating)")
  (o= (m:/ v2a 0.94657767) v2-expected2 "vector2 / scalar (allocating)")
  (o= (m:/ v3a 0.94657767) v3-expected2 "vector3 / scalar (allocating)")
  (o= (m:/ v4a 0.94657767) v4-expected2 "vector4 / scalar (allocating)")
  (m:/! v2a v2b v2-out)
  (m:/! v3a v3b v3-out)
  (m:/! v4a v4b v4-out)
  (o= v2-out v2-expected1 "vector2 / vector2 (in-place)")
  (o= v3-out v3-expected1 "vector3 / vector3 (in-place)")
  (o= v4-out v4-expected1 "vector4 / vector4 (in-place)")
  (m:/! v2a 0.94657767 v2-out)
  (m:/! v3a 0.94657767 v3-out)
  (m:/! v4a 0.94657767 v4-out)
  (o= v2-out v2-expected2 "vector2 / scalar (in-place)")
  (o= v3-out v3-expected2 "vector3 / scalar (in-place)")
  (o= v4-out v4-expected2 "vector4 / scalar (in-place)"))

(progn
  (ok (m:< (v 1 2) (v 2 3)) "vector2 < vector2")
  (ok (m:< (v 1 2 3) (v 2 3 4)) "vector3 < vector3")
  (ok (m:< (v 1 2 3 4) (v 2 3 4 5)) "vector4 < vector4"))

(progn
  (ok (m:> (v 2 3) (v 1 2)) "vector2 > vector2")
  (ok (m:> (v 2 3 4) (v 1 2 3)) "vector3 > vector3")
  (ok (m:> (v 2 3 4 5) (v 1 2 3 4)) "vector4 > vector4"))

(progn
  (ok (m:<= (v 1 2) (v 1 2)) "vector2 <= vector2 (test 1)")
  (ok (m:<= (v 1 2) (v 3 4)) "vector2 <= vector2 (test 2)")
  (ok (m:<= (v 1 2 3) (v 1 2 3)) "vector3 <= vector3 (test 1)")
  (ok (m:<= (v 1 2 3) (v 3 4 5)) "vector3 <= vector3 (test 2)")
  (ok (m:<= (v 1 2 3 4) (v 1 2 3 4)) "vector4 <= vector4 (test 1)")
  (ok (m:<= (v 1 2 3 4) (v 3 4 5 6)) "vector4 <= vector4 (test 2)"))

(progn
  (ok (m:>= (v 1 2) (v 1 2)) "vector2 >= vector2 (test 1)")
  (ok (m:>= (v 3 4) (v 1 2)) "vector2 >= vector2 (test 2)")
  (ok (m:>= (v 1 2 3) (v 1 2 3)) "vector3 >= vector3 (test 1)")
  (ok (m:>= (v 3 4 5) (v 1 2 3)) "vector3 >= vector3 (test 2)")
  (ok (m:>= (v 1 2 3 4) (v 1 2 3 4)) "vector4 >= vector4 (test 1)")
  (ok (m:>= (v 3 4 5 6) (v 1 2 3 4)) "vector4 >= vector4 (test 2)"))

(let ((v2 (v -0.4241562 -0.52400947))
      (v2-expected (v 0.4241562 0.52400947))
      (v2-out (v0 2))
      (v3 (v -0.4241562 -0.52400947 0.82413125))
      (v3-expected (v 0.4241562 0.52400947 0.82413125))
      (v3-out (v0 3))
      (v4 (v -0.4241562 -0.52400947 0.82413125 -0.094441175))
      (v4-expected (v 0.4241562 0.52400947 0.82413125 0.094441175))
      (v4-out (v0 4)))
  (o= (m:abs v2) v2-expected "vector2: abs (allocating)")
  (o= (m:abs v3) v3-expected "vector3: abs (allocating)")
  (o= (m:abs v4) v4-expected "vector4: abs (allocating)")
  (m:abs! v2 v2-out)
  (m:abs! v3 v3-out)
  (m:abs! v4 v4-out)
  (o= v2-out v2-expected "vector2: abs (in-place)")
  (o= v3-out v3-expected "vector3: abs (in-place)")
  (o= v4-out v4-expected "vector4: abs (in-place)"))

(let ((v2 (v 0.5 0.5))
      (v2-expected (v (/ pi 3) (/ pi 3)))
      (v2-out (v0 2))
      (v3 (v 0.5 0.5 0.5))
      (v3-expected (v (/ pi 3) (/ pi 3) (/ pi 3)))
      (v3-out (v0 3))
      (v4 (v 0.5 0.5 0.5 0.5))
      (v4-expected (v (/ pi 3) (/ pi 3) (/ pi 3) (/ pi 3)))
      (v4-out (v0 4)))
  (o= (m:acos v2) v2-expected "vector2: acos (allocating)")
  (o= (m:acos v3) v3-expected "vector3: acos (allocating)")
  (o= (m:acos v4) v4-expected "vector4: acos (allocating)")
  (m:acos! v2 v2-out)
  (m:acos! v3 v3-out)
  (m:acos! v4 v4-out)
  (o= v2-out v2-expected "vector2: acos (in-place)")
  (o= v3-out v3-expected "vector3: acos (in-place)")
  (o= v4-out v4-expected "vector4: acos (in-place)"))

(progn
  (s= (m:angle (v 0 1) (v 1 0)) (/ pi 2) "vector2: angle")
  (s= (m:angle (v 0 1 0) (v 1 0 1)) (/ pi 2) "vector3: angle")
  (s= (m:angle (v 0 1 0 0) (v 1 0 1 0)) (/ pi 2) "vector4: angle"))

(let ((v2 (v 1 1))
      (v2-expected (v (/ pi 2) (/ pi 2)))
      (v2-out (v0 2))
      (v3 (v 1 1 1))
      (v3-expected (v (/ pi 2) (/ pi 2) (/ pi 2)))
      (v3-out (v0 3))
      (v4 (v 1 1 1 1))
      (v4-expected (v (/ pi 2) (/ pi 2) (/ pi 2) (/ pi 2)))
      (v4-out (v0 4)))
  (o= (m:asin v2) v2-expected "vector2: asin (allocating)")
  (o= (m:asin v3) v3-expected "vector3: asin (allocating)")
  (o= (m:asin v4) v4-expected "vector4: asin (allocating)")
  (m:asin! v2 v2-out)
  (m:asin! v3 v3-out)
  (m:asin! v4 v4-out)
  (o= v2-out v2-expected "vector2: asin (in-place)")
  (o= v3-out v3-expected "vector3: asin (in-place)")
  (o= v4-out v4-expected "vector4: asin (in-place)"))

(let ((v2 (v 1 1))
      (v2-expected (v (/ pi 4) (/ pi 4)))
      (v2-out (v0 2))
      (v3 (v 1 1 1))
      (v3-expected (v (/ pi 4) (/ pi 4) (/ pi 4)))
      (v3-out (v0 3))
      (v4 (v 1 1 1 1))
      (v4-expected (v (/ pi 4) (/ pi 4) (/ pi 4) (/ pi 4)))
      (v4-out (v0 4)))
  (o= (m:atan v2) v2-expected "vector2: atan (allocating)")
  (o= (m:atan v3) v3-expected "vector3: atan (allocating)")
  (o= (m:atan v4) v4-expected "vector4: atan (allocating)")
  (m:atan! v2 v2-out)
  (m:atan! v3 v3-out)
  (m:atan! v4 v4-out)
  (o= v2-out v2-expected "vector2: atan (in-place)")
  (o= v3-out v3-expected "vector3: atan (in-place)")
  (o= v4-out v4-expected "vector4: atan (in-place)"))

(let ((v2 (v 1.1 2))
      (v2-expected (v 2 2))
      (v2-out (v0 2))
      (v3 (v 1.1 2 3.1))
      (v3-expected (v 2 2 4))
      (v3-out (v0 3))
      (v4 (v 1.1 2 3.1 4))
      (v4-expected (v 2 2 4 4))
      (v4-out (v0 4)))
  (o= (m:ceiling v2) v2-expected "vector2: ceiling (allocating)")
  (o= (m:ceiling v3) v3-expected "vector3: ceiling (allocating)")
  (o= (m:ceiling v4) v4-expected "vector4: ceiling (allocating)")
  (m:ceiling! v2 v2-out)
  (m:ceiling! v3 v3-out)
  (m:ceiling! v4 v4-out)
  (o= v2-out v2-expected "vector2: ceiling (in-place)")
  (o= v3-out v3-expected "vector3: ceiling (in-place)")
  (o= v4-out v4-expected "vector4: ceiling (in-place)"))

(let ((v2 (v -1.5185602 0.3374052))
      (v2-expected (v -1 0.3374052))
      (v2-out (v0 2))
      (v3 (v -1.5185602 0.3374052 2.4824293))
      (v3-expected (v -1 0.3374052 1))
      (v3-out (v0 3))
      (v4 (v -1.5185602 0.3374052 2.4824293 1.8188539))
      (v4-expected (v -1 0.3374052 1 1))
      (v4-out (v0 4)))
  (o= (m:clamp v2 -1 1) v2-expected "vector2: clamp (scalar bounds, allocating)")
  (o= (m:clamp v3 -1 1) v3-expected "vector3: clamp (scalar bounds, allocating)")
  (o= (m:clamp v4 -1 1) v4-expected "vector4: clamp (scalar bounds, allocating)")
  (o= (m:clamp v2 (v -1 -1) (v 1 1)) v2-expected "vector2: clamp (vector bounds, allocating)")
  (o= (m:clamp v3 (v -1 -1 -1) (v 1 1 1)) v3-expected "vector3: clamp (vector bounds, allocating)")
  (o= (m:clamp v4 (v -1 -1 -1 -1) (v 1 1 1 1))
      v4-expected
      "vector4: clamp (vector bounds, allocating)")
  (m:clamp! v2 -1 1 v2-out)
  (m:clamp! v3 -1 1 v3-out)
  (m:clamp! v4 -1 1 v4-out)
  (o= v2-out v2-expected "vector2: clamp (scalar bounds, in-place)")
  (o= v3-out v3-expected "vector3: clamp (scalar bounds, in-place)")
  (o= v4-out v4-expected "vector4: clamp (scalar bounds, in-place)")
  (m:clamp! v2 (v -1 -1) (v 1 1) v2-out)
  (m:clamp! v3 (v -1 -1 -1) (v 1 1 1) v3-out)
  (m:clamp! v4 (v -1 -1 -1 -1) (v 1 1 1 1) v4-out)
  (o= v2-out v2-expected "vector2: clamp (vector bounds, in-place)")
  (o= v3-out v3-expected "vector3: clamp (vector bounds, in-place)")
  (o= v4-out v4-expected "vector4: clamp (vector bounds, in-place)"))

(let ((v2 (v 1 2))
      (v2-out (v0 2))
      (v3 (v 1 2 3))
      (v3-out (v0 3))
      (v4 (v 1 2 3 4))
      (v4-out (v0 4)))
  (ok (not (eq (m:copy v2) v2)) "vector2: copy (allocating)")
  (ok (not (eq (m:copy v3) v3)) "vector3: copy (allocating)")
  (ok (not (eq (m:copy v4) v4)) "vector4: copy (allocating)")
  (m:copy! v2 v2-out)
  (m:copy! v3 v3-out)
  (m:copy! v4 v4-out)
  (o= v2 v2-out "vector2: copy (in-place)")
  (o= v3 v3-out "vector3: copy (in-place)")
  (o= v4 v4-out "vector4: copy (in-place)"))

(let ((v2 (v (/ pi 2) (/ pi 4)))
      (v2-expected (v 0 0.70710677))
      (v2-out (v0 2))
      (v3 (v (/ pi 2) (/ pi 4) (/ pi 3)))
      (v3-expected (v 0 0.70710677 0.5))
      (v3-out (v0 3))
      (v4 (v (/ pi 2) (/ pi 4) (/ pi 3) pi))
      (v4-expected (v 0 0.70710677 0.5 -1))
      (v4-out (v0 4)))
  (o= (m:cos v2) v2-expected "vector2: cos (allocating)")
  (o= (m:cos v3) v3-expected "vector3: cos (allocating)")
  (o= (m:cos v4) v4-expected "vector4: cos (allocating)")
  (m:cos! v2 v2-out)
  (m:cos! v3 v3-out)
  (m:cos! v4 v4-out)
  (o= v2-out v2-expected "vector2: cos (in-place)")
  (o= v3-out v3-expected "vector3: cos (in-place)")
  (o= v4-out v4-expected "vector4: cos (in-place)"))

(let ((a (v 1 0 0))
      (b (v 0 1 0))
      (c (v 0 0 1))
      (out (v0 3)))
  (o= (m:cross a b) (v 0 0 1) "vector3: cross (allocating, test 1)")
  (o= (m:cross a c) (v 0 -1 0) "vector3: cross (allocating, test 2)")
  (o= (m:cross b a) (v 0 0 -1) "vector3: cross (allocating, test 3)")
  (o= (m:cross b c) (v 1 0 0) "vector3: cross (allocating, test 4)")
  (o= (m:cross c a) (v 0 1 0) "vector3: cross (allocating, test 5)")
  (o= (m:cross c b) (v -1 0 0) "vector3: cross (allocating, test 6)")
  (m:cross! a b out)
  (o= out (v 0 0 1) "vector3: cross (in-place, test 1)")
  (m:cross! a c out)
  (o= out (v 0 -1 0) "vector3: cross (in-place, test 2)")
  (m:cross! b a out)
  (o= out (v 0 0 -1) "vector3: cross (in-place, test 3)")
  (m:cross! b c out)
  (o= out (v 1 0 0) "vector3: cross (in-place, test 4)")
  (m:cross! c a out)
  (o= out (v 0 1 0) "vector3: cross (in-place, test 5)")
  (m:cross! c b out)
  (o= out (v -1 0 0) "vector3: cross (in-place, test 6)"))

(progn
  (o= (m:default (v0 2)) (v0 2) "vector2: default")
  (o= (m:default (v0 3)) (v0 3) "vector3: default")
  (o= (m:default (v0 4)) (v0 4) "vector4: default"))

(let ((v2 (v 90 60))
      (v2-expected (v (/ pi 2) (/ pi 3)))
      (v2-out (v0 2))
      (v3 (v 90 60 45))
      (v3-expected (v (/ pi 2) (/ pi 3) (/ pi 4)))
      (v3-out (v0 3))
      (v4 (v 90 60 45 180))
      (v4-expected (v (/ pi 2) (/ pi 3) (/ pi 4) pi))
      (v4-out (v0 4)))
  (o= (m:degrees->radians v2) v2-expected "vector2: degrees->radians (allocating)")
  (o= (m:degrees->radians v3) v3-expected "vector3: degrees->radians (allocating)")
  (o= (m:degrees->radians v4) v4-expected "vector4: degrees->radians (allocating)")
  (m:degrees->radians! v2 v2-out)
  (m:degrees->radians! v3 v3-out)
  (m:degrees->radians! v4 v4-out)
  (o= v2-out v2-expected "vector2: degrees->radians (in-place)")
  (o= v3-out v3-expected "vector3: degrees->radians (in-place)")
  (o= v4-out v4-expected "vector4: degrees->radians (in-place)"))

(progn
  (s= (m:dot (v -0.21361923 0.39387107) (v -0.13104868 0.399935))
      0.18551734
      "vector2: dot")
  (s= (m:dot (v -0.21361923 0.39387107 0.0043354034) (v -0.13104868 0.399935 0.62945867))
      0.1882463
      "vector3: dot")
  (s= (m:dot (v -0.21361923 0.39387107 0.0043354034 0.8267517)
             (v -0.13104868 0.399935 0.62945867 0.44206798))
      0.55372673
      "vector4: dot"))

(let ((v2 (v 2 3))
      (v2-expected (v 4 9))
      (v2-out (v0 2))
      (v3 (v 2 3 4))
      (v3-expected (v 4 9 16))
      (v3-out (v0 3))
      (v4 (v 2 3 4 5))
      (v4-expected (v 4 9 16 25))
      (v4-out (v0 4)))
  (o= (m:expt v2 2) v2-expected "vector2: expt (allocating)")
  (o= (m:expt v3 2) v3-expected "vector3: expt (allocating)")
  (o= (m:expt v4 2) v4-expected "vector4: expt (allocating)")
  (m:expt! v2 2 v2-out)
  (m:expt! v3 2 v3-out)
  (m:expt! v4 2 v4-out)
  (o= v2-out v2-expected "vector2: expt (in-place)")
  (o= v3-out v3-expected "vector3: expt (in-place)")
  (o= v4-out v4-expected "vector4: expt (in-place)"))

(let ((v2 (v 1.8 2.1))
      (v2-expected (v 1 2))
      (v2-out (v0 2))
      (v3 (v 1.8 2.1 3.7))
      (v3-expected (v 1 2 3))
      (v3-out (v0 3))
      (v4 (v 1.8 2.1 3.7 4.2))
      (v4-expected (v 1 2 3 4))
      (v4-out (v0 4)))
  (o= (m:floor v2) v2-expected "vector2: floor (allocating)")
  (o= (m:floor v3) v3-expected "vector3: floor (allocating)")
  (o= (m:floor v4) v4-expected "vector4: floor (allocating)")
  (m:floor! v2 v2-out)
  (m:floor! v3 v3-out)
  (m:floor! v4 v4-out)
  (o= v2-out v2-expected "vector2: floor (in-place)")
  (o= v3-out v3-expected "vector3: floor (in-place)")
  (o= v4-out v4-expected "vector4: floor (in-place)"))

(let ((v2 (v 10.42 -10.42))
      (v2-expected (v 0.42 0.58))
      (v2-out (v0 2))
      (v3 (v 10.42 -10.42 -10.42))
      (v3-expected (v 0.42 0.58 0.58))
      (v3-out (v0 3))
      (v4 (v 10.42 -10.42 -10.42 10.42))
      (v4-expected (v 0.42 0.58 0.58 0.42))
      (v4-out (v0 4)))
  (o= (m:fract v2) v2-expected "vector2: fract (allocating)")
  (o= (m:fract v3) v3-expected "vector3: fract (allocating)")
  (o= (m:fract v4) v4-expected "vector4: fract (allocating)")
  (m:fract! v2 v2-out)
  (m:fract! v3 v3-out)
  (m:fract! v4 v4-out)
  (o= v2-out v2-expected "vector2: fract (in-place)")
  (o= v3-out v3-expected "vector3: fract (in-place)")
  (o= v4-out v4-expected "vector4: fract (in-place)"))

(let ((v2 (v 1 2))
      (v2-expected (v 1 0.5))
      (v2-out (v0 2))
      (v3 (v 1 2 4))
      (v3-expected (v 1 0.5 0.25))
      (v3-out (v0 3))
      (v4 (v 1 2 4 8))
      (v4-expected (v 1 0.5 0.25 0.125))
      (v4-out (v0 4)))
  (o= (m:invert v2) v2-expected "vector2: invert (allocating)")
  (o= (m:invert v3) v3-expected "vector3: invert (allocating)")
  (o= (m:invert v4) v4-expected "vector4: invert (allocating)")
  (m:invert! v2 v2-out)
  (m:invert! v3 v3-out)
  (m:invert! v4 v4-out)
  (o= v2-out v2-expected "vector2: invert (in-place)")
  (o= v3-out v3-expected "vector3: invert (in-place)")
  (o= v4-out v4-expected "vector4: invert (in-place)"))

(let ((v2a (v 0.74485755 0.092342734))
      (v2b (v 0.19426346 0.9881369))
      (v2-expected (v 0.4695605 0.5402398))
      (v2-out (v0 2))
      (v3a (v 0.74485755 0.092342734 0.2982279))
      (v3b (v 0.19426346 0.9881369 0.64691556))
      (v3-expected (v 0.4695605 0.5402398 0.47257173))
      (v3-out (v0 3))
      (v4a (v 0.74485755 0.092342734 0.2982279 0.093762994))
      (v4b (v 0.19426346 0.9881369 0.64691556 0.9857626))
      (v4-expected (v 0.4695605 0.5402398 0.47257173 0.5397628))
      (v4-out (v0 4)))
  (o= (m:lerp v2a v2b 0.5) v2-expected "vector2: lerp (allocating)")
  (o= (m:lerp v3a v3b 0.5) v3-expected "vector3: lerp (allocating)")
  (o= (m:lerp v4a v4b 0.5) v4-expected "vector4: lerp (allocating)")
  (m:lerp! v2a v2b 0.5 v2-out)
  (m:lerp! v3a v3b 0.5 v3-out)
  (m:lerp! v4a v4b 0.5 v4-out)
  (o= v2-out v2-expected "vector2: lerp (in-place)")
  (o= v3-out v3-expected "vector3: lerp (in-place)")
  (o= v4-out v4-expected "vector4: lerp (in-place)"))

(progn
  (s= (m:magnitude (v0 2)) 0 "vector2: magnitude (test 1)")
  (s= (m:magnitude (v 0 1)) 1 "vector2: magnitude (test 2)")
  (s= (m:magnitude (v 0.32979298 0.2571392)) 0.4181913 "vector2: magnitude (test 3)")
  (s= (m:magnitude (v0 3)) 0 "vector3: magnitude (test 1)")
  (s= (m:magnitude (v 0 0 1)) 1 "vector3: magnitude (test 2)")
  (s= (m:magnitude (v 0.32979298 0.2571392 0.19932675)) 0.46326572 "vector3: magnitude (test 3)")
  (s= (m:magnitude (v0 4)) 0 "vector4: magnitude (test 1)")
  (s= (m:magnitude (v 0 0 0 1)) 1 "vector4: magnitude (test 2)")
  (s= (m:magnitude (v 0.32979298 0.2571392 0.19932675 0.2647184))
      0.5335644
      "vector4: magnitude (test 3)"))

(progn
  (pass "vector2: magnitude-squared")
  (pass "vector3: magnitude-squared")
  (pass "vector4: magnitude-squared"))

(let ((v2a (v 1 4))
      (v2b (v 2 3))
      (v2-expected (v 2 4))
      (v2-out (v0 2))
      (v3a (v 1 4 5))
      (v3b (v 2 3 6))
      (v3-expected (v 2 4 6))
      (v3-out (v0 3))
      (v4a (v 1 4 5 8))
      (v4b (v 2 3 6 7))
      (v4-expected (v 2 4 6 8))
      (v4-out (v0 4)))
  (o= (m:max v2a v2b) v2-expected "vector2: max (allocating)")
  (o= (m:max v3a v3b) v3-expected "vector3: max (allocating)")
  (o= (m:max v4a v4b) v4-expected "vector4: max (allocating)")
  (m:max! v2a v2b v2-out)
  (m:max! v3a v3b v3-out)
  (m:max! v4a v4b v4-out)
  (o= v2-out v2-expected "vector2: max (in-place)")
  (o= v3-out v3-expected "vector3: max (in-place)")
  (o= v4-out v4-expected "vector4: max (in-place)"))

(let ((v2a (v 1 4))
      (v2b (v 2 3))
      (v2-expected (v 1 3))
      (v2-out (v0 2))
      (v3a (v 1 4 5))
      (v3b (v 2 3 6))
      (v3-expected (v 1 3 5))
      (v3-out (v0 3))
      (v4a (v 1 4 5 8))
      (v4b (v 2 3 6 7))
      (v4-expected (v 1 3 5 7))
      (v4-out (v0 4)))
  (o= (m:min v2a v2b) v2-expected "vector2: min (allocating)")
  (o= (m:min v3a v3b) v3-expected "vector3: min (allocating)")
  (o= (m:min v4a v4b) v4-expected "vector4: min (allocating)")
  (m:min! v2a v2b v2-out)
  (m:min! v3a v3b v3-out)
  (m:min! v4a v4b v4-out)
  (o= v2-out v2-expected "vector2: min (in-place)")
  (o= v3-out v3-expected "vector3: min (in-place)")
  (o= v4-out v4-expected "vector4: min (in-place)"))

(let ((v2 (v 10 20))
      (v2-expected (v 2 4))
      (v2-out (v0 2))
      (v3 (v 10 20 30))
      (v3-expected (v 2 4 6))
      (v3-out (v0 3))
      (v4 (v 10 20 30 40))
      (v4-expected (v 2 4 6 8))
      (v4-out (v0 4)))
  (o= (m:mod v2 5) v2-expected "vector2: mod (allocating)")
  (o= (m:mod v3 5) v3-expected "vector3: mod (allocating)")
  (o= (m:mod v4 5) v4-expected "vector4: mod (allocating)")
  (m:mod! v2 5 v2-out)
  (m:mod! v3 5 v3-out)
  (m:mod! v4 5 v4-out)
  (o= v2-out v2-expected "vector2: mod (in-place)")
  (o= v3-out v3-expected "vector3: mod (in-place)")
  (o= v4-out v4-expected "vector4: mod (in-place)"))

(let ((v2 (v -0.1677279 -0.7287135))
      (v2-expected (v 0.1677279 0.7287135))
      (v2-out (v0 2))
      (v3 (v -0.1677279 -0.7287135 -0.8905144))
      (v3-expected (v 0.1677279 0.7287135 0.8905144))
      (v3-out (v0 3))
      (v4 (v -0.1677279 -0.7287135 -0.8905144 0.55699535))
      (v4-expected (v 0.1677279 0.7287135 0.8905144 -0.55699535))
      (v4-out (v0 4)))
  (o= (m:negate v2) v2-expected "vector2: negate (allocating)")
  (o= (m:negate v3) v3-expected "vector3: negate (allocating)")
  (o= (m:negate v4) v4-expected "vector4: negate (allocating)")
  (m:negate! v2 v2-out)
  (m:negate! v3 v3-out)
  (m:negate! v4 v4-out)
  (o= v2-out v2-expected "vector2: negate (in-place)")
  (o= v3-out v3-expected "vector3: negate (in-place)")
  (o= v4-out v4-expected "vector4: negate (in-place)"))

(let ((v2 (v -0.6589291 0.23270178))
      (v2-expected (v -0.942928 0.3329964))
      (v2-out (v0 2))
      (v3 (v -0.6589291 0.23270178 -0.1047523))
      (v3-expected (v -0.9325094 0.3293170 -0.1482443))
      (v3-out (v0 3))
      (v4 (v -0.6589291 0.23270178 -0.1047523 0.6163341))
      (v4-expected (v -0.70274895 0.24817683 -0.1117185 0.6573213))
      (v4-out (v0 4)))
  (o= (m:normalize v2) v2-expected "vector2: normalize (allocating)")
  (o= (m:normalize v3) v3-expected "vector3: normalize (allocating)")
  (o= (m:normalize v4) v4-expected "vector4: normalize (allocating)")
  (m:normalize! v2 v2-out)
  (m:normalize! v3 v3-out)
  (m:normalize! v4 v4-out)
  (o= v2-out v2-expected "vector2: normalize (in-place)")
  (o= v3-out v3-expected "vector3: normalize (in-place)")
  (o= v4-out v4-expected "vector4: normalize (in-place)"))

(let ((v2-out (v0 2))
      (v3-out (v0 3))
      (v4-out (v0 4)))
  (o= (m:ones (v0 2)) (v 1 1) "vector2: ones (allocating)")
  (o= (m:ones (v0 3)) (v 1 1 1) "vector3: ones (allocating)")
  (o= (m:ones (v0 4)) (v 1 1 1 1) "vector4: ones (allocating)")
  (m:ones! v2-out)
  (m:ones! v3-out)
  (m:ones! v4-out)
  (o= v2-out (v 1 1) "vector2: ones (in-place)")
  (o= v3-out (v 1 1 1) "vector3: ones (in-place)")
  (o= v4-out (v 1 1 1 1) "vector4: ones (in-place)"))

(progn
  (ok (m:parallel? (v 0 0.12) (v 0 -21.5)) "vector2: parallel?")
  (ok (m:parallel? (v 0 0.12 0) (v 0 -21.5 0)) "vector3: parallel?")
  (ok (m:parallel? (v 0 0.12 0 0) (v 0 -21.5 0 0)) "vector4: parallel?"))

(let ((v2 (v (/ pi 2) (/ pi 3)))
      (v2-expected (v 90 60))
      (v2-out (v0 2))
      (v3 (v (/ pi 2) (/ pi 3) (/ pi 4)))
      (v3-expected (v 90 60 45))
      (v3-out (v0 3))
      (v4 (v (/ pi 2) (/ pi 3) (/ pi 4) pi))
      (v4-expected (v 90 60 45 180))
      (v4-out (v0 4)))
  (o= (m:radians->degrees v2) v2-expected "vector2: radians->degrees (allocating)")
  (o= (m:radians->degrees v3) v3-expected "vector3: radians->degrees (allocating)")
  (o= (m:radians->degrees v4) v4-expected "vector4: radians->degrees (allocating)")
  (m:radians->degrees! v2 v2-out)
  (m:radians->degrees! v3 v3-out)
  (m:radians->degrees! v4 v4-out)
  (o= v2-out v2-expected "vector2: radians->degrees (in-place)")
  (o= v3-out v3-expected "vector3: radians->degrees (in-place)")
  (o= v4-out v4-expected "vector4: radians->degrees (in-place)"))

(let ((v2 (v0 2))
      (v3 (v0 3))
      (v4 (v0 4)))
  (m:random! v2)
  (m:random! v3)
  (m:random! v4)
  (o/= v2 (m:vec/random 2) "vector2: random (in-place)")
  (o/= v3 (m:vec/random 3) "vector3: random (in-place)")
  (o/= v4 (m:vec/random 4) "vector4: random (in-place)"))

(let ((v2 (v 1.5 2))
      (v2-expected (v 2 2))
      (v2-out (v0 2))
      (v3 (v 1.5 2 2.5))
      (v3-expected (v 2 2 2))
      (v3-out (v0 3))
      (v4 (v 1.5 2 2.5 3))
      (v4-expected (v 2 2 2 3))
      (v4-out (v0 4)))
  (o= (m:round v2) v2-expected "vector2: round (allocating)")
  (o= (m:round v3) v3-expected "vector3: round (allocating)")
  (o= (m:round v4) v4-expected "vector4: round (allocating)")
  (m:round! v2 v2-out)
  (m:round! v3 v3-out)
  (m:round! v4 v4-out)
  (o= v2-out v2-expected "vector2: round (in-place)")
  (o= v3-out v3-expected "vector3: round (in-place)")
  (o= v4-out v4-expected "vector4: round (in-place)"))

(progn
  (ok (m:same-direction? (v 0 0.12) (v 0 21.5)) "vector2: same-direction?")
  (ok (m:same-direction? (v 0 0.12 0) (v 0 21.5 0)) "vector3: same-direction?")
  (ok (m:same-direction? (v 0 0.12 0 0) (v 0 21.5 0 0)) "vector4: same-direction?"))

(let ((v2 (v 2 -2))
      (v2-expected (v 1 -1))
      (v2-out (v0 2))
      (v3 (v 2 -2 2))
      (v3-expected (v 1 -1 1))
      (v3-out (v0 3))
      (v4 (v 2 -2 2 -2))
      (v4-expected (v 1 -1 1 -1))
      (v4-out (v0 4)))
  (o= (m:sign v2) v2-expected "vector2: sign (allocating)")
  (o= (m:sign v3) v3-expected "vector3: sign (allocating)")
  (o= (m:sign v4) v4-expected "vector4: sign (allocating)")
  (m:sign! v2 v2-out)
  (m:sign! v3 v3-out)
  (m:sign! v4 v4-out)
  (o= v2-out v2-expected "vector2: sign (in-place)")
  (o= v3-out v3-expected "vector3: sign (in-place)")
  (o= v4-out v4-expected "vector4: sign (in-place)"))

(let ((v2 (v (/ pi 2) (/ pi 4)))
      (v2-expected (v 1 0.70710677))
      (v2-out (v0 2))
      (v3 (v (/ pi 2) (/ pi 4) (/ pi 3)))
      (v3-expected (v 1 0.70710677 0.8660254))
      (v3-out (v0 3))
      (v4 (v (/ pi 2) (/ pi 4) (/ pi 3) pi))
      (v4-expected (v 1 0.70710677 0.8660254 0))
      (v4-out (v0 4)))
  (o= (m:sin v2) v2-expected "vector2: sin (allocating)")
  (o= (m:sin v3) v3-expected "vector3: sin (allocating)")
  (o= (m:sin v4) v4-expected "vector4: sin (allocating)")
  (m:sin! v2 v2-out)
  (m:sin! v3 v3-out)
  (m:sin! v4 v4-out)
  (o= v2-out v2-expected "vector2: sin (in-place)")
  (o= v3-out v3-expected "vector3: sin (in-place)")
  (o= v4-out v4-expected "vector4: sin (in-place)"))

(let ((v2 (v 4 9))
      (v2-expected (v 2 3))
      (v2-out (v0 2))
      (v3 (v 4 9 16))
      (v3-expected (v 2 3 4))
      (v3-out (v0 3))
      (v4 (v 4 9 16 25))
      (v4-expected (v 2 3 4 5))
      (v4-out (v0 4)))
  (o= (m:sqrt v2) v2-expected "vector2: sqrt (allocating)")
  (o= (m:sqrt v3) v3-expected "vector3: sqrt (allocating)")
  (o= (m:sqrt v4) v4-expected "vector4: sqrt (allocating)")
  (m:sqrt! v2 v2-out)
  (m:sqrt! v3 v3-out)
  (m:sqrt! v4 v4-out)
  (o= v2-out v2-expected "vector2: sqrt (in-place)")
  (o= v3-out v3-expected "vector3: sqrt (in-place)")
  (o= v4-out v4-expected "vector4: sqrt (in-place)"))

(let ((v2 (v (/ pi 3) (/ pi 3)))
      (v2-expected (v (sqrt 3) (sqrt 3)))
      (v2-out (v0 2))
      (v3 (v (/ pi 3) (/ pi 3) (/ pi 3)))
      (v3-expected (v (sqrt 3) (sqrt 3) (sqrt 3)))
      (v3-out (v0 3))
      (v4 (v (/ pi 3) (/ pi 3) (/ pi 3) (/ pi 3)))
      (v4-expected (v (sqrt 3) (sqrt 3) (sqrt 3) (sqrt 3)))
      (v4-out (v0 4)))
  (o= (m:tan v2) v2-expected "vector2: tan (allocating)")
  (o= (m:tan v3) v3-expected "vector3: tan (allocating)")
  (o= (m:tan v4) v4-expected "vector4: tan (allocating)")
  (m:tan! v2 v2-out)
  (m:tan! v3 v3-out)
  (m:tan! v4 v4-out)
  (o= v2-out v2-expected "vector2: tan (in-place)")
  (o= v3-out v3-expected "vector3: tan (in-place)")
  (o= v4-out v4-expected "vector4: tan (in-place)"))

(let ((v2 (v 1 2))
      (v3 (v 1 2 3))
      (v4 (v 1 2 3 4)))
  (a= (m:to-array (v 1 2) :single-float) #(1 2) "vector2: to-array (single-float, allocating)")
  (a= (m:to-array (v 1 2 3) :single-float) #(1 2 3) "vector3: to-array (single-float, allocating)")
  (a= (m:to-array (v 1 2 3 4) :single-float)
      #(1 2 3 4)
      "vector4: to-array (single-float, allocating)")
  (a= (m:to-array (v 1 2) :double-float) #(1 2) "vector2: to-array (double-float, allocating)")
  (a= (m:to-array (v 1 2 3) :double-float) #(1 2 3) "vector3: to-array (double-float, allocating)")
  (a= (m:to-array (v 1 2 3 4) :double-float)
      #(1 2 3 4)
      "vector4: to-array (double-float, allocating)")
  (m:to-array! v2 :single-float)
  (m:to-array! v3 :single-float)
  (m:to-array! v4 :single-float)
  (oa= v2 (m::components/single v2) "vector2: to-array (single-float, in-place)")
  (oa= v3 (m::components/single v3) "vector3: to-array (single-float, in-place)")
  (oa= v4 (m::components/single v4) "vector4: to-array (single-float, in-place)")
  (ok (eq (m:to-array! v2 :double-float) (m::components v2))
      "vector2: to-array (double-float, in-place)")
  (ok (eq (m:to-array! v3 :double-float) (m::components v3))
      "vector3: to-array (double-float, in-place)")
  (ok (eq (m:to-array! v4 :double-float) (m::components v4))
      "vector4: to-array (double-float, in-place)"))

(let ((v2 (v (/ pi 2) (/ pi 3)))
      (v2-expected (v 0.3328201 0.22188))
      (v2-out (v0 2))
      (v3 (v (/ pi 2) (/ pi 3) (/ pi 4)))
      (v3-expected (v 0.3072885 0.2048590 0.1536442))
      (v3-out (v0 3))
      (v4 (v (/ pi 2) (/ pi 3) (/ pi 4) pi))
      (v4-expected (v 0.1676232 0.1117488 0.0838116 0.3352465))
      (v4-out (v0 4)))
  (m:velocity! v2 0.4 v2-out)
  (m:velocity! v3 0.4 v3-out)
  (m:velocity! v4 0.4 v4-out)
  (o= v2-out v2-expected "vector2: velocity (in-place)")
  (o= v3-out v3-expected "vector3: velocity (in-place)")
  (o= v4-out v4-expected "vector4: velocity (in-place)"))

(let ((v2 (v0 2))
      (v2-out (v 1 2))
      (v3 (v0 3))
      (v3-out (v 1 2 3))
      (v4 (v0 4))
      (v4-out (v 1 2 3 4)))
  (ok (not (eq (m:zero v2) v2)) "vector2: zero (allocating)")
  (ok (not (eq (m:zero v3) v3)) "vector3: zero (allocating)")
  (ok (not (eq (m:zero v4) v4)) "vector4: zero (allocating)")
  (m:zero! v2-out)
  (m:zero! v3-out)
  (m:zero! v4-out)
  (ok (not (eq v2 (v 1 2))) "vector2: zero (in-place)")
  (ok (not (eq v3 (v 1 2 3))) "vector3: zero (in-place)")
  (ok (not (eq v4 (v 1 2 3 4))) "vector4: zero (in-place)")
  (ok (m:zero? v2) "vector2: zero?")
  (ok (m:zero? v3) "vector3: zero?")
  (ok (m:zero? v4) "vector4: zero?"))

(finalize)