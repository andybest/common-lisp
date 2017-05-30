(defpackage #:gamebox-math.test
  (:use #:cl
        #:gamebox-math
        #:prove))

(in-package :gamebox-math.test)

(diag "Running vector tests.")

(subtest "Vector creation: (vec 1 2 3)"
  (plan 5)
  (with-vector (v (vec 1 2 3))
    (is-type v '(simple-array single-float (3))
        "Vector type is a Common Lisp vector of 3 single-floats")
    (is v #(1 2 3) :test #'equalp
        "Vector is equivalent to #(1 2 3)")
    (is vx 1.0 "Component X is 1.0")
    (is vy 2.0 "Component Y is 2.0")
    (is vz 3.0 "Component Z is 3.0"))
  (finalize))
