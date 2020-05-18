(in-package #:net.mfiano.lisp.origin)

(int:define-op make-velocity! ((out v3:vec) (axis v3:vec) (rate single-float))
    (:out v3:vec)
  "`AXIS` is a vec3 of any length. `RATE` is in units/second. Returns a velocity
vec3 following the right hand rule whose direction is parallel to `AXIS` and
magnitude is `RATE`. Destructively modifies `OUT`."
  (v3:copy! out axis)
  (v3:normalize! out out)
  (v3:scale! out out rate))

(int:define-op make-velocity ((axis v3:vec) (rate single-float)) (:out v3:vec)
  "`AXIS` is a vec3 of any length. `RATE` is in units/second. Returns a velocity
vec3 following the right hand rule whose direction is parallel to `AXIS` and
magnitude is `RATE`. Allocates a fresh vec3."
  (make-velocity! (v3:vec) axis rate))

(int:define-op velocity->rotation! ((out q:quat) (velocity v3:vec)
                                    (delta single-float))
    (:out q:quat)
  "`VELOCITY` is a vec3 angular velocity whose magnitude represents
radians/second rotation about its vector. `DELTA` is an amount of time. Returns
a normalized quaternion that represents the angular velocity rotation in `DELTA`
amount of time. Destructively modifies `OUT`."
  (q:with-components ((o out))
    (v3:with-components ((av velocity))
      (v3:with-elements ((nav 0f0 0f0 0f0))
        (v3::%normalize navx navy navz avx avy avz)
        (q::%from-axis-angle ow ox oy oz navx navy navz
                             (* (v3:length velocity) delta))
        (q:normalize! out out)))))

(int:define-op velocity->rotation ((velocity v3:vec) (delta single-float))
    (:out q:quat)
  "`VELOCITY` is a vec3 angular velocity whose magnitude represents
radians/second rotation about its vector. `DELTA` is an amount of time. Returns
a normalized quaternion that represents the angular velocity rotation in `DELTA`
amount of time. Allocates a fresh quaternion."
  (velocity->rotation! (q:quat 1) velocity delta))
