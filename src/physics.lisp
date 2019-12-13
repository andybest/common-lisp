(in-package #:cl-user)

(defpackage #:origin.physics
  (:local-nicknames
   (#:v3 #:origin.vec3)
   (#:q #:origin.quat))
  (:use #:cl #:origin.internal)
  (:export
   #:angular-velocity
   #:angular-velocity!
   #:angular-velocity->rotation
   #:angular-velocity->rotation!))

(in-package #:origin.physics)

(define-op angular-velocity! ((out v3:vec) (axis (or v3:vec keyword))
                              (radians-per-second single-float))
    (:out v3:vec)
  "`AXIS` is a vec3 of any length or :X, :Y, :Z for each of the positive local
axes. `RATE` is in radians/second. Returns an angular velocity vec3 following
the right hand rule whose direction is parallel to `AXIS` and magnitude is
`RATE`. Destructively modifies `OUT`."
  (v3:with-components ((o out))
    (case axis
      (:x (psetf ox 1f0 oy 0f0 oz 0f0))
      (:y (psetf ox 0f0 oy 1f0 oz 0f0))
      (:z (psetf ox 0f0 oy 0f0 oz 1f0))
      (t (v3:normalize! out axis)))
    (v3:scale! out out radians-per-second)))

(define-op angular-velocity ((axis (or v3:vec keyword))
                             (radians-per-second single-float))
    (:out v3:vec)
  "`AXIS` is a vec3 of any length or :X, :Y, :Z for each of the positive local
axes. `RATE` is in radians/second. Returns an angular velocity vec3 following
the right hand rule whose direction is parallel to `AXIS` and magnitude is
`RATE`. Allocates a fresh vec3."
  (angular-velocity! (v3:zero) axis radians-per-second))

(define-op angular-velocity->rotation! ((out q:quat) (angular-velocity v3:vec)
                                        (delta single-float))
    (:out q:quat)
  "`ANGULAR-VELOCITY` is a vec3 angular velocity whose magnitude represents
radians/second rotation about its vector. `DELTA` is an amount of time. Returns
a normalized quaternion that represents the angular velocity rotation in `DELTA`
amount of time. Destructively modifies `OUT`."
  (q:with-components ((o out))
    (v3:with-components ((av angular-velocity))
      (v3:with-elements ((nav 0f0 0f0 0f0))
        (v3::%normalize navx navy navz avx avy avz)
        (q::%from-axis-angle ow ox oy oz navx navy navz
                             (* (v3:length angular-velocity) delta))
        (q:normalize! out out)))))

(define-op angular-velocity->rotation ((angular-velocity v3:vec)
                                       (delta single-float))
    (:out q:quat)
  "`ANGULAR-VELOCITY` is a vec3 angular velocity whose magnitude represents
radians/second rotation about its vector. `DELTA` is an amount of time. Returns
a normalized quaternion that represents the angular velocity rotation in `DELTA`
amount of time. Allocates a fresh quaternion."
  (angular-velocity->rotation! (q:id) angular-velocity delta))
