(in-package :gamebox-math)

(defdoc (quat structure)
  "A pair of quaternions used to describe a rotation and translation in ~
3-dimensions.")

(defdoc (with-dquat macro)
  "A convenience macro for concisely accessing components of a dual quaternion.
Example: (with-dquat (d dquat) (values drw ddw)) would allow accessing the W ~
components of the real and dual parts of a dual quaternion as simply the ~
symbols DRW and DDW.")

(defdoc (with-dquats macro)
  "A convenience macro for concisely accessing components of multiple dual ~
quaternions.
Example: (with-dquats ((a dquat1) (b dquat2)) (values arw brw)) would access ~
the W component of dquat1, and the W component of dquaternion.")

(defdoc (dquat function)
  "Create a new dual quaternion.")

(defdoc (dquat-identity! function)
  "Modify the components of DQUAT to form an identity dual quaternion.
Alias: DQID!")

(defdoc (quat-identity function)
  "Create an identity dual quaternion.
Alias: DQID")

(defdoc (dquat= function)
  "Check if the components of DQUAT1 are equal to the components of DQUAT2.
Alias DQ=")

(defdoc (dquat~ function)
  "Check if the components of DQUAT1 are approximately equal to the components ~
of DQUAT2.
Alias DQ~~")

(defdoc (dquat-copy! function)
  "Copy the components of DQUAT, storing the result in OUT-DQUAT.
Alias: DQCP!")

(defdoc (dquat-copy function)
  "Copy the components of DQUAT, storing the result in a new dual quaternion.
Alias: DQCP")

(defdoc (dquat+! function)
  "Dual quaternion addition of DQUAT1 and DQUAT2, storing the result in ~
OUT-DQUAT.
Alias: DQ+!")

(defdoc (dquat+ function)
  "Dual quaternion addition of DQUAT1 and DQUAT2, storing the result as a new ~
dual quaternion.
Alias: DQ+")

(defdoc (dquat-! function)
  "Dual quaternion subtraction of DQUAT2 from DQUAT1, storing the result in ~
OUT-DQUAT.
Alias: DQ-!")

(defdoc (dquat- function)
  "Dual quaternion subtraction of DQUAT2 from DQUAT1, storing the result as a ~
new dual quaternion.
Alias: DQ-")

(defdoc (dquat*! function)
  "Dual quaternion multiplication of DQUAT1 and DQUAT2, storing the result in ~
OUT-DQUAT.
Alias: DQ*!")

(defdoc (dquat* function)
  "Dual quaternion multiplication of DQUAT1 and DQUAT2, storing the result as a ~
new dual quaternion.
Alias: DQ*")

(defdoc (dquat-scale! function)
  "Dual quaternion scalar multiplication of DQUAT by SCALAR, storing the result ~
in OUT-DQUAT.
Alias: DQSCALE!")

(defdoc (dquat-scale function)
  "Dual quaternion scalar multiplication of DQUAT by SCALAR, storing the result ~
as a new dual quaternion.
Alias: DQSCALE")

(defdoc (dquat-conjugate! function)
  "Calculate the conjugate of DQUAT, storing the result in OUT-DQUAT.
Alias: DQCONJ!")

(defdoc (dquat-conjugate function)
  "Calculate the conjugate of DQUAT, storing the result as a new dual quaternion.
Alias: DQCONJ")

(defdoc (dquat-magnitude-squared function)
  "Compute the magnitude (also known as length or Euclidean norm) of the real ~
part of DQUAT. This results in a squared ~
value, which is cheaper to compute.
Alias: DQMAGSQ")

(defdoc (dquat-magnitude function)
  "Compute the magnitude (also known as length or Euclidean norm) of the real ~
part of DQUAT.
Alias: DQMAG")

(defdoc (dquat-normalize! function)
  "Normalize a dual quaternion so its real part has a magnitude of 1.0, storing ~
the result in OUT-DQUAT.
Alias: DQNORMALIZE!")

(defdoc (dquat-normalize function)
  "Normalize a dual quaternion so its real part has a magnitude of 1.0, storing ~
the result as a new dual quaternion.
Alias: DQNORMALIZE")

(defdoc (dquat-negate! function)
  "Negate each component of DQUAT, storing the result in OUT-DQUAT.
Alias: DQNEG!")

(defdoc (dquat-negate function)
  "Negate each component of DQUAT, storing the result as a new dual quaternion.
Alias: DQNEG")

(defdoc (dquat-dot function)
  "Compute the dot product of DQUAT1 and DQUAT2.
Alias: DQDOT")

(defdoc (dquat-inverse! function)
  "Compute the multiplicative inverse of DQUAT, storing the result in OUT-DQUAT.
Alias: DQINV!")

(defdoc (dquat-inverse function)
  "Compute the multiplicative inverse of DQUAT, storing the result as a new ~
dual quaternion.
Alias: DQINV")

(defdoc (dquat-translation-to-vec! function)
  "Decode the translation in the dual part of a dual quaternion, storing the ~
result in OUT-VEC.
Alias: DQTR->V!")

(defdoc (dquat-translation-to-vec function)
  "Decode the translation in the dual part of a dual quaternion, storing the ~
result as a new vector.
Alias: DQTR->V")

(defdoc (dquat-translation-from-vec! function)
  "Encode a translation vector into a dual quaternion, storing the result in ~
OUT-DQUAT.
Alias: V->DQTR!")

(defdoc (dquat-translation-from-vec function)
  "Encode a translation vector into a dual quaternion, storing the result in a ~
new dual quaternion.
Alias: V->DQTR")

(defdoc (dquat-translate! function)
  "Translate a quaternion in each of 3 dimensions as specified by VEC, storing ~
the result in OUT-DQUAT.
Alias: DQTR!")

(defdoc (dquat-translate function)
  "Translate a quaternion in each of 3 dimensions as specified by VEC, storing ~
the result as a new dual quaternion.
Alias: DQTR")

(defdoc (dquat-rotation-to-quat! function)
  "Get the rotation of a dual quaternion, storing the result in OUT-QUAT.
Alias: DQROT->Q!")

(defdoc (dquat-rotation-to-quat function)
  "Get the rotation of a dual quaternion, storing the result as a new quaternion.
Alias: DQROT->Q")

(defdoc (dquat-rotate! function)
  "Rotate a dual quaternion in each of 3 dimensions as specified by the vector ~
of radians VEC, storing the result in ~
OUT-DQUAT.
Alias: DQROT!")

(defdoc (dquat-rotate function)
  "Rotate a dual quaternion in each of 3 dimensions as specified by the vector ~
of radians VEC, storing the result as a ~
new dual quaternion.
Alias: DQROT")

(defdoc (dquat-to-matrix! function)
  "Convert a dual quaternion to a matrix, storing the result in OUT-MATRIX.
Alias: DQ->M!")

(defdoc (dquat-to-matrix function)
  "Convert a dual quaternion to a matrix, storing the result as a new matrix.
Alias: DQ->M")

(defdoc (dquat-to-screw-parameters function)
  "Convert a dual quaternion to a set of 6 screw parameters.
Alias: dq->screw")

(defdoc (dquat-from-screw-parameters! function)
  "Convert a set of 6 screw parameters to a dual quaternion, storing the result ~
in OUT-DQUAT.
Alias: screw->dq!")

(defdoc (dquat-from-screw-parameters function)
  "Convert a set of 6 screw parameters to a dual quaternion, storing the result ~
as a new dual quaternion.
Alias: screw->dq")

(defdoc (dquat-sclerp! function)
  "Perform a screw spherical linear interpolation between DQUAT1 and DQUAT2 by ~
the interpolation coefficient COEFF, ~
storing the result in OUT-DQUAT.
Alias: DQSCLERP!")

(defdoc (dquat-sclerp function)
  "Perform a screw spherical linear interpolation between DQUAT1 and DQUAT2 by ~
the interpolation coefficient COEFF, ~
storing the result as a new dual quaternion.
Alias: DQSCLERP")

(defdoc (dquat-nlerp! function)
  "Perform a normalized linear interpolation between DQUAT1 and DQUAT2 by the ~
interpolation coefficient COEFF, storing ~
the result in OUT-DQUAT.
Alias: DQNLERP!")

(defdoc (dquat-nlerp function)
  "Perform a normalized linear interpolation between DQUAT1 and DQUAT2 by the ~
interpolation coefficient COEFF, storing ~
the result as a new dual quaternion.
Alias: DQNLERP")
