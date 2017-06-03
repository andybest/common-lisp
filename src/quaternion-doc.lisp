(in-package :gamebox-math)

(defdoc (quat structure)
  "A 4-dimensional complex number used to describe a rotation in 3-dimensions.
This is a typed structure, which compiles to an actual single-float array, but includes named slots for accessing ~
components.")

(defdoc (with-quat macro)
  "A convenience macro for concisely accessing components of a quaternion.
Example: (with-quat (q quat) qw) would allow accessing the W component of the quaternion as simply the symbol QW.")

(defdoc (with-quats macro)
  "A convenience macro for concisely accessing components of multiple quaternions.
Example: (with-quats ((a quaternion1) (b quaternion2)) (values ax by)) would access the X component of quaternion1, ~
and the Y component of quaternion2.")

(defdoc (qref function)
  "A virtualized quaternion component reader. Use this instead of AREF to prevent unintended behavior should ordering ~
of a quaternion ever change.")

(defdoc ((setf qref) function)
  "A virtualized quaternion component writer. Use this instead of (SETF AREF) to prevent unintended behavior should ~
ordering of a quaternion ever change.")

(defdoc (quat function)
  "Create a new quaternion.")

(defdoc (quat-identity! function)
  "Modify the components of QUAT to form an identity quaternion.
Alias: QID!")

(defdoc (quat-identity function)
  "Create an identity quaternion.
Alias: QID")

(defdoc (quat= function)
  "Check if the components of QUAT1 are equal to the components of QUAT2.
Alias Q=")

(defdoc (quat~ function)
  "Check if the components of QUAT1 are approximately equal to the components of QUAT2.
Alias Q~~")

(defdoc (quat-copy! function)
  "Copy the components of QUAT, storing the result in OUT-QUAT.
Alias: QCP!")

(defdoc (quat-copy function)
  "Copy the components of QUAT, storing the result in a new quaternion.
Alias: QCP")

(defdoc (quat+! function)
  "Quaternion addition of QUAT1 and QUAT2, storing the result in OUT-QUAT.
Alias: Q+!")

(defdoc (quat+ function)
  "Quaternion addition of QUAT1 and QUAT2, storing the result as a new quaternion.
Alias: Q+")

(defdoc (quat-! function)
  "Quaternion subtraction of QUAT2 from QUAT1, storing the result in OUT-QUAT.
Alias: Q-!")

(defdoc (quat- function)
  "Quaternion subtraction of QUAT2 from QUAT1, storing the result as a new quaternion.
Alias: Q-")

(defdoc (quat*! function)
  "Quaternion multiplication of QUAT1 and QUAT2, storing the result in OUT-QUAT.
Alias: Q*!")

(defdoc (quat* function)
  "Quaternion multiplication of QUAT1 and QUAT2, storing the result as a new quaternion.
Alias: Q*")

(defdoc (quat-scale! function)
  "Quaternion scalar multiplication of QUAT by SCALAR, storing the result in OUT-QUAT.
Alias: QSCALE!")

(defdoc (quat-scale function)
  "Quaternion scalar multiplication of QUAT by SCALAR, storing the result as a new quaternion.
Alias: QSCALE")

(defdoc (quat-cross! function)
  "Compute the cross product of QUAT1 and QUAT2, storing the result in OUT-QUAT.
Alias: QCROSS!")

(defdoc (quat-cross! function)
  "Compute the cross product of QUAT1 and QUAT2, storing the result as a new quaternion.
Alias: QCROSS")

(defdoc (quat-conjugate! function)
  "Calculate the conjugate of QUAT, storing the result in OUT-QUAT.
Alias: QCONJ!")

(defdoc (quat-conjugate function)
  "Calculate the conjugate of QUAT, storing the result as a new quaternion.
Alias: QCONJ")

(defdoc (quat-magnitude-squared function)
  "Compute the magnitude (also known as length or Euclidean norm) of QUAT. This results in a squared value, which is ~
cheaper to compute.
Alias: QMAGSQ")

(defdoc (quat-magnitude function)
  "Compute the magnitude (also known as length or Euclidean norm) of QUAT.
Alias: QMAG")

(defdoc (quat-normalize! function)
  "Normalize a quaternion so it has a magnitude of 1.0, storing the result in OUT-QUAT.
Alias: QNORMALIZE!")

(defdoc (quat-normalize function)
  "Normalize a quaternion so it has a magnitude of 1.0, storing the result as a new quaternion.
Alias: QNORMALIZE")

(defdoc (quat-negate! function)
  "Negate each component of QUAT, storing the result in OUT-QUAT.
Alias: QNEG!")

(defdoc (quat-negate function)
  "Negate each component of QUAT, storing the result as a new quaternion.
Alias: QNEG")

(defdoc (quat-dot function)
  "Compute the dot product of QUAT1 and QUAT2.
Alias: QDOT")

(defdoc (quat-inverse! function)
  "Compute the multiplicative inverse of QUAT, storing the result in OUT-QUAT.
Alias: QINV!")

(defdoc (quat-inverse function)
  "Compute the multiplicative inverse of QUAT, storing the result as a new quaternion.
Alias: QINV")

(defdoc (quat-rotate! function)
  "Rotate a quaternion in each of 3 dimensions as specified by the vector of radians VEC, storing the result in OUT-QUAT.
Alias: QROT!")

(defdoc (quat-rotate function)
  "Rotate a quaternion in each of 3 dimensions as specified by the vector of radians VEC, storing the result as a new ~
quaternion.
Alias: QROT")

(defdoc (quat-rotate! function)
  "Create a quaternion representing an ANGLE of rotation in radians around AXIS, storing the result in OUT-QUAT.
Alias QROT!")

(defdoc (quat-rotate function)
  "Create a quaternion representing an ANGLE of rotation in radians around AXIS, storing the result as a new ~
quaternion.
Alias: QROT")

(defdoc (quat-to-vec! function)
  "Convert a quaternion to a vector, storing the result in OUT-VEC.
Alias: Q->V!")

(defdoc (quat-to-vec function)
  "Convert a quaternion to a vector, storing the result as a new vector.
Alias: Q->V")

(defdoc (quat-from-vec! function)
  "Convert a vector to a quaternion, storing the result in OUT-QUAT.
Alias: V->Q!")

(defdoc (quat-from-vec function)
  "Convert a vector to a quaternion, storing the result as a new quaternion.
Alias: V->Q")

(defdoc (quat-to-matrix! function)
  "Convert a quaternion to a matrix, storing the result in OUT-MATRIX.
Alias: Q->M!")

(defdoc (quat-to-matrix function)
  "Convert a quaternion to a matrix, storing the result as a new matrix.
Alias: Q->M")

(defdoc (quat-from-matrix! function)
  "Convert a matrix to a quaternion, storing the result in OUT-QUAT.
Alias M->Q!")

(defdoc (quat-from-matrix function)
  "Convert a matrix to a quaternion storing the result as a new quaternion.
Alias: M->Q")

(defdoc (quat-slerp! function)
  "Perform a spherical linear interpolation between QUAT1 and QUAT2 by the interpolation coefficient COEFF, storing ~
the result in OUT-QUAT.
Alias: QSLERP!")

(defdoc (quat-slerp function)
  "Perform a spherical linear interpolation between QUAT1 and QUAT2 by the interpolation coefficient COEFF, storing ~
the result as a new quaternion.
Alias: QSLERP")
