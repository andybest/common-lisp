(in-package :gamebox-math)

(defdoc (vec structure)
  "A Euclidean vector of 3 single-float components.
This is a typed structure, which compiles to an actual single-float array, but includes named slots for accessing ~
components.")

(defdoc (with-vector macro)
  "A convenience macro for concisely accessing components of a vector.
Example: (with-vector (v vector) vz) would allow accessing the Z component of the vector as simply the symbol VZ.")

(defdoc (with-vectors macro)
  "A convenience macro for concisely accessing components of multiple vectors.
Example: (with-vectors ((a vector1) (b vector2) (c vector3)) (values ax by cz)) would access the X component of ~
vector1, the Y component of vector2, and the Z component of vector3.")

(defdoc (vref function)
  "A virtualized vector component reader. Use this instead of AREF to prevent unintended behavior should ordering of a ~
vector ever change.")

(defdoc ((setf vref) function)
  "A virtualized vector component writer. Use this instead of (SETF AREF) to prevent unintended behavior should ~
ordering of a vector ever change.")

(defdoc (vec function)
  "Create a new vector.")

(defdoc (vec-copy! function)
  "Copy the components of VEC, storing the result in OUT-VEC.
Alias: VCP!")

(defdoc (vec-copy function)
  "Copy the components of VEC, storing the result in a new vector.
Alias: VCP")

(defdoc (vec-clamp! function)
  "Clamp each component of VEC within the range of [MIN, MAX], storing the result in OUT-VEC.
Alias: VCLAMP!")

(defdoc (vec-clamp function)
  "Clamp each component of VEC within the range of [MIN, MAX], storing the result as a new vector.
Alias: VCLAMP")

(defdoc (vec-stabilize! function)
  "Adjust any component of VEC to zero if below the epsilon TOLERANCE, storing the result in OUT-VEC.
Alias: VSTAB!")

(defdoc (vec-stabilize function)
  "Adjust any component of VEC to zero if below the epsilon TOLERANCE, storing the result as a new vector.
Alias: VSTAB")

(defdoc (vec-zero! function)
  "Set each component of VEC to zero.
Alias: VZERO!")

(defdoc (vec-zero function)
  "Create a new zero vector. This is the same as calling #'VEC with no arguments.
Alias: VZERO")

(defdoc (vec-to-list function)
  "Convert VEC to a list of components.
Alias: V->LIST")

(defdoc (vec-from-list function)
  "Convert LIST to a vector.
Alias: LIST->V")

(defdoc (vec= function)
  "Check if the components of VEC1 are equal to the components of VEC2.
Alias: V=")

(defdoc (vec~ function)
  "Check if the components of VEC1 are approximately equal to the components of VEC2, according to the epsilon TOLERANCE.
Alias: V~~")

(defdoc (vec+! function)
  "Vector addition of VEC1 and VEC2, storing the result in OUT-VEC.
Alias: V+!")

(defdoc (vec+ function)
  "Vector addition of VEC1 and VEC2, storing the result as a new vector.
Alias: V+")

(defdoc (vec-! function)
  "Vector subtraction of VEC2 from VEC1, storing the result in OUT-VEC.
Alias: V-!")

(defdoc (vec- function)
  "Vector subtraction of VEC2 from VEC1, storing the result as a new vector.
Alias: V-")

(defdoc (vec-hadamard*! function)
  "Component-wise multiplication (the Hadamard product) of VEC1 and VEC2, storing the result in OUT-VEC.
Alias: VHAD*!")

(defdoc (vec-hadamard* function)
  "Component-wise multiplication (the Hadamard product) of VEC1 and VEC2, storing the result as a new vector.
Alias: VHAD*")

(defdoc (vec-hadamard/! function)
  "Component-wise division (the Hadamard quotient) of VEC1 by VEC2, storing the result in OUT-VEC.
Alias: VHAD/!")

(defdoc (vec-hadamard/ function)
  "Component-wise division (the Hadamard quotient) of VEC1 by VEC2, storing the result as a new vector.
Alias: VHAD/")

(defdoc (vec-scale! function)
  "Vector scalar multiplication of VEC by SCALAR, storing the result in OUT-VEC.
Alias: VSCALE!")

(defdoc (vec-scale function)
  "Vector scalar multiplication of VEC by SCALAR, storing the result as a new vector.
Alias: VSCALE")

(defdoc (vec-dot function)
  "Compute the dot product of VEC1 and VEC2.
Alias: VDOT")

(defdoc (vec-magnitude-squared function)
  "Compute the magnitude (also known as length or Euclidean norm) of VEC. This results in a squared value, which is ~
cheaper to compute. It is useful when you want to compare relative vector lengths, which does not need the expensive ~
square root function. Use VEC-MAGNITUDE for other cases.
Alias: VMAGSQ")

(defdoc (vec-magnitude function)
  "Compute the magnitude (also known as length or Euclidean norm) of VEC. If you only need to compare lengths of ~
vectors, use VEC-MAGNITUDE-SQUARED instead, as it is cheaper to compute without the square root call of this function.
Alias: VMAG")

(defdoc (vec-normalize! function)
  "Normalize a vector so it has a magnitude of 1.0, storing the result in OUT-VEC.
Alias: VNORMALIZE!")

(defdoc (vec-normalize function)
  "Normalize a vector so it has a magnitude of 1.0, storing the result as a new vector.
Alias: VNORMALIZE")

(defdoc (vec-round! function)
  "Round each component of VEC to the nearest integer, storing the result in OUT-VEC.
Alias: VROUND!")

(defdoc (vec-round function)
  "Round each component of VEC to the nearest integer, storing the result as a new vector.
Alias: VROUND")

(defdoc (vec-abs! function)
  "Modify VEC to have the absolute value of each component, storing the result in OUT-VEC.")

(defdoc (vec-abs function)
  "Modify VEC to have the absolute value of each component, storing the result as a new vector.")

(defdoc (vec-negate! function)
  "Negate each component of VEC, storing the result in OUT-VEC.
Alias: VNEG!")

(defdoc (vec-negate function)
  "Negate each component of VEC, storing the result as a new vector.
Alias: VNEG")

(defdoc (vec-cross! function)
  "Compute the cross product of VEC1 and VEC2, storing the result in OUT-VEC.
Alias: VCROSS!")

(defdoc (vec-cross function)
  "Compute the cross product of VEC1 and VEC2, storing the result as a new vector.
Alias: VCROSS")

(defdoc (vec-box function)
  "Compute the box product of VEC1, VEC2, and VEC3.
Alias: VBOX")

(defdoc (vec-angle function)
  "Compute the angle in radians between VEC1 and VEC2.
Alias: VANGLE")

(defdoc (vec-zero-p function)
  "Check if all components of VEC are zero.
Alias: VZEROP")

(defdoc (vec-direction= function)
  "Check if the directions of VEC1 and VEC2 are the same.
Alias: VDIR=")

(defdoc (vec-parallel-p function)
  "Check if VEC1 and VEC2 are parallel to each other.
Alias: VPARALLELP")

(defdoc (vec-lerp! function)
  "Perform a linear interpolation between VEC1 and VEC2 by the interpolation coefficient COEFF, storing the result ~
in OUT-VEC.
Alias: VLERP!")

(defdoc (vec-lerp function)
  "Perform a linear interpolation between VEC1 and VEC2 by the interpolation coefficient COEFF, storing the result ~
as a new vector.
Alias: VLERP")

(defdoc (vec< function)
  "Check if each component of VEC1 is less than that component of VEC2.
Alias: V<")

(defdoc (vec<= function)
  "Check if each component of VEC1 is less than or equal to that component of VEC2.
Alias: V<=")

(defdoc (vec> function)
  "Check if each component of VEC1 is greater than that component of VEC2.
Alias: V>")

(defdoc (vec< function)
  "Check if each component of VEC1 is greater than or equal to that component of VEC2.
Alias: V>=")

(defdoc (vec-min! function)
  "Component-wise minimum of VEC1 and VEC2, storing the result in OUT-VEC.
Alias: VMIN!")

(defdoc (vec-min function)
  "Component-wise minimum of VEC1 and VEC2, storing the result as a new vector.
Alias: VMIN")

(defdoc (vec-max! function)
  "Component-wise maximum of VEC1 and VEC2, storing the result in OUT-VEC.
Alias: VMAX!")

(defdoc (vec-max function)
  "Component-wise maximum of VEC1 and VEC2, storing the result as a new vector.
Alias: VMAX")
