((abs
  :name "abs"
  :parameters ((x generic-float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (abs
  :name "abs"
  :parameters ((x generic-int))
  :return generic-int
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (abs
  :name "abs"
  :parameters ((x generic-double))
  :return generic-double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (acos
  :name "acos"
  :parameters ((x generic-float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (acosh
  :name "acosh"
  :parameters ((x generic-float))
  :return generic-float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (all
  :name "all"
  :parameters ((x generic-bvec))
  :return bool
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (all-invocations
  :name "allInvocations"
  :parameters ((value bool))
  :return bool
  :stages all
  :versions (:460)
  :vulkan t)
 (all-invocations-equal
  :name "allInvocationsEqual"
  :parameters ((value bool))
  :return bool
  :stages all
  :versions (:460)
  :vulkan t)
 (any
  :name "any"
  :parameters ((x generic-bvec))
  :return bool
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (any-invocation
  :name "anyInvocation"
  :parameters ((value bool))
  :return bool
  :stages all
  :versions (:460)
  :vulkan t)
 (asin
  :name "asin"
  :parameters ((x generic-float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (asinh
  :name "asinh"
  :parameters ((x generic-float))
  :return generic-float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (atan
  :name "atan"
  :parameters ((y generic-float) (x generic-float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (atan
  :name "atan"
  :parameters ((y-over-x generic-float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (atanh
  :name "atanh"
  :parameters ((x generic-float))
  :return generic-float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (atomic-add
  :name "atomicAdd"
  :parameters ((mem uint inout) (data uint))
  :return uint
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (atomic-add
  :name "atomicAdd"
  :parameters ((mem int inout) (data int))
  :return int
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (atomic-and
  :name "atomicAnd"
  :parameters ((mem uint inout) (data uint))
  :return uint
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (atomic-and
  :name "atomicAnd"
  :parameters ((mem int inout) (data int))
  :return int
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (atomic-comp-swap
  :name "atomicCompSwap"
  :parameters ((mem uint inout) (compare uint) (data uint))
  :return uint
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (atomic-comp-swap
  :name "atomicCompSwap"
  :parameters ((mem int inout) (compare int) (data int))
  :return int
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (atomic-counter
  :name "atomicCounter"
  :parameters ((c atomic-uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (atomic-counter-add
  :name "atomicCounterAdd"
  :parameters ((c atomic-uint) (data uint))
  :return uint
  :stages all
  :versions (:460)
  :vulkan t)
 (atomic-counter-and
  :name "atomicCounterAnd"
  :parameters ((c atomic-uint) (data uint))
  :return uint
  :stages all
  :versions (:460)
  :vulkan t)
 (atomic-counter-comp-swap
  :name "atomicCounterCompSwap"
  :parameters ((c atomic-uint) (compare uint) (data uint))
  :return uint
  :stages all
  :versions (:460)
  :vulkan t)
 (atomic-counter-decrement
  :name "atomicCounterIncrement"
  :parameters ((c atomic-uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (atomic-counter-exchange
  :name "atomicCounterExchange"
  :parameters ((c atomic-uint) (data uint))
  :return uint
  :stages all
  :versions (:460)
  :vulkan t)
 (atomic-counter-increment
  :name "atomicCounterIncrement"
  :parameters ((c atomic-uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (atomic-counter-max
  :name "atomicCounterMax"
  :parameters ((c atomic-uint) (data uint))
  :return uint
  :stages all
  :versions (:460)
  :vulkan t)
 (atomic-counter-min
  :name "atomicCounterMin"
  :parameters ((c atomic-uint) (data uint))
  :return uint
  :stages all
  :versions (:460)
  :vulkan t)
 (atomic-counter-or
  :name "atomicCounterOr"
  :parameters ((c atomic-uint) (data uint))
  :return uint
  :stages all
  :versions (:460)
  :vulkan t)
 (atomic-counter-subtract
  :name "atomicCounterSubtract"
  :parameters ((c atomic-uint) (data uint))
  :return uint
  :stages all
  :versions (:460)
  :vulkan t)
 (atomic-counter-xor
  :name "atomicCounterXor"
  :parameters ((c atomic-uint) (data uint))
  :return uint
  :stages all
  :versions (:460)
  :vulkan t)
 (atomic-exchange
  :name "atomicExchange"
  :parameters ((mem uint inout) (data uint))
  :return uint
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (atomic-exchange
  :name "atomicExchange"
  :parameters ((mem int inout) (data int))
  :return int
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (atomic-max
  :name "atomicMax"
  :parameters ((mem uint inout) (data uint))
  :return uint
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (atomic-max
  :name "atomicMax"
  :parameters ((mem int inout) (data int))
  :return int
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (atomic-min
  :name "atomicMin"
  :parameters ((mem uint inout) (data uint))
  :return uint
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (atomic-min
  :name "atomicMin"
  :parameters ((mem int inout) (data int))
  :return int
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (atomic-or
  :name "atomicOr"
  :parameters ((mem uint inout) (data uint))
  :return uint
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (atomic-or
  :name "atomicOr"
  :parameters ((mem int inout) (data int))
  :return int
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (atomic-xor
  :name "atomicXor"
  :parameters ((mem uint inout) (data uint))
  :return uint
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (atomic-xor
  :name "atomicXor"
  :parameters ((mem int inout) (data int))
  :return int
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (barrier
     :name "barrier"
   :parameters ()
   :return void
   :stages (tessellation-control compute)
   :versions (:400 :410 :420 :430 :440 :450 :460)
   :vulkan t)
 (bit-count
  :name "bitCount"
  :parameters ((value generic-int))
  :return generic-int
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (bit-count
  :name "bitCount"
  :parameters ((value generic-uint))
  :return generic-int
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (bitfield-extract
  :name "bitfieldExtract"
  :parameters ((value generic-int) (offset int) (bits int))
  :return generic-int
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (bitfield-extract
  :name "bitfieldExtract"
  :parameters ((value generic-uint) (offset int) (bits int))
  :return generic-uint
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (bitfield-insert
  :name "bitfieldInsert"
  :parameters ((base generic-int) (insert generic-int) (offset int) (bits int))
  :return generic-int
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (bitfield-insert
  :name "bitfieldInsert"
  :parameters ((base generic-uint) (insert generic-uint) (offset int) (bits int))
  :return generic-uint
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (bitfield-reverse
  :name "bitfieldReverse"
  :parameters ((value generic-int))
  :return generic-int
  :stages all
  :versions (:400 :410 :420 :430 :440 :450)
  :vulkan t)
 (bitfield-reverse
  :name "bitfieldReverse"
  :parameters ((value generic-int highp))
  :return generic-int
  :stages all
  :versions (:460)
  :vulkan t)
 (bitfield-reverse
  :name "bitfieldReverse"
  :parameters ((value generic-uint))
  :return generic-uint
  :stages all
  :versions (:400 :410 :420 :430 :440 :450)
  :vulkan t)
 (bitfield-reverse
  :name "bitfieldReverse"
  :parameters ((value generic-uint highp))
  :return generic-uint
  :stages all
  :versions (:460)
  :vulkan t)
 (ceil
  :name "ceil"
  :parameters ((x generic-float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (ceil
  :name "ceil"
  :parameters ((x generic-double))
  :return generic-double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (clamp
  :name "clamp"
  :parameters ((x generic-float) (min-val generic-float) (max-val generic-float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (clamp
  :name "clamp"
  :parameters ((x generic-float) (min-val float) (max-val float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (clamp
  :name "clamp"
  :parameters ((x generic-double) (min-val generic-double) (max-val generic-double))
  :return generic-double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (clamp
  :name "clamp"
  :parameters ((x generic-double) (min-val double) (max-val double))
  :return generic-double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (clamp
  :name "clamp"
  :parameters ((x generic-int) (min-val generic-int) (max-val generic-int))
  :return generic-int
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (clamp
  :name "clamp"
  :parameters ((x generic-int) (min-val int) (max-val int))
  :return generic-int
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (clamp
  :name "clamp"
  :parameters ((x generic-uint) (min-val generic-uint) (max-val generic-uint))
  :return generic-uint
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (clamp
  :name "clamp"
  :parameters ((x generic-uint) (min-val uint) (max-val uint))
  :return generic-uint
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (cos
  :name "cos"
  :parameters ((angle generic-float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (cosh
  :name "cosh"
  :parameters ((x generic-float))
  :return generic-float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (cross
  :name "cross"
  :parameters ((x vec3) (y vec3))
  :return vec3
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (cross
  :name "cross"
  :parameters ((x dvec3) (y dvec3))
  :return dvec3
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (degrees
  :name "degrees"
  :parameters ((radians generic-float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (determinant
  :name "determinant"
  :parameters ((m mat2))
  :return float
  :stages all
  :versions (:150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (determinant
  :name "determinant"
  :parameters ((m mat3))
  :return float
  :stages all
  :versions (:150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (determinant
  :name "determinant"
  :parameters ((m mat4))
  :return float
  :stages all
  :versions (:150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (dfdx
  :name "dFdx"
  :parameters ((p generic-float))
  :return generic-float
  :stages (fragment)
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (dfdx-coarse
  :name "dFdxCoarse"
  :parameters ((p generic-float))
  :return generic-float
  :stages (fragment)
  :versions (:450 :460)
  :vulkan t)
 (dfdx-fine
  :name "dFdxFine"
  :parameters ((p generic-float))
  :return generic-float
  :stages (fragment)
  :versions (:450 :460)
  :vulkan t)
 (dfdy
  :name "dFdy"
  :parameters ((p generic-float))
  :return generic-float
  :stages (fragment)
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (dfdy-coarse
  :name "dFdyCoarse"
  :parameters ((p generic-float))
  :return generic-float
  :stages (fragment)
  :versions (:450 :460)
  :vulkan t)
 (dfdy-fine
  :name "dFdyFine"
  :parameters ((p generic-float))
  :return generic-float
  :stages (fragment)
  :versions (:450 :460)
  :vulkan t)
 (distance
  :name "distance"
  :parameters ((p0 generic-float) (p1 generic-float))
  :return float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (distance
  :name "distance"
  :parameters ((p0 generic-double) (p1 generic-double))
  :return double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (dot
  :name "dot"
  :parameters ((x generic-float) (y generic-float))
  :return float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (dot
  :name "dot"
  :parameters ((x generic-double) (y generic-double))
  :return double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (emit-stream-vertex
  :name "EmitStreamVertex"
  :parameters ((stream int))
  :return void
  :stages (geometry)
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (emit-vertex
  :name "EmitVertex"
  :parameters ()
  :return void
  :stages (geometry)
  :versions (:150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (end-stream-primitive
  :name "EndStreamPrimitive"
  :parameters ((stream int))
  :return void
  :stages (geometry)
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (end-primitive
  :name "EndPrimitive"
  :parameters ()
  :return void
  :stages (geometry)
  :versions (:150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (equal
  :name "equal"
  :parameters ((x generic-vec) (y generic-vec))
  :return generic-bvec
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (equal
  :name "equal"
  :parameters ((x generic-ivec) (y generic-ivec))
  :return generic-bvec
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (equal
  :name "equal"
  :parameters ((x generic-uvec) (y generic-uvec))
  :return generic-bvec
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (equal
  :name "equal"
  :parameters ((x generic-bvec) (y generic-bvec))
  :return generic-bvec
  :stages all
  :versions (:120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (exp
  :name "exp"
  :parameters ((x generic-float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (exp2
  :name "exp2"
  :parameters ((x generic-float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (face-forward
  :name "faceforward"
  :parameters ((n generic-float) (i generic-float) (n-ref generic-float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (face-forward
  :name "faceforward"
  :parameters ((n generic-double) (i generic-double) (n-ref generic-double))
  :return generic-double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (find-lsb
  :name "findLSB"
  :parameters ((value generic-int))
  :return generic-int
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (find-lsb
  :name "findLSB"
  :parameters ((value generic-uint))
  :return generic-int
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (find-msb
  :name "findMSB"
  :parameters ((value generic-int))
  :return generic-int
  :stages all
  :versions (:400 :410 :420 :430 :440 :450)
  :vulkan t)
 (find-msb
  :name "findMSB"
  :parameters ((value generic-int highp))
  :return generic-int
  :stages all
  :versions (:460)
  :vulkan t)
 (find-msb
  :name "findMSB"
  :parameters ((value generic-uint))
  :return generic-int
  :stages all
  :versions (:400 :410 :420 :430 :440 :450)
  :vulkan t)
 (find-msb
  :name "findMSB"
  :parameters ((value generic-uint highp))
  :return generic-int
  :stages all
  :versions (:460)
  :vulkan t)
 (float-bits-to-int
  :name "floatBitsToInt"
  :parameters ((value generic-float))
  :return generic-int
  :stages all
  :versions (:330 :400 :410 :420 :430 :440 :450)
  :vulkan t)
 (float-bits-to-int
  :name "floatBitsToInt"
  :parameters ((value generic-float highp))
  :return generic-int
  :stages all
  :versions (:460)
  :vulkan t)
 (float-bits-to-uint
  :name "floatBitsToUInt"
  :parameters ((value generic-float))
  :return generic-uint
  :stages all
  :versions (:330 :400 :410 :420 :430 :440 :450)
  :vulkan t)
 (float-bits-to-uint
  :name "floatBitsToUInt"
  :parameters ((value generic-float highp))
  :return generic-uint
  :stages all
  :versions (:460)
  :vulkan t)
 (floor
  :name "floor"
  :parameters ((x generic-float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (floor
  :name "floor"
  :parameters ((x generic-double))
  :return generic-double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (fma
  :name "fma"
  :parameters ((a generic-float) (b generic-float) (c generic-float))
  :return generic-float
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (fma
  :name "fma"
  :parameters ((a generic-double) (b generic-double) (c generic-double))
  :return generic-double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (fract
  :name "fract"
  :parameters ((x generic-float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (fract
  :name "fract"
  :parameters ((x generic-double))
  :return generic-double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (frexp
  :name "frexp"
  :parameters ((x generic-float) (exp generic-int out))
  :return generic-float
  :stages all
  :versions (:400 :410 :420 :430 :440 :450)
  :vulkan t)
 (frexp
  :name "frexp"
  :parameters ((x generic-float highp) (exp generic-int out highp))
  :return generic-float
  :stages all
  :versions (:460)
  :vulkan t)
 (frexp
  :name "frexp"
  :parameters ((x generic-double) (exp generic-int out))
  :return generic-double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450)
  :vulkan t)
 (frexp
  :name "frexp"
  :parameters ((x generic-double highp) (exp generic-int out))
  :return generic-double
  :stages all
  :versions (:460)
  :vulkan t)
 (ftransform
  :name "ftransform"
  :parameters ()
  :return vec4
  :stages (vertex)
  :versions ((:120 :130 :150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat))
  :vulkan nil)
 (fwidth
  :name "fwidth"
  :parameters ((p generic-float))
  :return generic-float
  :stages (fragment)
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (fwidth-coarse
  :name "fwidthCoarse"
  :parameters ((p generic-float))
  :return generic-float
  :stages (fragment)
  :versions (:450 :460)
  :vulkan t)
 (fwidth-fine
  :name "fwidthFine"
  :parameters ((p generic-float))
  :return generic-float
  :stages (fragment)
  :versions (:450 :460)
  :vulkan t)
 (greater-than
  :name "greaterThan"
  :parameters ((x generic-vec) (y generic-vec))
  :return generic-bvec
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (greater-than
  :name "greaterThan"
  :parameters ((x generic-ivec) (y generic-ivec))
  :return generic-bvec
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (greater-than
  :name "greaterThan"
  :parameters ((x generic-uvec) (y generic-uvec))
  :return generic-bvec
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (greater-than-equal
  :name "greaterThanEqual"
  :parameters ((x generic-vec) (y generic-vec))
  :return generic-bvec
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (greater-than-equal
  :name "greaterThanEqual"
  :parameters ((x generic-ivec) (y generic-ivec))
  :return generic-bvec
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (greater-than-equal
  :name "greaterThanEqual"
  :parameters ((x generic-uvec) (y generic-uvec))
  :return generic-bvec
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (group-memory-barrier
  :name "groupMemoryBarrier"
  :parameters ()
  :return void
  :stages (compute)
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (image-atomic-add
  :name "imageAtomicAdd"
  :parameters ((image generic-image-2d readonly) (p ivec2) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-add
  :name "imageAtomicAdd"
  :parameters ((image generic-image-3d readonly) (p ivec3) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-add
  :name "imageAtomicAdd"
  :parameters ((image generic-image-cube readonly) (p ivec3) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-add
  :name "imageAtomicAdd"
  :parameters ((image generic-image-buffer readonly) (p int) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-add
  :name "imageAtomicAdd"
  :parameters ((image generic-image-2d-array readonly) (p ivec3) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-add
  :name "imageAtomicAdd"
  :parameters ((image generic-image-cube-array readonly) (p ivec3) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-add
  :name "imageAtomicAdd"
  :parameters ((image generic-image-1d readonly) (p int) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-add
  :name "imageAtomicAdd"
  :parameters ((image generic-image-1d-array readonly) (p ivec2) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-add
  :name "imageAtomicAdd"
  :parameters ((image generic-image-2d-rect readonly) (p ivec2) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-add
  :name "imageAtomicAdd"
  :parameters ((image generic-image-2d-ms readonly) (p ivec2) (sample int) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-add
  :name "imageAtomicAdd"
  :parameters ((image generic-image-2d-ms-array readonly) (p ivec3) (sample int) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-add
  :name "imageAtomicAdd"
  :parameters ((image generic-image-2d readonly) (p ivec2) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-add
  :name "imageAtomicAdd"
  :parameters ((image generic-image-3d readonly) (p ivec3) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-add
  :name "imageAtomicAdd"
  :parameters ((image generic-image-cube readonly) (p ivec3) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-add
  :name "imageAtomicAdd"
  :parameters ((image generic-image-buffer readonly) (p int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-add
  :name "imageAtomicAdd"
  :parameters ((image generic-image-2d-array readonly) (p ivec3) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-add
  :name "imageAtomicAdd"
  :parameters ((image generic-image-cube-array readonly) (p ivec3) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-add
  :name "imageAtomicAdd"
  :parameters ((image generic-image-1d readonly) (p int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-add
  :name "imageAtomicAdd"
  :parameters ((image generic-image-1d-array readonly) (p ivec2) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-add
  :name "imageAtomicAdd"
  :parameters ((image generic-image-2d-rect readonly) (p ivec2) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-add
  :name "imageAtomicAdd"
  :parameters ((image generic-image-2d-ms readonly) (p ivec2) (sample int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-add
  :name "imageAtomicAdd"
  :parameters ((image generic-image-2d-ms-array readonly) (p ivec3) (sample int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-and
  :name "imageAtomicAnd"
  :parameters ((image generic-image-2d readonly) (p ivec2) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-and
  :name "imageAtomicAnd"
  :parameters ((image generic-image-3d readonly) (p ivec3) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-and
  :name "imageAtomicAnd"
  :parameters ((image generic-image-cube readonly) (p ivec3) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-and
  :name "imageAtomicAnd"
  :parameters ((image generic-image-buffer readonly) (p int) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-and
  :name "imageAtomicAnd"
  :parameters ((image generic-image-2d-array readonly) (p ivec3) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-and
  :name "imageAtomicAnd"
  :parameters ((image generic-image-cube-array readonly) (p ivec3) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-and
  :name "imageAtomicAnd"
  :parameters ((image generic-image-1d readonly) (p int) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-and
  :name "imageAtomicAnd"
  :parameters ((image generic-image-1d-array readonly) (p ivec2) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-and
  :name "imageAtomicAnd"
  :parameters ((image generic-image-2d-rect readonly) (p ivec2) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-and
  :name "imageAtomicAnd"
  :parameters ((image generic-image-2d-ms readonly) (p ivec2) (sample int) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-and
  :name "imageAtomicAnd"
  :parameters ((image generic-image-2d-ms-array readonly) (p ivec3) (sample int) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-and
  :name "imageAtomicAnd"
  :parameters ((image generic-image-2d readonly) (p ivec2) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-and
  :name "imageAtomicAnd"
  :parameters ((image generic-image-3d readonly) (p ivec3) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-and
  :name "imageAtomicAnd"
  :parameters ((image generic-image-cube readonly) (p ivec3) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-and
  :name "imageAtomicAnd"
  :parameters ((image generic-image-buffer readonly) (p int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-and
  :name "imageAtomicAnd"
  :parameters ((image generic-image-2d-array readonly) (p ivec3) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-and
  :name "imageAtomicAnd"
  :parameters ((image generic-image-cube-array readonly) (p ivec3) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-and
  :name "imageAtomicAnd"
  :parameters ((image generic-image-1d readonly) (p int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-and
  :name "imageAtomicAnd"
  :parameters ((image generic-image-1d-array readonly) (p ivec2) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-and
  :name "imageAtomicAnd"
  :parameters ((image generic-image-2d-rect readonly) (p ivec2) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-and
  :name "imageAtomicAnd"
  :parameters ((image generic-image-2d-ms readonly) (p ivec2) (sample int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-and
  :name "imageAtomicAnd"
  :parameters ((image generic-image-2d-ms-array readonly) (p ivec3) (sample int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-comp-swap
  :name "imageAtomicCompSwap"
  :parameters ((image generic-image-2d readonly) (p ivec2) (compare uint) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-comp-swap
  :name "imageAtomicCompSwap"
  :parameters ((image generic-image-3d readonly) (p ivec3) (compare uint) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-comp-swap
  :name "imageAtomicCompSwap"
  :parameters ((image generic-image-cube readonly) (p ivec3) (compare uint) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-comp-swap
  :name "imageAtomicCompSwap"
  :parameters ((image generic-image-buffer readonly) (p int) (compare uint) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-comp-swap
  :name "imageAtomicCompSwap"
  :parameters ((image generic-image-2d-array readonly) (p ivec3) (compare uint) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-comp-swap
  :name "imageAtomicCompSwap"
  :parameters ((image generic-image-cube-array readonly) (p ivec3) (compare uint) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-comp-swap
  :name "imageAtomicCompSwap"
  :parameters ((image generic-image-1d readonly) (p int) (compare uint) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-comp-swap
  :name "imageAtomicCompSwap"
  :parameters ((image generic-image-1d-array readonly) (p ivec2) (compare uint) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-comp-swap
  :name "imageAtomicCompSwap"
  :parameters ((image generic-image-2d-rect readonly) (p ivec2) (compare uint) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-comp-swap
  :name "imageAtomicCompSwap"
  :parameters ((image generic-image-2d-ms readonly) (p ivec2) (sample int) (compare uint) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-comp-swap
  :name "imageAtomicCompSwap"
  :parameters ((image generic-image-2d-ms-array readonly) (p ivec3) (sample int) (compare uint) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-comp-swap
  :name "imageAtomicCompSwap"
  :parameters ((image generic-image-2d readonly) (p ivec2) (compare int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-comp-swap
  :name "imageAtomicCompSwap"
  :parameters ((image generic-image-3d readonly) (p ivec3) (compare int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-comp-swap
  :name "imageAtomicCompSwap"
  :parameters ((image generic-image-cube readonly) (p ivec3) (compare int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-comp-swap
  :name "imageAtomicCompSwap"
  :parameters ((image generic-image-buffer readonly) (p int) (compare int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-comp-swap
  :name "imageAtomicCompSwap"
  :parameters ((image generic-image-2d-array readonly) (p ivec3) (compare int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-comp-swap
  :name "imageAtomicCompSwap"
  :parameters ((image generic-image-cube-array readonly) (p ivec3) (compare int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-comp-swap
  :name "imageAtomicCompSwap"
  :parameters ((image generic-image-1d readonly) (p int) (compare int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-comp-swap
  :name "imageAtomicCompSwap"
  :parameters ((image generic-image-1d-array readonly) (p ivec2) (compare int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-comp-swap
  :name "imageAtomicCompSwap"
  :parameters ((image generic-image-2d-rect readonly) (p ivec2) (compare int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-comp-swap
  :name "imageAtomicCompSwap"
  :parameters ((image generic-image-2d-ms readonly) (p ivec2) (sample int) (compare int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-comp-swap
  :name "imageAtomicCompSwap"
  :parameters ((image generic-image-2d-ms-array readonly) (p ivec3) (sample int) (compare int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-exchange
  :name "imageAtomicExchange"
  :parameters ((image generic-image-2d readonly) (p ivec2) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-exchange
  :name "imageAtomicExchange"
  :parameters ((image generic-image-3d readonly) (p ivec3) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-exchange
  :name "imageAtomicExchange"
  :parameters ((image generic-image-cube readonly) (p ivec3) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-exchange
  :name "imageAtomicExchange"
  :parameters ((image generic-image-buffer readonly) (p int) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-exchange
  :name "imageAtomicExchange"
  :parameters ((image generic-image-2d-array readonly) (p ivec3) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-exchange
  :name "imageAtomicExchange"
  :parameters ((image generic-image-cube-array readonly) (p ivec3) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-exchange
  :name "imageAtomicExchange"
  :parameters ((image generic-image-1d readonly) (p int) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-exchange
  :name "imageAtomicExchange"
  :parameters ((image generic-image-1d-array readonly) (p ivec2) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-exchange
  :name "imageAtomicExchange"
  :parameters ((image generic-image-2d-rect readonly) (p ivec2) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-exchange
  :name "imageAtomicExchange"
  :parameters ((image generic-image-2d-ms readonly) (p ivec2) (sample int) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-exchange
  :name "imageAtomicExchange"
  :parameters ((image generic-image-2d-ms-array readonly) (p ivec3) (sample int) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-exchange
  :name "imageAtomicExchange"
  :parameters ((image generic-image-2d readonly) (p ivec2) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-exchange
  :name "imageAtomicExchange"
  :parameters ((image generic-image-3d readonly) (p ivec3) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-exchange
  :name "imageAtomicExchange"
  :parameters ((image generic-image-cube readonly) (p ivec3) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-exchange
  :name "imageAtomicExchange"
  :parameters ((image generic-image-buffer readonly) (p int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-exchange
  :name "imageAtomicExchange"
  :parameters ((image generic-image-2d-array readonly) (p ivec3) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-exchange
  :name "imageAtomicExchange"
  :parameters ((image generic-image-cube-array readonly) (p ivec3) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-exchange
  :name "imageAtomicExchange"
  :parameters ((image generic-image-1d readonly) (p int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-exchange
  :name "imageAtomicExchange"
  :parameters ((image generic-image-1d-array readonly) (p ivec2) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-exchange
  :name "imageAtomicExchange"
  :parameters ((image generic-image-2d-rect readonly) (p ivec2) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-exchange
  :name "imageAtomicExchange"
  :parameters ((image generic-image-2d-ms readonly) (p ivec2) (sample int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-exchange
  :name "imageAtomicExchange"
  :parameters ((image generic-image-2d-ms-array readonly) (p ivec3) (sample int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-exchange
  :name "imageAtomicExchange"
  :parameters ((image generic-image-2d readonly) (p ivec2) (data float))
  :return float
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-exchange
  :name "imageAtomicExchange"
  :parameters ((image generic-image-3d readonly) (p ivec3) (data float))
  :return float
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-exchange
  :name "imageAtomicExchange"
  :parameters ((image generic-image-cube readonly) (p ivec3) (data float))
  :return float
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-exchange
  :name "imageAtomicExchange"
  :parameters ((image generic-image-buffer readonly) (p int) (data float))
  :return float
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-exchange
  :name "imageAtomicExchange"
  :parameters ((image generic-image-2d-array readonly) (p ivec3) (data float))
  :return float
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-exchange
  :name "imageAtomicExchange"
  :parameters ((image generic-image-cube-array readonly) (p ivec3) (data float))
  :return float
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-exchange
  :name "imageAtomicExchange"
  :parameters ((image generic-image-1d readonly) (p int) (data float))
  :return float
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-exchange
  :name "imageAtomicExchange"
  :parameters ((image generic-image-1d-array readonly) (p ivec2) (data float))
  :return float
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-exchange
  :name "imageAtomicExchange"
  :parameters ((image generic-image-2d-rect readonly) (p ivec2) (data float))
  :return float
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-exchange
  :name "imageAtomicExchange"
  :parameters ((image generic-image-2d-ms readonly) (p ivec2) (sample int) (data float))
  :return float
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-exchange
  :name "imageAtomicExchange"
  :parameters ((image generic-image-2d-ms-array readonly) (p ivec3) (sample int) (data float))
  :return float
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-max
  :name "imageAtomicMax"
  :parameters ((image generic-image-2d readonly) (p ivec2) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-max
  :name "imageAtomicMax"
  :parameters ((image generic-image-3d readonly) (p ivec3) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-max
  :name "imageAtomicMax"
  :parameters ((image generic-image-cube readonly) (p ivec3) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-max
  :name "imageAtomicMax"
  :parameters ((image generic-image-buffer readonly) (p int) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-max
  :name "imageAtomicMax"
  :parameters ((image generic-image-2d-array readonly) (p ivec3) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-max
  :name "imageAtomicMax"
  :parameters ((image generic-image-cube-array readonly) (p ivec3) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-max
  :name "imageAtomicMax"
  :parameters ((image generic-image-1d readonly) (p int) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-max
  :name "imageAtomicMax"
  :parameters ((image generic-image-1d-array readonly) (p ivec2) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-max
  :name "imageAtomicMax"
  :parameters ((image generic-image-2d-rect readonly) (p ivec2) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-max
  :name "imageAtomicMax"
  :parameters ((image generic-image-2d-ms readonly) (p ivec2) (sample int) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-max
  :name "imageAtomicMax"
  :parameters ((image generic-image-2d-ms-array readonly) (p ivec3) (sample int) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-max
  :name "imageAtomicMax"
  :parameters ((image generic-image-2d readonly) (p ivec2) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-max
  :name "imageAtomicMax"
  :parameters ((image generic-image-3d readonly) (p ivec3) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-max
  :name "imageAtomicMax"
  :parameters ((image generic-image-cube readonly) (p ivec3) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-max
  :name "imageAtomicMax"
  :parameters ((image generic-image-buffer readonly) (p int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-max
  :name "imageAtomicMax"
  :parameters ((image generic-image-2d-array readonly) (p ivec3) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-max
  :name "imageAtomicMax"
  :parameters ((image generic-image-cube-array readonly) (p ivec3) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-max
  :name "imageAtomicMax"
  :parameters ((image generic-image-1d readonly) (p int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-max
  :name "imageAtomicMax"
  :parameters ((image generic-image-1d-array readonly) (p ivec2) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-max
  :name "imageAtomicMax"
  :parameters ((image generic-image-2d-rect readonly) (p ivec2) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-max
  :name "imageAtomicMax"
  :parameters ((image generic-image-2d-ms readonly) (p ivec2) (sample int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-max
  :name "imageAtomicMax"
  :parameters ((image generic-image-2d-ms-array readonly) (p ivec3) (sample int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-min
  :name "imageAtomicMin"
  :parameters ((image generic-image-2d readonly) (p ivec2) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-min
  :name "imageAtomicMin"
  :parameters ((image generic-image-3d readonly) (p ivec3) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-min
  :name "imageAtomicMin"
  :parameters ((image generic-image-cube readonly) (p ivec3) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-min
  :name "imageAtomicMin"
  :parameters ((image generic-image-buffer readonly) (p int) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-min
  :name "imageAtomicMin"
  :parameters ((image generic-image-2d-array readonly) (p ivec3) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-min
  :name "imageAtomicMin"
  :parameters ((image generic-image-cube-array readonly) (p ivec3) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-min
  :name "imageAtomicMin"
  :parameters ((image generic-image-1d readonly) (p int) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-min
  :name "imageAtomicMin"
  :parameters ((image generic-image-1d-array readonly) (p ivec2) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-min
  :name "imageAtomicMin"
  :parameters ((image generic-image-2d-rect readonly) (p ivec2) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-min
  :name "imageAtomicMin"
  :parameters ((image generic-image-2d-ms readonly) (p ivec2) (sample int) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-min
  :name "imageAtomicMin"
  :parameters ((image generic-image-2d-ms-array readonly) (p ivec3) (sample int) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-min
  :name "imageAtomicMin"
  :parameters ((image generic-image-2d readonly) (p ivec2) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-min
  :name "imageAtomicMin"
  :parameters ((image generic-image-3d readonly) (p ivec3) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-min
  :name "imageAtomicMin"
  :parameters ((image generic-image-cube readonly) (p ivec3) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-min
  :name "imageAtomicMin"
  :parameters ((image generic-image-buffer readonly) (p int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-min
  :name "imageAtomicMin"
  :parameters ((image generic-image-2d-array readonly) (p ivec3) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-min
  :name "imageAtomicMin"
  :parameters ((image generic-image-cube-array readonly) (p ivec3) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-min
  :name "imageAtomicMin"
  :parameters ((image generic-image-1d readonly) (p int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-min
  :name "imageAtomicMin"
  :parameters ((image generic-image-1d-array readonly) (p ivec2) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-min
  :name "imageAtomicMin"
  :parameters ((image generic-image-2d-rect readonly) (p ivec2) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-min
  :name "imageAtomicMin"
  :parameters ((image generic-image-2d-ms readonly) (p ivec2) (sample int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-min
  :name "imageAtomicMin"
  :parameters ((image generic-image-2d-ms-array readonly) (p ivec3) (sample int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-or
  :name "imageAtomicOr"
  :parameters ((image generic-image-2d readonly) (p ivec2) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-or
  :name "imageAtomicOr"
  :parameters ((image generic-image-3d readonly) (p ivec3) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-or
  :name "imageAtomicOr"
  :parameters ((image generic-image-cube readonly) (p ivec3) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-or
  :name "imageAtomicOr"
  :parameters ((image generic-image-buffer readonly) (p int) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-or
  :name "imageAtomicOr"
  :parameters ((image generic-image-2d-array readonly) (p ivec3) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-or
  :name "imageAtomicOr"
  :parameters ((image generic-image-cube-array readonly) (p ivec3) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-or
  :name "imageAtomicOr"
  :parameters ((image generic-image-1d readonly) (p int) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-or
  :name "imageAtomicOr"
  :parameters ((image generic-image-1d-array readonly) (p ivec2) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-or
  :name "imageAtomicOr"
  :parameters ((image generic-image-2d-rect readonly) (p ivec2) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-or
  :name "imageAtomicOr"
  :parameters ((image generic-image-2d-ms readonly) (p ivec2) (sample int) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-or
  :name "imageAtomicOr"
  :parameters ((image generic-image-2d-ms-array readonly) (p ivec3) (sample int) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-or
  :name "imageAtomicOr"
  :parameters ((image generic-image-2d readonly) (p ivec2) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-or
  :name "imageAtomicOr"
  :parameters ((image generic-image-3d readonly) (p ivec3) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-or
  :name "imageAtomicOr"
  :parameters ((image generic-image-cube readonly) (p ivec3) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-or
  :name "imageAtomicOr"
  :parameters ((image generic-image-buffer readonly) (p int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-or
  :name "imageAtomicOr"
  :parameters ((image generic-image-2d-array readonly) (p ivec3) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-or
  :name "imageAtomicOr"
  :parameters ((image generic-image-cube-array readonly) (p ivec3) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-or
  :name "imageAtomicOr"
  :parameters ((image generic-image-1d readonly) (p int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-or
  :name "imageAtomicOr"
  :parameters ((image generic-image-1d-array readonly) (p ivec2) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-or
  :name "imageAtomicOr"
  :parameters ((image generic-image-2d-rect readonly) (p ivec2) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-or
  :name "imageAtomicOr"
  :parameters ((image generic-image-2d-ms readonly) (p ivec2) (sample int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-or
  :name "imageAtomicOr"
  :parameters ((image generic-image-2d-ms-array readonly) (p ivec3) (sample int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-xor
  :name "imageAtomicXor"
  :parameters ((image generic-image-2d readonly) (p ivec2) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-xor
  :name "imageAtomicXor"
  :parameters ((image generic-image-3d readonly) (p ivec3) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-xor
  :name "imageAtomicXor"
  :parameters ((image generic-image-cube readonly) (p ivec3) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-xor
  :name "imageAtomicXor"
  :parameters ((image generic-image-buffer readonly) (p int) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-xor
  :name "imageAtomicXor"
  :parameters ((image generic-image-2d-array readonly) (p ivec3) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-xor
  :name "imageAtomicXor"
  :parameters ((image generic-image-cube-array readonly) (p ivec3) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-xor
  :name "imageAtomicXor"
  :parameters ((image generic-image-1d readonly) (p int) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-xor
  :name "imageAtomicXor"
  :parameters ((image generic-image-1d-array readonly) (p ivec2) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-xor
  :name "imageAtomicXor"
  :parameters ((image generic-image-2d-rect readonly) (p ivec2) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-xor
  :name "imageAtomicXor"
  :parameters ((image generic-image-2d-ms readonly) (p ivec2) (sample int) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-xor
  :name "imageAtomicXor"
  :parameters ((image generic-image-2d-ms-array readonly) (p ivec3) (sample int) (data uint))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-xor
  :name "imageAtomicXor"
  :parameters ((image generic-image-2d readonly) (p ivec2) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-xor
  :name "imageAtomicXor"
  :parameters ((image generic-image-3d readonly) (p ivec3) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-xor
  :name "imageAtomicXor"
  :parameters ((image generic-image-cube readonly) (p ivec3) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-xor
  :name "imageAtomicXor"
  :parameters ((image generic-image-buffer readonly) (p int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-xor
  :name "imageAtomicXor"
  :parameters ((image generic-image-2d-array readonly) (p ivec3) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-xor
  :name "imageAtomicXor"
  :parameters ((image generic-image-cube-array readonly) (p ivec3) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-xor
  :name "imageAtomicXor"
  :parameters ((image generic-image-1d readonly) (p int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-xor
  :name "imageAtomicXor"
  :parameters ((image generic-image-1d-array readonly) (p ivec2) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-xor
  :name "imageAtomicXor"
  :parameters ((image generic-image-2d-rect readonly) (p ivec2) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-xor
  :name "imageAtomicXor"
  :parameters ((image generic-image-2d-ms readonly) (p ivec2) (sample int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-atomic-xor
  :name "imageAtomicXor"
  :parameters ((image generic-image-2d-ms-array readonly) (p ivec3) (sample int) (data int))
  :return int
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-load
  :name "imageLoad"
  :parameters ((image generic-image-2d readonly) (p ivec2))
  :return generic-vec4
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-load
  :name "imageLoad"
  :parameters ((image generic-image-3d readonly) (p ivec3))
  :return generic-vec4
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-load
  :name "imageLoad"
  :parameters ((image generic-image-cube readonly) (p ivec3))
  :return generic-vec4
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-load
  :name "imageLoad"
  :parameters ((image generic-image-buffer readonly) (p int))
  :return generic-vec4
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-load
  :name "imageLoad"
  :parameters ((image generic-image-2d-array readonly) (p ivec3))
  :return generic-vec4
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-load
  :name "imageLoad"
  :parameters ((image generic-image-cube-array readonly) (p ivec3))
  :return generic-vec4
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-load
  :name "imageLoad"
  :parameters ((image generic-image-1d readonly) (p int))
  :return generic-vec4
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-load
  :name "imageLoad"
  :parameters ((image generic-image-1d-array readonly) (p ivec2))
  :return generic-vec4
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-load
  :name "imageLoad"
  :parameters ((image generic-image-2d-rect readonly) (p ivec2))
  :return generic-vec4
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-load
  :name "imageLoad"
  :parameters ((image generic-image-2d-ms readonly) (p ivec2) (sample int))
  :return generic-vec4
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-load
  :name "imageLoad"
  :parameters ((image generic-image-2d-ms-array readonly) (p ivec3) (sample int))
  :return generic-vec4
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-samples
  :name "imageSamples"
  :parameters ((image generic-image-2d-ms (any readonly writeonly)))
  :return int
  :stages all
  :versions (:440 :450 :460)
  :vulkan t)
 (image-samples
  :name "imageSamples"
  :parameters ((image generic-image-2d-ms-array (any readonly writeonly)))
  :return int
  :stages all
  :versions (:440 :450 :460)
  :vulkan t)
 (image-size
  :name "imageSize"
  :parameters ((image generic-image-1d (any readonly writeonly)))
  :return int
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (image-size
  :name "imageSize"
  :parameters ((image generic-image-2d (any readonly writeonly)))
  :return ivec2
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (image-size
  :name "imageSize"
  :parameters ((image generic-image-3d (any readonly writeonly)))
  :return ivec3
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (image-size
  :name "imageSize"
  :parameters ((image generic-image-cube (any readonly writeonly)))
  :return ivec2
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (image-size
  :name "imageSize"
  :parameters ((image generic-image-cube-array (any readonly writeonly)))
  :return ivec3
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (image-size
  :name "imageSize"
  :parameters ((image generic-image-2d-array (any readonly writeonly)))
  :return ivec3
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (image-size
  :name "imageSize"
  :parameters ((image generic-image-2d-rect (any readonly writeonly)))
  :return ivec2
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (image-size
  :name "imageSize"
  :parameters ((image generic-image-1d-array (any readonly writeonly)))
  :return ivec2
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (image-size
  :name "imageSize"
  :parameters ((image generic-image-2d-ms (any readonly writeonly)))
  :return ivec2
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (image-size
  :name "imageSize"
  :parameters ((image generic-image-2d-ms-array (any readonly writeonly)))
  :return ivec3
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (image-size
  :name "imageSize"
  :parameters ((image generic-image-buffer (any readonly writeonly)))
  :return int
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (image-store
  :name "imageStore"
  :parameters ((image generic-image-2d writeonly) (p ivec2) (data generic-vec4))
  :return void
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-store
  :name "imageStore"
  :parameters ((image generic-image-3d writeonly) (p ivec3) (data generic-vec4))
  :return void
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-store
  :name "imageStore"
  :parameters ((image generic-image-cube writeonly) (p ivec3) (data generic-vec4))
  :return void
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-store
  :name "imageStore"
  :parameters ((image generic-image-buffer writeonly) (p int) (data generic-vec4))
  :return void
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-store
  :name "imageStore"
  :parameters ((image generic-image-2d-array writeonly) (p ivec3) (data generic-vec4))
  :return void
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-store
  :name "imageStore"
  :parameters ((image generic-image-cube-array writeonly) (p ivec3) (data generic-vec4))
  :return void
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-store
  :name "imageStore"
  :parameters ((image generic-image-1d writeonly) (p int) (data generic-vec4))
  :return void
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-store
  :name "imageStore"
  :parameters ((image generic-image-1d-array writeonly) (p ivec2) (data generic-vec4))
  :return void
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-store
  :name "imageStore"
  :parameters ((image generic-image-2d-rect writeonly) (p ivec2) (data generic-vec4))
  :return void
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-store
  :name "imageStore"
  :parameters ((image generic-image-2d-ms writeonly) (p ivec2) (sample int) (data generic-vec4))
  :return void
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (image-store
  :name "imageStore"
  :parameters ((image generic-image-2d-ms-array writeonly) (p ivec3) (sample int) (data generic-vec4))
  :return void
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (imul-extended
  :name "imulExtended"
  :parameters ((x generic-int) (y generic-int) (msb generic-int out) (lsb generic-int out))
  :return void
  :stages all
  :versions (:400 :410 :420 :430 :440 :450)
  :vulkan t)
 (imul-extended
  :name "imulExtended"
  :parameters ((x generic-int highp) (y generic-int highp) (msb generic-int out highp) (lsb generic-int out highp))
  :return void
  :stages all
  :versions (:460)
  :vulkan t)
 (int-bits-to-float
  :name "intBitsToFloat"
  :parameters ((value generic-int))
  :return generic-float
  :stages all
  :versions (:330 :400 :410 :420 :430 :440 :450)
  :vulkan t)
 (int-bits-to-float
  :name "intBitsToFloat"
  :parameters ((value generic-int highp))
  :return generic-float
  :stages all
  :versions (:460)
  :vulkan t)
 (interpolate-at-centroid
  :name "interpolateAtCentroid"
  :parameters ((interpolant generic-float))
  :return generic-float
  :stages (fragment)
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (interpolate-at-offset
  :name "interpolateAtOffset"
  :parameters ((interpolant generic-float) (offset vec2))
  :return generic-float
  :stages (fragment)
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (interpolate-at-sample
  :name "interpolateAtSample"
  :parameters ((interpolant generic-float) (sample int))
  :return generic-float
  :stages (fragment)
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (inverse
  :name "inverse"
  :parameters ((m mat2))
  :return mat2
  :stages all
  :versions (:140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (inverse
  :name "inverse"
  :parameters ((m mat3))
  :return mat3
  :stages all
  :versions (:140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (inverse
  :name "inverse"
  :parameters ((m mat4))
  :return mat4
  :stages all
  :versions (:140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (inverse-sqrt
  :name "inversesqrt"
  :parameters ((x generic-float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (inverse-sqrt
  :name "inversesqrt"
  :parameters ((x generic-double))
  :return generic-double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (is-inf
  :name "isinf"
  :parameters ((x generic-float))
  :return generic-bool
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (is-inf
  :name "isinf"
  :parameters ((x generic-double))
  :return generic-bool
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (is-nan
  :name "isnan"
  :parameters ((x generic-float))
  :return generic-bool
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (is-nan
  :name "isnan"
  :parameters ((x generic-double))
  :return generic-bool
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (ldexp
  :name "ldexp"
  :parameters ((x generic-float) (exp generic-int))
  :return generic-float
  :stages all
  :versions (:400 :410 :420 :430 :440 :450)
  :vulkan t)
 (ldexp
  :name "ldexp"
  :parameters ((x generic-float highp) (exp generic-int highp))
  :return generic-float
  :stages all
  :versions (:460)
  :vulkan t)
 (ldexp
  :name "ldexp"
  :parameters ((x generic-double) (exp generic-int))
  :return generic-double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (length
  :name "length"
  :parameters ((x generic-float))
  :return float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (length
  :name "length"
  :parameters ((x generic-double))
  :return double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (less-than
  :name "lessThan"
  :parameters ((x generic-vec) (y generic-vec))
  :return generic-bvec
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (less-than
  :name "lessThan"
  :parameters ((x generic-ivec) (y generic-ivec))
  :return generic-bvec
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (less-than
  :name "lessThan"
  :parameters ((x generic-uvec) (y generic-uvec))
  :return generic-bvec
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (less-than-equal
  :name "lessThanEqual"
  :parameters ((x generic-vec) (y generic-vec))
  :return generic-bvec
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (less-than-equal
  :name "lessThanEqual"
  :parameters ((x generic-ivec) (y generic-ivec))
  :return generic-bvec
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (less-than-equal
  :name "lessThanEqual"
  :parameters ((x generic-uvec) (y generic-uvec))
  :return generic-bvec
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (log
  :name "log"
  :parameters ((x generic-float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (log2
  :name "log2"
  :parameters ((x generic-float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (matrix-comp-mult
  :name "matrixCompMult"
  :parameters ((x generic-mat) (y generic-mat))
  :return generic-mat
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (max
  :name "max"
  :parameters ((x generic-float) (y generic-float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (max
  :name "max"
  :parameters ((x generic-float) (y float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (max
  :name "max"
  :parameters ((x generic-double) (y generic-double))
  :return generic-double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (max
  :name "max"
  :parameters ((x generic-double) (y double))
  :return generic-double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (max
  :name "max"
  :parameters ((x generic-int) (y generic-int))
  :return generic-int
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (max
  :name "max"
  :parameters ((x generic-int) (y int))
  :return generic-int
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (max
  :name "max"
  :parameters ((x generic-uint) (y generic-uint))
  :return generic-uint
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (max
  :name "max"
  :parameters ((x generic-uint) (y uint))
  :return generic-uint
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (memory-barrier
  :name "memoryBarrier"
  :parameters ()
  :return void
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (memory-barrier-atomic-counter
  :name "memoryBarrierAtomicCounter"
  :parameters ()
  :return void
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (memory-barrier-buffer
  :name "memoryBarrierBuffer"
  :parameters ()
  :return void
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (memory-barrier-shared
  :name "memoryBarrierShared"
  :parameters ()
  :return void
  :stages (compute)
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (memory-barrier-image
  :name "memoryBarrierImage"
  :parameters ()
  :return void
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (min
  :name "min"
  :parameters ((x generic-float) (y generic-float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (min
  :name "min"
  :parameters ((x generic-float) (y float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (min
  :name "min"
  :parameters ((x generic-double) (y generic-double))
  :return generic-double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (min
  :name "min"
  :parameters ((x generic-double) (y double))
  :return generic-double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (min
  :name "min"
  :parameters ((x generic-int) (y generic-int))
  :return generic-int
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (min
  :name "min"
  :parameters ((x generic-int) (y int))
  :return generic-int
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (min
  :name "min"
  :parameters ((x generic-uint) (y generic-uint))
  :return generic-uint
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (min
  :name "min"
  :parameters ((x generic-uint) (y uint))
  :return generic-uint
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (mix
  :name "mix"
  :parameters ((x generic-float) (y generic-float) (a generic-float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (mix
  :name "mix"
  :parameters ((x generic-float) (y generic-float) (a float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (mix
  :name "mix"
  :parameters ((x generic-double) (y generic-double) (a generic-double))
  :return generic-double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (mix
  :name "mix"
  :parameters ((x generic-double) (y generic-double) (a double))
  :return generic-double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (mix
  :name "mix"
  :parameters ((x generic-float) (y generic-float) (a generic-bool))
  :return generic-float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (mix
  :name "mix"
  :parameters ((x generic-double) (y generic-double) (a generic-bool))
  :return generic-double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (mix
  :name "mix"
  :parameters ((x generic-int) (y generic-int) (a generic-bool))
  :return generic-int
  :stages all
  :versions (:440 :450 :460)
  :vulkan t)
 (mix
  :name "mix"
  :parameters ((x generic-uint) (y generic-uint) (a generic-bool))
  :return generic-uint
  :stages all
  :versions (:440 :450 :460)
  :vulkan t)
 (mix
  :name "mix"
  :parameters ((x generic-bool) (y generic-bool) (a generic-bool))
  :return generic-bool
  :stages all
  :versions (:440 :450 :460)
  :vulkan t)
 (mod
  :name "mod"
  :parameters ((x generic-float) (y float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (mod
  :name "mod"
  :parameters ((x generic-float) (y generic-float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (mod
  :name "mod"
  :parameters ((x generic-double) (y double))
  :return generic-double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (mod
  :name "mod"
  :parameters ((x generic-double) (y generic-double))
  :return generic-double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (modf
  :name "modf"
  :parameters ((x generic-float) (i generic-float out))
  :return generic-float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (modf
  :name "modf"
  :parameters ((x generic-double) (i generic-double out))
  :return generic-double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (noise1
  :name "noise1"
  :parameters ((x generic-float))
  :return float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (noise2
  :name "noise2"
  :parameters ((x generic-float))
  :return vec2
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (noise3
  :name "noise3"
  :parameters ((x generic-float))
  :return vec3
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (noise4
  :name "noise4"
  :parameters ((x generic-float))
  :return vec4
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (normalize
  :name "normalize"
  :parameters ((x generic-float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (normalize
  :name "normalize"
  :parameters ((x generic-double))
  :return generic-double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (not
  :name "not"
  :parameters ((x generic-bvec))
  :return generic-bvec
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (not-equal
  :name "notEqual"
  :parameters ((x generic-vec) (y generic-vec))
  :return generic-bvec
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (not-equal
  :name "notEqual"
  :parameters ((x generic-ivec) (y generic-ivec))
  :return generic-bvec
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (not-equal
  :name "notEqual"
  :parameters ((x generic-uvec) (y generic-uvec))
  :return generic-bvec
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (not-equal
  :name "notEqual"
  :parameters ((x generic-bvec) (y generic-bvec))
  :return generic-bvec
  :stages all
  :versions (:120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (outer-product
  :name "outerProduct"
  :parameters ((c vec2) (r vec2))
  :return mat2
  :stages all
  :versions (:120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (outer-product
  :name "outerProduct"
  :parameters ((c vec3) (r vec3))
  :return mat3
  :stages all
  :versions (:120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (outer-product
  :name "outerProduct"
  :parameters ((c vec4) (r vec4))
  :return mat4
  :stages all
  :versions (:120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (outer-product
  :name "outerProduct"
  :parameters ((c vec3) (r vec2))
  :return mat2x3
  :stages all
  :versions (:120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (outer-product
  :name "outerProduct"
  :parameters ((c vec2) (r vec3))
  :return mat3x2
  :stages all
  :versions (:120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (outer-product
  :name "outerProduct"
  :parameters ((c vec4) (r vec2))
  :return mat2x4
  :stages all
  :versions (:120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (outer-product
  :name "outerProduct"
  :parameters ((c vec2) (r vec4))
  :return mat4x2
  :stages all
  :versions (:120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (outer-product
  :name "outerProduct"
  :parameters ((c vec4) (r vec3))
  :return mat3x4
  :stages all
  :versions (:120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (outer-product
  :name "outerProduct"
  :parameters ((c vec3) (r vec4))
  :return mat4x3
  :stages all
  :versions (:120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (pack-double-2x32
  :name "packDouble2x32"
  :parameters ((v uvec2))
  :return double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (pack-half-2x16
  :name "packHalf2x16"
  :parameters ((v vec2))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (pack-snorm-2x16
  :name "packSnorm2x16"
  :parameters ((v vec2))
  :return uint
  :stages all
  :versions (:420 :430 :440 :450)
  :vulkan t)
 (pack-snorm-2x16
  :name "packSnorm2x16"
  :parameters ((v vec2))
  :return (uint highp)
  :stages all
  :versions (:460)
  :vulkan t)
 (pack-snorm-4x8
  :name "packSnorm4x8"
  :parameters ((v vec4))
  :return uint
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (pack-unorm-2x16
  :name "packUnorm2x16"
  :parameters ((v vec2))
  :return uint
  :stages all
  :versions (:400 :410 :420 :430 :440 :450)
  :vulkan t)
 (pack-unorm-2x16
  :name "packUnorm2x16"
  :parameters ((v vec2))
  :return (uint highp)
  :stages all
  :versions (:460)
  :vulkan t)
 (pack-unorm-4x8
  :name "packUnorm4x8"
  :parameters ((v vec4))
  :return uint
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (pow
  :name "pow"
  :parameters ((x generic-float) (y generic-float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (radians
  :name "radians"
  :parameters ((degrees generic-float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (reflect
  :name "reflect"
  :parameters ((i generic-float) (n generic-float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (reflect
  :name "reflect"
  :parameters ((i generic-double) (n generic-double))
  :return generic-double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (refract
  :name "refract"
  :parameters ((i generic-float) (n generic-float) (eta float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (refract
  :name "refract"
  :parameters ((i generic-double) (n generic-double) (eta float))
  :return generic-double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450)
  :vulkan t)
 (refract
  :name "refract"
  :parameters ((i generic-double) (n generic-double) (eta double))
  :return generic-double
  :stages all
  :versions (:460)
  :vulkan t)
 (round
  :name "round"
  :parameters ((x generic-float))
  :return generic-float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (round
  :name "round"
  :parameters ((x generic-double))
  :return generic-double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (round-even
  :name "roundEven"
  :parameters ((x generic-float))
  :return generic-float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (round-even
  :name "roundEven"
  :parameters ((x generic-double))
  :return generic-double
  :stages all
  :versions ((:400 :410 :420 :430 :440 :450 :460) ())
  :vulkan t)
 (shadow-1d
  :name "shadow1D"
  :parameters ((sampler sampler-1d-shadow) (coord vec3))
  :return vec4
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (shadow-1d
  :name "shadow1D"
  :parameters ((sampler sampler-1d-shadow) (coord vec3) (bias float))
  :return vec4
  :stages (fragment)
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (shadow-1d-lod
  :name "shadow1DLod"
  :parameters ((sampler sampler-1d-shadow) (coord vec3) (lod float))
  :return vec4
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (shadow-1d-proj
  :name "shadow1DProj"
  :parameters ((sampler sampler-1d-shadow) (coord vec4))
  :return vec4
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (shadow-1d-proj
  :name "shadow1DProj"
  :parameters ((sampler sampler-1d-shadow) (coord vec4) (bias float))
  :return vec4
  :stages (fragment)
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (shadow-1d-proj-lod
  :name "shadow1DProjLod"
  :parameters ((sampler sampler-1d-shadow) (coord vec4) (lod float))
  :return vec4
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (shadow-2d
  :name "shadow2D"
  :parameters ((sampler sampler-2d-shadow) (coord vec3))
  :return vec4
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (shadow-2d
  :name "shadow2D"
  :parameters ((sampler sampler-2d-shadow) (coord vec3) (bias float))
  :return vec4
  :stages (fragment)
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (shadow-2d-lod
  :name "shadow2DLod"
  :parameters ((sampler sampler-2d-shadow) (coord vec3) (lod float))
  :return vec4
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (shadow-2d-proj
  :name "shadow2DProj"
  :parameters ((sampler sampler-2d-shadow) (coord vec4))
  :return vec4
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (shadow-2d-proj
  :name "shadow2DProj"
  :parameters ((sampler sampler-2d-shadow) (coord vec4) (bias float))
  :return vec4
  :stages (fragment)
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (shadow-2d-proj-lod
  :name "shadow2DProjLod"
  :parameters ((sampler sampler-2d-shadow) (coord vec4) (lod float))
  :return vec4
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (sign
  :name "sign"
  :parameters ((x generic-float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (sign
  :name "sign"
  :parameters ((x generic-int))
  :return generic-int
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (sign
  :name "sign"
  :parameters ((x generic-double))
  :return generic-double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (sin
  :name "sin"
  :parameters ((angle generic-float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (sinh
  :name "sinh"
  :parameters ((x generic-float))
  :return generic-float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (smooth-step
  :name "smoothstep"
  :parameters ((edge0 generic-float) (edge1 generic-float) (x generic-float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (smooth-step
  :name "smoothstep"
  :parameters ((edge0 float) (edge1 float) (x generic-float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (smooth-step
  :name "smoothstep"
  :parameters ((edge0 generic-double) (edge1 generic-double) (x generic-double))
  :return generic-double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (smooth-step
  :name "smoothstep"
  :parameters ((edge0 double) (edge1 double) (x generic-double))
  :return generic-double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (sqrt
  :name "sqrt"
  :parameters ((x generic-float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (sqrt
  :name "sqrt"
  :parameters ((x generic-double))
  :return generic-double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (step
  :name "step"
  :parameters ((edge generic-float) (x generic-float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (step
  :name "step"
  :parameters ((edge float) (x generic-float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (step
  :name "step"
  :parameters ((edge generic-double) (x generic-double))
  :return generic-double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (step
  :name "step"
  :parameters ((edge double) (x generic-double))
  :return generic-double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (subpass-load
  :name "subpassLoad"
  :parameters ((subpass generic-subpass-input))
  :return generic-vec4
  :stages (fragment)
  :versions (:460)
  :vulkan only)
 (subpass-load
  :name "subpassLoad"
  :parameters ((subpass generic-subpass-input-ms) (sample int))
  :return generic-vec4
  :stages (fragment)
  :versions (:460)
  :vulkan only)
 (tan
  :name "tan"
  :parameters ((angle generic-float))
  :return generic-float
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (tanh
  :name "tanh"
  :parameters ((x generic-float))
  :return generic-float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texel-fetch
  :name "texelFetch"
  :parameters ((sampler generic-sampler-1d) (p int) (lod int))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texel-fetch
  :name "texelFetch"
  :parameters ((sampler generic-sampler-2d) (p ivec2) (lod int))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texel-fetch
  :name "texelFetch"
  :parameters ((sampler generic-sampler-3d) (p ivec3) (lod int))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texel-fetch
  :name "texelFetch"
  :parameters ((sampler generic-sampler-2d-rect) (p ivec2))
  :return generic-vec4
  :stages all
  :versions (:140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texel-fetch
  :name "texelFetch"
  :parameters ((sampler generic-sampler-1d-array) (p ivec2) (lod int))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texel-fetch
  :name "texelFetch"
  :parameters ((sampler generic-sampler-2d-array) (p ivec3) (lod int))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texel-fetch
  :name "texelFetch"
  :parameters ((sampler generic-sampler-buffer) (p int))
  :return generic-vec4
  :stages all
  :versions (:140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texel-fetch
  :name "texelFetch"
  :parameters ((sampler generic-sampler-2d-ms) (p ivec2) (sample int))
  :return generic-vec4
  :stages all
  :versions (:150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texel-fetch
  :name "texelFetch"
  :parameters ((sampler generic-sampler-2d-ms-array) (p ivec3) (sample int))
  :return generic-vec4
  :stages all
  :versions (:150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texel-fetch-offset
  :name "texelFetchOffset"
  :parameters ((sampler generic-sampler-1d) (p int) (lod int) (offset int))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texel-fetch-offset
  :name "texelFetchOffset"
  :parameters ((sampler generic-sampler-2d) (p ivec2) (lod int) (offset ivec2))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texel-fetch-offset
  :name "texelFetchOffset"
  :parameters ((sampler generic-sampler-3d) (p ivec3) (lod int) (offset ivec3))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texel-fetch-offset
  :name "texelFetchOffset"
  :parameters ((sampler generic-sampler-2d-rect) (p ivec2) (offset ivec2))
  :return generic-vec4
  :stages all
  :versions (:140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texel-fetch-offset
  :name "texelFetchOffset"
  :parameters ((sampler generic-sampler-1d-array) (p ivec2) (lod int) (offset int))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texel-fetch-offset
  :name "texelFetchOffset"
  :parameters ((sampler generic-sampler-2d-array) (p ivec3) (lod int) (offset ivec2))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture
  :name "texture"
  :parameters ((sampler generic-sampler-1d) (p float))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture
  :name "texture"
  :parameters ((sampler generic-sampler-1d) (p float) (bias float))
  :return generic-vec4
  :stages (fragment)
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture
  :name "texture"
  :parameters ((sampler generic-sampler-2d) (p vec2))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture
  :name "texture"
  :parameters ((sampler generic-sampler-2d) (p vec2) (bias float))
  :return generic-vec4
  :stages (fragment)
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture
  :name "texture"
  :parameters ((sampler generic-sampler-3d) (p vec3))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture
  :name "texture"
  :parameters ((sampler generic-sampler-3d) (p vec3) (bias float))
  :return generic-vec4
  :stages (fragment)
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture
  :name "texture"
  :parameters ((sampler generic-sampler-cube) (p vec3))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture
  :name "texture"
  :parameters ((sampler generic-sampler-cube) (p vec3) (bias float))
  :return generic-vec4
  :stages (fragment)
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture
  :name "texture"
  :parameters ((sampler sampler-1d-shadow) (p vec3))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture
  :name "texture"
  :parameters ((sampler generic-1d-shadow) (p vec3) (bias float))
  :return float
  :stages (fragment)
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture
  :name "texture"
  :parameters ((sampler sampler-2d-shadow) (p vec3))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture
  :name "texture"
  :parameters ((sampler generic-2d-shadow) (p vec3) (bias float))
  :return float
  :stages (fragment)
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture
  :name "texture"
  :parameters ((sampler sampler-cube-shadow) (p vec4))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture
  :name "texture"
  :parameters ((sampler generic-cube-shadow) (p vec4) (bias float))
  :return float
  :stages (fragment)
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture
  :name "texture"
  :parameters ((sampler generic-sampler-2d-array) (p vec3))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture
  :name "texture"
  :parameters ((sampler generic-sampler-2d-array) (p vec3) (bias float))
  :return generic-vec4
  :stages (fragment)
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture
  :name "texture"
  :parameters ((sampler generic-sampler-cube-array) (p vec4))
  :return generic-vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture
  :name "texture"
  :parameters ((sampler generic-sampler-cube-array) (p vec4) (bias float))
  :return generic-vec4
  :stages (fragment)
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture
  :name "texture"
  :parameters ((sampler generic-sampler-1d-array) (p vec2))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture
  :name "texture"
  :parameters ((sampler generic-sampler-1d-array) (p vec2) (bias float))
  :return generic-vec4
  :stages (fragment)
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture
  :name "texture"
  :parameters ((sampler sampler-1d-array-shadow) (p vec3))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture
  :name "texture"
  :parameters ((sampler sampler-1d-array-shadow) (p vec3) (bias float))
  :return float
  :stages (fragment)
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture
  :name "texture"
  :parameters ((sampler sampler-2d-array-shadow) (p vec4))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture
  :name "texture"
  :parameters ((sampler generic-sampler-2d-rect) (p vec2))
  :return generic-vec4
  :stages all
  :versions (:140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture
  :name "texture"
  :parameters ((sampler sampler-2d-rect-shadow) (p vec3))
  :return float
  :stages all
  :versions (:140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture
  :name "texture"
  :parameters ((sampler sampler-cube-array-shadow) (p vec4) (compare float))
  :return float
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-1d
  :name "texture1D"
  :parameters ((sampler sampler-1d) (coord float))
  :return vec4
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (texture-1d
  :name "texture1D"
  :parameters ((sampler sampler-1d) (coord float) (bias float))
  :return vec4
  :stages (fragment)
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (texture-1d-lod
  :name "texture1DLod"
  :parameters ((sampler sampler-1d) (coord float) (lod float))
  :return vec4
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (texture-1d-proj
  :name "texture1DProj"
  :parameters ((sampler sampler-1d) (coord vec2))
  :return vec4
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (texture-1d-proj
  :name "texture1DProj"
  :parameters ((sampler sampler-1d) (coord vec2) (bias float))
  :return vec4
  :stages (fragment)
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (texture-1d-proj
  :name "texture1DProj"
  :parameters ((sampler sampler-1d) (coord vec4))
  :return vec4
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (texture-1d-proj
  :name "texture1DProj"
  :parameters ((sampler sampler-1d) (coord vec4) (bias float))
  :return vec4
  :stages (fragment)
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (texture-1d-proj-lod
  :name "texture1DProjLod"
  :parameters ((sampler sampler-1d) (coord vec2) (lod float))
  :return vec4
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (texture-1d-proj-lod
  :name "texture1DProjLod"
  :parameters ((sampler sampler-1d) (coord vec4) (lod float))
  :return vec4
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (texture-2d
  :name "texture2D"
  :parameters ((sampler sampler-2d) (coord vec2))
  :return vec4
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (texture-2d
  :name "texture2D"
  :parameters ((sampler sampler-2d) (coord vec2) (bias float))
  :return vec4
  :stages (fragment)
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (texture-2d-lod
  :name "texture2DLod"
  :parameters ((sampler sampler-2d) (coord vec2) (lod float))
  :return vec4
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (texture-2d-proj
  :name "texture2DProj"
  :parameters ((sampler sampler-2d) (coord vec3))
  :return vec4
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (texture-2d-proj
  :name "texture2DProj"
  :parameters ((sampler sampler-2d) (coord vec3) (bias float))
  :return vec4
  :stages (fragment)
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (texture-2d-proj
  :name "texture2DProj"
  :parameters ((sampler sampler-2d) (coord vec4))
  :return vec4
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (texture-2d-proj
  :name "texture2DProj"
  :parameters ((sampler sampler-2d) (coord vec4) (bias float))
  :return vec4
  :stages (fragment)
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (texture-2d-proj-lod
  :name "texture2DProjLod"
  :parameters ((sampler sampler-2d) (coord vec3) (lod float))
  :return vec4
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (texture-2d-proj-lod
  :name "texture2DProjLod"
  :parameters ((sampler sampler-2d) (coord vec4) (lod float))
  :return vec4
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (texture-3d
  :name "texture3D"
  :parameters ((sampler sampler-3d) (coord vec3))
  :return vec4
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (texture-3d
  :name "texture3D"
  :parameters ((sampler sampler-3d) (coord vec3) (bias float))
  :return vec4
  :stages (fragment)
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (texture-3d-lod
  :name "texture3DLod"
  :parameters ((sampler sampler-3d) (coord vec3) (lod float))
  :return vec4
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (texture-3d-proj
  :name "texture3DProj"
  :parameters ((sampler sampler-3d) (coord vec4))
  :return vec4
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (texture-3d-proj
  :name "texture3DProj"
  :parameters ((sampler sampler-3d) (coord vec4) (bias float))
  :return vec4
  :stages (fragment)
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (texture-3d-proj-lod
  :name "texture3DProjLod"
  :parameters ((sampler sampler-3d) (coord vec4) (lod float))
  :return vec4
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (texture-cube
  :name "textureCube"
  :parameters ((sampler sampler-cube) (coord vec3) (lod float))
  :return vec4
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (texture-cube-lod
  :name "textureCubeLod"
  :parameters ((sampler sampler-cube) (coord vec3) (lod float))
  :return vec4
  :stages all
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (texture-gather
  :name "textureGather"
  :parameters ((sampler generic-sampler-2d) (p vec2))
  :return generic-vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-gather
  :name "textureGather"
  :parameters ((sampler generic-sampler-2d) (p vec2) (comp int))
  :return generic-vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-gather
  :name "textureGather"
  :parameters ((sampler generic-sampler-2d-array) (p vec3))
  :return generic-vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-gather
  :name "textureGather"
  :parameters ((sampler generic-sampler-2d-array) (p vec3) (comp int))
  :return generic-vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-gather
  :name "textureGather"
  :parameters ((sampler generic-sampler-cube) (p vec3))
  :return generic-vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-gather
  :name "textureGather"
  :parameters ((sampler generic-sampler-cube) (p vec3) (comp int))
  :return generic-vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-gather
  :name "textureGather"
  :parameters ((sampler generic-sampler-cube-array) (p vec4))
  :return generic-vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-gather
  :name "textureGather"
  :parameters ((sampler generic-sampler-cube-array) (p vec4) (comp int))
  :return generic-vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-gather
  :name "textureGather"
  :parameters ((sampler generic-sampler-2d-rect) (p vec2))
  :return generic-vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-gather
  :name "textureGather"
  :parameters ((sampler generic-sampler-2d-rect) (p vec2) (comp int))
  :return generic-vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-gather
  :name "textureGather"
  :parameters ((sampler sampler-2d-shadow) (p vec2) (ref-z float))
  :return vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-gather
  :name "textureGather"
  :parameters ((sampler sampler-2d-array-shadow) (p vec3) (ref-z float))
  :return vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-gather
  :name "textureGather"
  :parameters ((sampler sampler-cube-shadow) (p vec3) (ref-z float))
  :return vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-gather
  :name "textureGather"
  :parameters ((sampler sampler-cube-array-shadow) (p vec4) (ref-z float))
  :return vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-gather
  :name "textureGather"
  :parameters ((sampler sampler-2d-rect-shadow) (p vec2) (ref-z float))
  :return vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-gather-offset
  :name "textureGatherOffset"
  :parameters ((sampler generic-sampler-2d) (p vec2) (offset ivec2))
  :return generic-vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-gather-offset
  :name "textureGatherOffset"
  :parameters ((sampler generic-sampler-2d) (p vec2) (offset ivec2) (comp int))
  :return generic-vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-gather-offset
  :name "textureGatherOffset"
  :parameters ((sampler generic-sampler-2d-array) (p vec3) (offset ivec2))
  :return generic-vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-gather-offset
  :name "textureGatherOffset"
  :parameters ((sampler generic-sampler-2d-array) (p vec3) (offset ivec2) (comp int))
  :return generic-vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-gather-offset
  :name "textureGatherOffset"
  :parameters ((sampler sampler-2d-shadow) (p vec2) (ref-z float) (offset ivec2))
  :return vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-gather-offset
  :name "textureGatherOffset"
  :parameters ((sampler sampler-2d-array-shadow) (p vec3) (ref-z float) (offset ivec2))
  :return vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-gather-offset
  :name "textureGatherOffset"
  :parameters ((sampler generic-sampler-2d-rect) (p vec2) (offset ivec2))
  :return generic-vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-gather-offset
  :name "textureGatherOffset"
  :parameters ((sampler generic-sampler-2d-rect) (p vec2) (offset ivec2) (comp int))
  :return generic-vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-gather-offset
  :name "textureGatherOffset"
  :parameters ((sampler sampler-2d-rect-shadow) (p vec2) (ref-z float) (offset ivec2))
  :return vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-gather-offsets
  :name "textureGatherOffsets"
  :parameters ((sampler generic-sampler-2d) (p vec2) (offsets (ivec2 4)))
  :return generic-vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-gather-offsets
  :name "textureGatherOffsets"
  :parameters ((sampler generic-sampler-2d) (p vec2) (offsets (ivec2 4)) (comp int))
  :return generic-vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-gather-offsets
  :name "textureGatherOffsets"
  :parameters ((sampler generic-sampler-2d-array) (p vec3) (offsets (ivec2 4)))
  :return generic-vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-gather-offsets
  :name "textureGatherOffsets"
  :parameters ((sampler generic-sampler-2d-array) (p vec3) (offsets (ivec2 4)) (comp int))
  :return generic-vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-gather-offsets
  :name "textureGatherOffsets"
  :parameters ((sampler sampler-2d-shadow) (p vec2) (ref-z float) (offsets (ivec2 4)))
  :return vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-gather-offsets
  :name "textureGatherOffsets"
  :parameters ((sampler sampler-2d-array-shadow) (p vec3) (ref-z float) (offsets (ivec2 4)))
  :return vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-gather-offsets
  :name "textureGatherOffsets"
  :parameters ((sampler generic-sampler-2d-rect) (p vec2) (offsets (ivec2 4)))
  :return generic-vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-gather-offsets
  :name "textureGatherOffsets"
  :parameters ((sampler generic-sampler-2d-rect) (p vec2) (offsets (ivec2 4)) (comp int))
  :return generic-vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-gather-offsets
  :name "textureGatherOffsets"
  :parameters ((sampler sampler-2d-rect-shadow) (p vec2) (ref-z float) (offsets (ivec2 4)))
  :return vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-grad
  :name "textureGrad"
  :parameters ((sampler generic-sampler-1d) (p float) (dpdx float) (dpdy float))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-grad
  :name "textureGrad"
  :parameters ((sampler generic-sampler-2d) (p vec2) (dpdx vec2) (dpdy vec2))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-grad
  :name "textureGrad"
  :parameters ((sampler generic-sampler-3d) (p vec3) (dpdx vec3) (dpdy vec3))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-grad
  :name "textureGrad"
  :parameters ((sampler generic-sampler-cube) (p vec3) (dpdx vec3) (dpdy vec3))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-grad
  :name "textureGrad"
  :parameters ((sampler generic-sampler-2d-rect) (p vec2) (dpdx vec2) (dpdy vec2))
  :return generic-vec4
  :stages all
  :versions (:140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-grad
  :name "textureGrad"
  :parameters ((sampler sampler-2d-rect-shadow) (p vec3) (dpdx vec2) (dpdy vec2))
  :return float
  :stages all
  :versions (:140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-grad
  :name "textureGrad"
  :parameters ((sampler sampler-1d-shadow) (p vec3) (dpdx float) (dpdy float))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-grad
  :name "textureGrad"
  :parameters ((sampler generic-sampler-1d-array) (p vec2) (dpdx float) (dpdy float))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-grad
  :name "textureGrad"
  :parameters ((sampler generic-sampler-2d-array) (p vec3) (dpdx vec2) (dpdy vec2))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-grad
  :name "textureGrad"
  :parameters ((sampler sampler-1d-array-shadow) (p vec3) (dpdx float) (dpdy float))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-grad
  :name "textureGrad"
  :parameters ((sampler sampler-2d-shadow) (p vec3) (dpdx vec2) (dpdy vec2))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-grad
  :name "textureGrad"
  :parameters ((sampler sampler-cube-shadow) (p vec4) (dpdx vec3) (dpdy vec3))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-grad
  :name "textureGrad"
  :parameters ((sampler sampler-2d-array-shadow) (p vec4) (dpdx vec2) (dpdy vec2))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-grad
  :name "textureGrad"
  :parameters ((sampler generic-sampler-cube-array) (p vec4) (dpdx vec3) (dpdy vec3))
  :return generic-vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-grad-offset
  :name "textureGradOffset"
  :parameters ((sampler generic-sampler-1d) (p float) (dpdx float) (dpdy float) (offset int))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-grad-offset
  :name "textureGradOffset"
  :parameters ((sampler generic-sampler-2d) (p vec2) (dpdx vec2) (dpdy vec2) (offset ivec2))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-grad-offset
  :name "textureGradOffset"
  :parameters ((sampler generic-sampler-3d) (p vec3) (dpdx vec3) (dpdy vec3) (offset ivec3))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-grad-offset
  :name "textureGradOffset"
  :parameters ((sampler generic-sampler-2d-rect) (p vec2) (dpdx vec2) (dpdy vec2) (offset ivec2))
  :return generic-vec4
  :stages all
  :versions (:140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-grad-offset
  :name "textureGradOffset"
  :parameters ((sampler sampler-2d-rect-shadow) (p vec3) (dpdx vec2) (dpdy vec2) (offset ivec2))
  :return float
  :stages all
  :versions (:140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-grad-offset
  :name "textureGradOffset"
  :parameters ((sampler sampler-1d-shadow) (p vec3) (dpdx float) (dpdy float) (offset int))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-grad-offset
  :name "textureGradOffset"
  :parameters ((sampler sampler-2d-shadow) (p vec3) (dpdx vec2) (dpdy vec2) (offset ivec2))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-grad-offset
  :name "textureGradOffset"
  :parameters ((sampler generic-sampler-2d-array) (p vec3) (dpdx vec2) (dpdy vec2) (offset ivec2))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-grad-offset
  :name "textureGradOffset"
  :parameters ((sampler generic-sampler-1d-array) (p vec2) (dpdx float) (dpdy float) (offset int))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-grad-offset
  :name "textureGradOffset"
  :parameters ((sampler sampler-1d-array-shadow) (p vec3) (dpdx float) (dpdy float) (offset int))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-grad-offset
  :name "textureGradOffset"
  :parameters ((sampler sampler-2d-array-shadow) (p vec4) (dpdx vec2) (dpdy vec2) (offset ivec2))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-lod
  :name "textureLod"
  :parameters ((sampler generic-sampler-1d) (p float) (lod float))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-lod
  :name "textureLod"
  :parameters ((sampler generic-sampler-2d) (p vec2) (lod float))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-lod
  :name "textureLod"
  :parameters ((sampler generic-sampler-3d) (p vec3) (lod float))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-lod
  :name "textureLod"
  :parameters ((sampler generic-sampler-cube) (p vec3) (lod float))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-lod
  :name "textureLod"
  :parameters ((sampler sampler-2d-shadow) (p vec3) (lod float))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-lod
  :name "textureLod"
  :parameters ((sampler sampler-1d-shadow) (p vec3) (lod float))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-lod
  :name "textureLod"
  :parameters ((sampler generic-sampler-1d-array) (p vec2) (lod float))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-lod
  :name "textureLod"
  :parameters ((sampler sampler-1d-array-shadow) (p vec3) (lod float))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-lod
  :name "textureLod"
  :parameters ((sampler generic-sampler-2d-array) (p vec3) (lod float))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-lod
  :name "textureLod"
  :parameters ((sampler generic-sampler-cube-array) (p vec4) (lod float))
  :return generic-vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-lod-offset
  :name "textureLodOffset"
  :parameters ((sampler generic-sampler-1d) (p float) (lod float) (offset int))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-lod-offset
  :name "textureLodOffset"
  :parameters ((sampler generic-sampler-2d) (p vec2) (lod float) (offset ivec2))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-lod-offset
  :name "textureLodOffset"
  :parameters ((sampler generic-sampler-3d) (p vec3) (lod float) (offset ivec3))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-lod-offset
  :name "textureLodOffset"
  :parameters ((sampler sampler-1d-shadow) (p vec3) (lod float) (offset int))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-lod-offset
  :name "textureLodOffset"
  :parameters ((sampler sampler-2d-shadow) (p vec3) (lod float) (offset ivec2))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-lod-offset
  :name "textureLodOffset"
  :parameters ((sampler generic-sampler-1d-array) (p vec2) (lod float) (offset int))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-lod-offset
  :name "textureLodOffset"
  :parameters ((sampler generic-sampler-2d-array) (p vec3) (lod float) (offset ivec2))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-lod-offset
  :name "textureLodOffset"
  :parameters ((sampler sampler-1d-array-shadow) (p vec3) (lod float) (offset int))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-offset
  :name "textureOffset"
  :parameters ((sampler generic-sampler-1d) (p float) (offset int))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-offset
  :name "textureOffset"
  :parameters ((sampler generic-sampler-1d) (p float) (offset int) (bias float))
  :return generic-vec4
  :stages (fragment)
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-offset
  :name "textureOffset"
  :parameters ((sampler generic-sampler-2d) (p vec2) (offset ivec2))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-offset
  :name "textureOffset"
  :parameters ((sampler generic-sampler-2d) (p vec2) (offset ivec2) (bias float))
  :return generic-vec4
  :stages (fragment)
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-offset
  :name "textureOffset"
  :parameters ((sampler generic-sampler-3d) (p vec3) (offset ivec3))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-offset
  :name "textureOffset"
  :parameters ((sampler generic-sampler-3d) (p vec3) (offset ivec3) (bias float))
  :return generic-vec4
  :stages (fragment)
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-offset
  :name "textureOffset"
  :parameters ((sampler sampler-2d-shadow) (p vec3) (offset ivec2))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-offset
  :name "textureOffset"
  :parameters ((sampler sampler-2d-shadow) (p vec3) (offset ivec2) (bias float))
  :return float
  :stages (fragment)
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-offset
  :name "textureOffset"
  :parameters ((sampler generic-sampler-2d-rect) (p vec2) (offset ivec2))
  :return generic-vec4
  :stages all
  :versions (:140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-offset
  :name "textureOffset"
  :parameters ((sampler sampler-2d-rect-shadow) (p vec3) (offset ivec2))
  :return float
  :stages all
  :versions (:140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-offset
  :name "textureOffset"
  :parameters ((sampler sampler-1d-shadow) (p vec3) (offset int))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-offset
  :name "textureOffset"
  :parameters ((sampler sampler-1d-shadow) (p vec3) (offset int) (bias float))
  :return float
  :stages (fragment)
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-offset
  :name "textureOffset"
  :parameters ((sampler generic-sampler-1d-array) (p vec2) (offset int))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-offset
  :name "textureOffset"
  :parameters ((sampler generic-sampler-1d-array) (p vec2) (offset int) (bias float))
  :return generic-vec4
  :stages (fragment)
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-offset
  :name "textureOffset"
  :parameters ((sampler generic-sampler-2d-array) (p vec3) (offset ivec2))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-offset
  :name "textureOffset"
  :parameters ((sampler generic-sampler-2d-array) (p vec3) (offset ivec2) (bias float))
  :return generic-vec4
  :stages (fragment)
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-offset
  :name "textureOffset"
  :parameters ((sampler sampler-1d-array-shadow) (p vec3) (offset int))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-offset
  :name "textureOffset"
  :parameters ((sampler sampler-1d-array-shadow) (p vec3) (offset int) (bias float))
  :return float
  :stages (fragment)
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-offset
  :name "textureOffset"
  :parameters ((sampler sampler-2d-array-shadow) (p vec4) (offset ivec2))
  :return float
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (texture-proj
  :name "textureProj"
  :parameters ((sampler generic-sampler-1d) (p vec2))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj
  :name "textureProj"
  :parameters ((sampler generic-sampler-1d) (p vec2) (bias float))
  :return generic-vec4
  :stages (fragment)
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj
  :name "textureProj"
  :parameters ((sampler generic-sampler-1d) (p vec4))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj
  :name "textureProj"
  :parameters ((sampler generic-sampler-1d) (p vec4) (bias float))
  :return generic-vec4
  :stages (fragment)
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj
  :name "textureProj"
  :parameters ((sampler generic-sampler-2d) (p vec3))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj
  :name "textureProj"
  :parameters ((sampler generic-sampler-2d) (p vec3) (bias float))
  :return generic-vec4
  :stages (fragment)
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj
  :name "textureProj"
  :parameters ((sampler generic-sampler-2d) (p vec4))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj
  :name "textureProj"
  :parameters ((sampler generic-sampler-2d) (p vec4) (bias float))
  :return generic-vec4
  :stages (fragment)
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj
  :name "textureProj"
  :parameters ((sampler generic-sampler-3d) (p vec4))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj
  :name "textureProj"
  :parameters ((sampler generic-sampler-3d) (p vec4) (bias float))
  :return generic-vec4
  :stages (fragment)
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj
  :name "textureProj"
  :parameters ((sampler sampler-1d-shadow) (p vec4))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj
  :name "textureProj"
  :parameters ((sampler sampler-1d-shadow) (p vec4) (bias float))
  :return float
  :stages (fragment)
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj
  :name "textureProj"
  :parameters ((sampler sampler-2d-shadow) (p vec4))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj
  :name "textureProj"
  :parameters ((sampler sampler-2d-shadow) (p vec4) (bias float))
  :return float
  :stages (fragment)
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj
  :name "textureProj"
  :parameters ((sampler generic-sampler-2d-rect) (p vec3))
  :return generic-vec4
  :stages all
  :versions (:140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj
  :name "textureProj"
  :parameters ((sampler generic-sampler-2d-rect) (p vec4))
  :return generic-vec4
  :stages all
  :versions (:140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj
  :name "textureProj"
  :parameters ((sampler sampler-2d-rect-shadow) (p vec4))
  :return float
  :stages all
  :versions (:140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-grad
  :name "textureProjGrad"
  :parameters ((sampler generic-sampler-1d) (p vec2) (dpdx float) (dpdy float))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-grad
  :name "textureProjGrad"
  :parameters ((sampler generic-sampler-1d) (p vec4) (dpdx float) (dpdy float))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-grad
  :name "textureProjGrad"
  :parameters ((sampler generic-sampler-2d) (p vec3) (dpdx vec2) (dpdy vec2))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-grad
  :name "textureProjGrad"
  :parameters ((sampler generic-sampler-2d) (p vec4) (dpdx vec2) (dpdy vec2))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-grad
  :name "textureProjGrad"
  :parameters ((sampler generic-sampler-3d) (p vec4) (dpdx vec3) (dpdy vec3))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-grad
  :name "textureProjGrad"
  :parameters ((sampler generic-sampler-2d-rect) (p vec3) (dpdx vec2) (dpdy vec2))
  :return generic-vec4
  :stages all
  :versions (:140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-grad
  :name "textureProjGrad"
  :parameters ((sampler generic-sampler-2d-rect) (p vec4) (dpdx vec2) (dpdy vec2))
  :return generic-vec4
  :stages all
  :versions (:140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-grad
  :name "textureProjGrad"
  :parameters ((sampler sampler-2d-rect-shadow) (p vec4) (dpdx vec2) (dpdy vec2))
  :return float
  :stages all
  :versions (:140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-grad
  :name "textureProjGrad"
  :parameters ((sampler sampler-1d-shadow) (p vec4) (dpdx float) (dpdy float))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-grad
  :name "textureProjGrad"
  :parameters ((sampler sampler-2d-shadow) (p vec4) (dpdx vec2) (dpdy vec2))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-grad-offset
  :name "textureProjGradOffset"
  :parameters ((sampler generic-sampler-1d) (p vec2) (dpdx float) (dpdy float) (offset int))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-grad-offset
  :name "textureProjGradOffset"
  :parameters ((sampler generic-sampler-1d) (p vec4) (dpdx float) (dpdy float) (offset int))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-grad-offset
  :name "textureProjGradOffset"
  :parameters ((sampler generic-sampler-2d) (p vec3) (dpdx vec2) (dpdy vec2) (offset ivec2))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-grad-offset
  :name "textureProjGradOffset"
  :parameters ((sampler generic-sampler-2d) (p vec4) (dpdx vec2) (dpdy vec2) (offset ivec2))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-grad-offset
  :name "textureProjGradOffset"
  :parameters ((sampler generic-sampler-3d) (p vec4) (dpdx vec3) (dpdy vec3) (offset ivec3))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-grad-offset
  :name "textureProjGradOffset"
  :parameters ((sampler generic-sampler-2d-rect) (p vec3) (dpdx vec2) (dpdy vec2) (offset ivec2))
  :return generic-vec4
  :stages all
  :versions (:140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-grad-offset
  :name "textureProjGradOffset"
  :parameters ((sampler generic-sampler-2d-rect) (p vec4) (dpdx vec2) (dpdy vec2) (offset ivec2))
  :return generic-vec4
  :stages all
  :versions (:140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-grad-offset
  :name "textureProjGradOffset"
  :parameters ((sampler sampler-2d-rect-shadow) (p vec4) (dpdx vec2) (dpdy vec2) (offset ivec2))
  :return float
  :stages all
  :versions (:140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-grad-offset
  :name "textureProjGradOffset"
  :parameters ((sampler sampler-1d-shadow) (p vec4) (dpdx float) (dpdy float) (offset int))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-grad-offset
  :name "textureProjGradOffset"
  :parameters ((sampler sampler-2d-shadow) (p vec4) (dpdx vec2) (dpdy vec2) (offset ivec2))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-lod
  :name "textureProjLod"
  :parameters ((sampler generic-sampler-1d) (p vec2) (lod float))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-lod
  :name "textureProjLod"
  :parameters ((sampler generic-sampler-1d) (p vec4) (lod float))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-lod
  :name "textureProjLod"
  :parameters ((sampler generic-sampler-2d) (p vec3) (lod float))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-lod
  :name "textureProjLod"
  :parameters ((sampler generic-sampler-2d) (p vec4) (lod float))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-lod
  :name "textureProjLod"
  :parameters ((sampler generic-sampler-3d) (p vec4) (lod float))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-lod
  :name "textureProjLod"
  :parameters ((sampler sampler-1d-shadow) (p vec4) (lod float))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-lod
  :name "textureProjLod"
  :parameters ((sampler sampler-2d-shadow) (p vec4) (lod float))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-lod-offset
  :name "textureProjLodOffset"
  :parameters ((sampler generic-sampler-1d) (p vec2) (lod float) (offset int))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-lod-offset
  :name "textureProjLodOffset"
  :parameters ((sampler generic-sampler-1d) (p vec4) (lod float) (offset int))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-lod-offset
  :name "textureProjLodOffset"
  :parameters ((sampler generic-sampler-2d) (p vec3) (lod float) (offset ivec2))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-lod-offset
  :name "textureProjLodOffset"
  :parameters ((sampler generic-sampler-2d) (p vec4) (lod float) (offset ivec2))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-lod-offset
  :name "textureProjLodOffset"
  :parameters ((sampler generic-sampler-3d) (p vec4) (lod float) (offset ivec3))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-lod-offset
  :name "textureProjLodOffset"
  :parameters ((sampler sampler-1d-shadow) (p vec4) (lod float) (offset int))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-lod-offset
  :name "textureProjLodOffset"
  :parameters ((sampler sampler-2d-shadow) (p vec4) (lod float) (offset ivec2))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-offset
  :name "textureProjOffset"
  :parameters ((sampler generic-sampler-1d) (p vec2) (offset int))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-offset
  :name "textureProjOffset"
  :parameters ((sampler generic-sampler-1d) (p vec2) (offset int) (bias float))
  :return generic-vec4
  :stages (fragment)
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-offset
  :name "textureProjOffset"
  :parameters ((sampler generic-sampler-1d) (p vec4) (offset int))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-offset
  :name "textureProjOffset"
  :parameters ((sampler generic-sampler-1d) (p vec4) (offset int) (bias float))
  :return generic-vec4
  :stages (fragment)
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-offset
  :name "textureProjOffset"
  :parameters ((sampler generic-sampler-2d) (p vec3) (offset ivec2))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-offset
  :name "textureProjOffset"
  :parameters ((sampler generic-sampler-2d) (p vec3) (offset ivec2) (bias float))
  :return generic-vec4
  :stages (fragment)
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-offset
  :name "textureProjOffset"
  :parameters ((sampler generic-sampler-2d) (p vec4) (offset ivec2))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-offset
  :name "textureProjOffset"
  :parameters ((sampler generic-sampler-2d) (p vec4) (offset ivec2) (bias float))
  :return generic-vec4
  :stages (fragment)
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-offset
  :name "textureProjOffset"
  :parameters ((sampler generic-sampler-3d) (p vec4) (offset ivec3))
  :return generic-vec4
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-offset
  :name "textureProjOffset"
  :parameters ((sampler generic-sampler-3d) (p vec4) (offset ivec3) (bias float))
  :return generic-vec4
  :stages (fragment)
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-offset
  :name "textureProjOffset"
  :parameters ((sampler generic-sampler-2d-rect) (p vec3) (offset ivec2))
  :return generic-vec4
  :stages all
  :versions (:140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-offset
  :name "textureProjOffset"
  :parameters ((sampler generic-sampler-2d-rect) (p vec4) (offset ivec2))
  :return generic-vec4
  :stages all
  :versions (:140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-offset
  :name "textureProjOffset"
  :parameters ((sampler sampler-2d-rect-shadow) (p vec4) (offset ivec2))
  :return float
  :stages all
  :versions (:140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-offset
  :name "textureProjOffset"
  :parameters ((sampler sampler-1d-shadow) (p vec4) (offset int))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-offset
  :name "textureProjOffset"
  :parameters ((sampler sampler-1d-shadow) (p vec4) (offset int) (bias float))
  :return float
  :stages (fragment)
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-offset
  :name "textureProjOffset"
  :parameters ((sampler sampler-2d-shadow) (p vec4) (offset ivec2))
  :return float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-proj-offset
  :name "textureProjOffset"
  :parameters ((sampler sampler-2d-shadow) (p vec4) (offset ivec2) (bias float))
  :return float
  :stages (fragment)
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-query-levels
  :name "textureQueryLevels"
  :parameters ((sampler generic-sampler-1d))
  :return int
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (texture-query-levels
  :name "textureQueryLevels"
  :parameters ((sampler generic-sampler-2d))
  :return int
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (texture-query-levels
  :name "textureQueryLevels"
  :parameters ((sampler generic-sampler-3d))
  :return int
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (texture-query-levels
  :name "textureQueryLevels"
  :parameters ((sampler generic-sampler-cube))
  :return int
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (texture-query-levels
  :name "textureQueryLevels"
  :parameters ((sampler generic-sampler-1d-array))
  :return int
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (texture-query-levels
  :name "textureQueryLevels"
  :parameters ((sampler generic-sampler-2d-array))
  :return int
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (texture-query-levels
  :name "textureQueryLevels"
  :parameters ((sampler generic-sampler-cube-array))
  :return int
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (texture-query-levels
  :name "textureQueryLevels"
  :parameters ((sampler sampler-1d-shadow))
  :return int
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (texture-query-levels
  :name "textureQueryLevels"
  :parameters ((sampler sampler-2d-shadow))
  :return int
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (texture-query-levels
  :name "textureQueryLevels"
  :parameters ((sampler sampler-cube-shadow))
  :return int
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (texture-query-levels
  :name "textureQueryLevels"
  :parameters ((sampler sampler-1d-array-shadow))
  :return int
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (texture-query-levels
  :name "textureQueryLevels"
  :parameters ((sampler sampler-2d-array-shadow))
  :return int
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (texture-query-levels
  :name "textureQueryLevels"
  :parameters ((sampler sampler-cube-array-shadow))
  :return int
  :stages all
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (texture-query-lod
  :name "textureQueryLod"
  :parameters ((sampler generic-sampler-1d) (p float))
  :return vec2
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-query-lod
  :name "textureQueryLod"
  :parameters ((sampler generic-sampler-2d) (p vec2))
  :return vec2
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-query-lod
  :name "textureQueryLod"
  :parameters ((sampler generic-sampler-3d) (p vec3))
  :return vec2
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-query-lod
  :name "textureQueryLod"
  :parameters ((sampler generic-sampler-cube) (p vec3))
  :return vec2
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-query-lod
  :name "textureQueryLod"
  :parameters ((sampler generic-sampler-1d-array) (p float))
  :return vec2
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-query-lod
  :name "textureQueryLod"
  :parameters ((sampler generic-sampler-2d-array) (p vec2))
  :return vec2
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-query-lod
  :name "textureQueryLod"
  :parameters ((sampler generic-sampler-cube-array) (p vec3))
  :return vec2
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-query-lod
  :name "textureQueryLod"
  :parameters ((sampler sampler-1d-shadow) (p float))
  :return vec2
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-query-lod
  :name "textureQueryLod"
  :parameters ((sampler sampler-2d-shadow) (p vec2))
  :return vec2
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-query-lod
  :name "textureQueryLod"
  :parameters ((sampler sampler-cube-shadow) (p vec3))
  :return vec2
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-query-lod
  :name "textureQueryLod"
  :parameters ((sampler sampler-1d-array-shadow) (p float))
  :return vec2
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-query-lod
  :name "textureQueryLod"
  :parameters ((sampler sampler-2d-array-shadow) (p vec2))
  :return vec2
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-query-lod
  :name "textureQueryLod"
  :parameters ((sampler sampler-cube-array-shadow) (p vec3))
  :return vec2
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-samples
  :name "textureSamples"
  :parameters ((sampler generic-sampler-2d-ms))
  :return int
  :stages all
  :versions (:440 :450 :460)
  :vulkan t)
 (texture-samples
  :name "textureSamples"
  :parameters ((sampler generic-sampler-2d-ms-array))
  :return int
  :stages all
  :versions (:440 :450 :460)
  :vulkan t)
 (texture-size
  :name "textureSize"
  :parameters ((sampler generic-sampler-1d) (lod int))
  :return int
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-size
  :name "textureSize"
  :parameters ((sampler generic-sampler-2d) (lod int))
  :return ivec2
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-size
  :name "textureSize"
  :parameters ((sampler generic-sampler-3d) (lod int))
  :return ivec3
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-size
  :name "textureSize"
  :parameters ((sampler generic-sampler-cube) (lod int))
  :return ivec2
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-size
  :name "textureSize"
  :parameters ((sampler sampler-1d-shadow) (lod int))
  :return int
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-size
  :name "textureSize"
  :parameters ((sampler sampler-2d-shadow) (lod int))
  :return ivec2
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-size
  :name "textureSize"
  :parameters ((sampler sampler-cube-shadow) (lod int))
  :return ivec2
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-size
  :name "textureSize"
  :parameters ((sampler generic-sampler-cube-array) (lod int))
  :return ivec3
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-size
  :name "textureSize"
  :parameters ((sampler sampler-cube-array-shadow) (lod int))
  :return ivec3
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-size
  :name "textureSize"
  :parameters ((sampler generic-sampler-2d-rect))
  :return ivec2
  :stages all
  :versions (:140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-size
  :name "textureSize"
  :parameters ((sampler sampler-2d-rect-shadow))
  :return ivec2
  :stages all
  :versions (:140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-size
  :name "textureSize"
  :parameters ((sampler generic-sampler-1d-array) (lod int))
  :return ivec2
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-size
  :name "textureSize"
  :parameters ((sampler sampler-1d-array-shadow) (lod int))
  :return ivec2
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-size
  :name "textureSize"
  :parameters ((sampler generic-sampler-2d-array) (lod int))
  :return ivec3
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-size
  :name "textureSize"
  :parameters ((sampler sampler-2d-array-shadow) (lod int))
  :return ivec3
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-size
  :name "textureSize"
  :parameters ((sampler generic-sampler-buffer))
  :return int
  :stages all
  :versions (:140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-size
  :name "textureSize"
  :parameters ((sampler generic-sampler-2d-ms))
  :return ivec2
  :stages all
  :versions (:150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (texture-size
  :name "textureSize"
  :parameters ((sampler generic-sampler-2d-ms-array))
  :return ivec3
  :stages all
  :versions (:150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (transpose
  :name "transpose"
  :parameters ((m mat2))
  :return mat2
  :stages all
  :versions (:120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (transpose
  :name "transpose"
  :parameters ((m mat3))
  :return mat3
  :stages all
  :versions (:120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (transpose
  :name "transpose"
  :parameters ((m mat4))
  :return mat4
  :stages all
  :versions (:120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (transpose
  :name "transpose"
  :parameters ((m mat3x2))
  :return mat2x3
  :stages all
  :versions (:120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (transpose
  :name "transpose"
  :parameters ((m mat2x3))
  :return mat3x2
  :stages all
  :versions (:120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (transpose
  :name "transpose"
  :parameters ((m mat4x2))
  :return mat2x4
  :stages all
  :versions (:120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (transpose
  :name "transpose"
  :parameters ((m mat2x4))
  :return mat4x2
  :stages all
  :versions (:120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (transpose
  :name "transpose"
  :parameters ((m mat4x3))
  :return mat3x4
  :stages all
  :versions (:120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (transpose
  :name "transpose"
  :parameters ((m mat3x4))
  :return mat4x3
  :stages all
  :versions (:120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (trunc
  :name "trunc"
  :parameters ((x generic-float))
  :return generic-float
  :stages all
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (trunc
  :name "trunc"
  :parameters ((x generic-double))
  :return generic-double
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (uadd-carry
  :name "uaddCarry"
  :parameters ((x generic-uint) (y generic-uint) (carry generic-uint out))
  :return generic-uint
  :stages all
  :versions (:400 :410 :420 :430 :440 :450)
  :vulkan t)
 (uadd-carry
  :name "uaddCarry"
  :parameters ((x generic-uint highp) (y generic-uint highp) (carry generic-uint out lowp))
  :return generic-uint
  :stages all
  :versions (:460)
  :vulkan t)
 (uint-bits-to-float
  :name "uintBitsToFloat"
  :parameters ((value generic-uint))
  :return generic-float
  :stages all
  :versions (:330 :400 :410 :420 :430 :440 :450)
  :vulkan t)
 (uint-bits-to-float
  :name "uintBitsToFloat"
  :parameters ((value generic-uint highp))
  :return generic-float
  :stages all
  :versions (:460)
  :vulkan t)
 (umul-extended
  :name "umulExtended"
  :parameters ((x generic-uint) (y generic-uint) (msb generic-uint out) (lsb generic-uint out))
  :return void
  :stages all
  :versions (:400 :410 :420 :430 :440 :450)
  :vulkan t)
 (umul-extended
  :name "umulExtended"
  :parameters ((x generic-uint highp) (y generic-uint highp) (msb generic-uint out highp) (lsb generic-uint out highp))
  :return void
  :stages all
  :versions (:460)
  :vulkan t)
 (unpack-double-2x32
  :name "unpackDouble2x32"
  :parameters ((v double))
  :return uvec2
  :stages all
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (unpack-half-2x16
  :name "unpackHalf2x16"
  :parameters ((v uint))
  :return vec2
  :stages all
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (unpack-snorm-2x16
  :name "unpackSnorm2x16"
  :parameters ((p uint))
  :return vec2
  :stages all
  :versions (:420 :430 :440 :450)
  :vulkan t)
 (unpack-snorm-2x16
  :name "unpackSnorm2x16"
  :parameters ((p uint highp))
  :return vec2
  :stages all
  :versions (:460)
  :vulkan t)
 (unpack-snorm-4x8
  :name "unpackSnorm4x8"
  :parameters ((p uint))
  :return vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450)
  :vulkan t)
 (unpack-snorm-4x8
  :name "unpackSnorm4x8"
  :parameters ((p uint highp))
  :return vec4
  :stages all
  :versions (:460)
  :vulkan t)
 (unpack-unorm-2x16
  :name "unpackUnorm2x16"
  :parameters ((p uint))
  :return vec2
  :stages all
  :versions (:400 :410 :420 :430 :440 :450)
  :vulkan t)
 (unpack-unorm-2x16
  :name "unpackUnorm2x16"
  :parameters ((p uint highp))
  :return vec2
  :stages all
  :versions (:460)
  :vulkan t)
 (unpack-unorm-4x8
  :name "unpackUnorm4x8"
  :parameters ((p uint))
  :return vec4
  :stages all
  :versions (:400 :410 :420 :430 :440 :450)
  :vulkan t)
 (unpack-unorm-4x8
  :name "unpackUnorm4x8"
  :parameters ((p uint highp))
  :return vec4
  :stages all
  :versions (:460)
  :vulkan t)
 (usub-borrow
  :name "usubBorrow"
  :parameters ((x generic-uint) (y generic-uint) (borrow generic-uint out))
  :return generic-uint
  :stages all
  :versions (:400 :410 :420 :430 :440 :450)
  :vulkan t)
 (usub-borrow
  :name "usubBorrow"
  :parameters ((x generic-uint highp) (y generic-uint highp) (borrow generic-uint out lowp))
  :return generic-uint
  :stages all
  :versions (:460)
  :vulkan t))
