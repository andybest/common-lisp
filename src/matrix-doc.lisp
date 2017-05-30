(in-package :gamebox-math)

(defdoc (matrix structure)
  "A 4x4 column-major matrix consisting of column vectors.
This is a typed structure, which compiles to an actual single-float array, but includes named slots for accessing ~
components.")

(defdoc (with-matrix macro)
  "A convenience macro for concisely accessing components of a matrix.
Example: (with-matrix (m matrix) 23) would allow accessing row 2/column 3 of the matrix as simply the symbol M23.")

(defdoc (with-matrices macro)
  "A convenience macro for concisely accessing components of multiple matrices.
Example: (with-matrices ((a matrix1) (b matrix2)) (values a12 b31)) would access row 1/column 2 of matrix1, and ~
row 3/column 1 of matrix2.")

(defdoc (mref function)
  "A virtualized matrix component reader. Use this instead of AREF to prevent unintended behavior should ordering of a ~
matrix ever change.")

(defdoc ((setf mref) function)
  "A virtualized matrix component writer. Use this instead of (SETF AREF) to prevent unintended behavior should ~
ordering of a matrix ever change.")

(defdoc (matrix function)
  "Create a new matrix.")

(defdoc (matrix-identity! function)
  "Modify the components of MATRIX to form an identity matrix.
Alias: MID!")

(defdoc (matrix-identity function)
  "Create an identity matrix.
Alias: MID")

(defdoc (matrix-copy! function)
  "Copy the components of MATRIX, storing the result in OUT-MATRIX.
Alias: MCP!")

(defdoc (matrix-copy function)
  "Copy the components of MATRIX, storing the result as a new matrix.
Alias: MCP")

(defdoc (matrix-clamp! function)
  "Clamp each component of MATRIX within the range of [MIN, MAX], storing the result in OUT-MATRIX.
Alias: MCLAMP!")

(defdoc (matrix-clamp function)
  "Clamp each component of MATRIX within the range of [MIN, MAX], storing the result as a new matrix.
Alias: MCLAMP")

(defdoc (matrix*! function)
  "Matrix multiplication of MATRIX1 by MATRIX2, storing the result in OUT-MATRIX.
Alias: M*!")

(defdoc (matrix* function)
  "Matrix multiplication of MATRIX1 by MATRIX2, storing the result as a new matrix.
Alias: M*")

(defdoc (matrix-translation-to-vec! function)
  "Copy the components in the translation column of MATRIX, storing the result in OUT-VEC.
Alias: MTR->V!")

(defdoc (matrix-translation-to-vec function)
  "Copy the components in the translation column of MATRIX, storing the result as a new vector.
Alias: MTR->V")

(defdoc (matrix-translation-from-vec! function)
  "Copy the components of VEC, storing the result in the translation column of MATRIX.
Alias: V->MTR!")

(defdoc (matrix-translation-from-vec function)
  "Copy the components of VEC, storing the result in the translation column of a new matrix.
Alias: V->MTR")

(defdoc (matrix-translate! function)
  "Translate a matrix by the translation vector VEC, storing the result in OUT-MATRIX.")

(defdoc (matrix-translate function)
  "Translate a matrix by the translation vector VEC, storing the result as a new matrix.")

(defdoc (matrix-copy-rotation! function)
  "Copy the 3x3 rotation components of MATRIX, storing the result in OUT-MATRIX.
Alias: MCPROT!")

(defdoc (matrix-copy-rotation function)
  "Copy the 3x3 rotation components of MATRIX, storing the result as a new matrix.
Alias: MCPROT")

(defdoc (matrix-rotation-to-vec! function)
  "Get a particular rotation AXIS from MATRIX, storing the result in OUT-VEC.
Alias: MROT->V!")

(defdoc (matrix-rotation-to-vec function)
  "Get a particular rotation AXIS from MATRIX, storing the result as a new vector.
Alias: MROT->V")

(defdoc (matrix-rotation-from-vec! function)
  "Set a particular rotation AXIS of MATRIX, storing the result in MATRIX.
Alias: V->MROT!")

(defdoc (matrix-rotation-from-vec function)
  "Set a particular rotation AXIS of MATRIX, storing the result as a new matrix.
Alias: V->MROT")

(defdoc (matrix-rotate! function)
  "Rotate a matrix in each of 3 dimensions as specified by the vector of radians VEC, storing the result in OUT-MATRIX.
Alias: MROT!")

(defdoc (matrix-rotate function)
  "Rotate a matrix in each of 3 dimensions as specified by the vector of radians VEC, storing the result as a new matrix.
Alias: MROT")

(defdoc (matrix-scale-to-vec! function)
  "Get the scale components of a matrix, storing the result in OUT-VEC.
Alias: MSCALE->V!")

(defdoc (matrix-scale-to-vec function)
  "Get the scale components of a matrix, storing the result as a new vector.
Alias: MSCALE->V")

(defdoc (matrix-scale-from-vec! function)
  "Set the scale components of a matrix, storing the result in OUT-MATRIX.
Alias: V->MSCALE!")

(defdoc (matrix-scale-from-vec function)
  "Set the scale components of a matrix, storing the result in a new matrix.
Alias: V->MSCALE")

(defdoc (matrix*vec! function)
  "Multiplication of MATRIX and VEC, storing the result in OUT-VEC.
Alias: M*V!")

(defdoc (matrix*vec function)
  "Multiplication of MATRIX and VEC, storing the result as a new vector.
Alias: M*V")

(defdoc (matrix-transpose! function)
  "Transpose MATRIX, storing the result in OUT-MATRIX.
Alias: MTRANSPOSE!")

(defdoc (matrix-transpose function)
  "Transpose MATRIX, storing the result as a new matrix.
Alias: MTRANSPOSE")

(defdoc (matrix-orthogonal-p function)
  "Check if MATRIX is orthogonal. An orthogonal matrix is a square matrix with all of its rows (or columns) being ~
perpendicular to each other, and of unit length.
Alias: MORTHOP")

(defdoc (matrix-orthogonalize! function)
  "Orthogonalize a matrix using the 'modified' Gram-Schidt method (MGS), storing the result in OUT-MATRIX.")

(defdoc (matrix-orthogonalize function)
  "Orthogonalize a matrix using the 'modified' Gram-Schidt method (MGS), storing the result as a new matrix.")

(defdoc (matrix-trace function)
  "Compute the trace of MATRIX.
Alias: MTRACE")

(defdoc (matrix-determinant function)
  "Compute the determinant of MATRIX.
Alias: MDET")

(defdoc (matrix-invert-orthogonal! function)
  "Invert MATRIX if its 3x3 rotation is an orthogonal matrix, storing the result in OUT-MATRIX.
Warning: This will only work on matrices that have an orthogonal 3x3 rotation matrix.
Alias: MINVTORTHO!")

(defdoc (matrix-invert-orthogonal function)
  "Invert MATRIX if its 3x3 rotation is an orthogonal matrix, storing the result as a new matrix.
Warning: This will only work on matrices that have an orthogonal 3x3 rotation matrix.
Alias: MINVTORTHO")

(defdoc (matrix-invert! function)
  "Invert MATRIX, storing the result in OUT-MATRIX.
Warning: This method is slower than MATRIX-INVERT-ORTHOGONAL, but not all matrices can be inverted with the fast method.
Warning: A matrix with a determinant of zero cannot be inverted, and will raise an error.
Alias: MINVT!")

(defdoc (matrix-invert function)
  "Invert MATRIX, storing the result as a new matrix.
Warning: This method is slower than MATRIX-INVERT-ORTHOGONAL, but not all matrices can be inverted with the fast method.
Warning: A matrix with a determinant of zero cannot be inverted, and will raise an error.
Alias: MINVT")

(defdoc (make-view-matrix! function)
  "Create a view matrix, storing the result in OUT-MATRIX.
Alias: MKVIEW!")

(defdoc (make-view-matrix function)
  "Create a view matrix, storing the result as a new matrix.
Alias: MKVIEW")

(defdoc (make-orthographic-matrix! function)
  "Create an orthographic projection matrix, storing the result in OUT-MATRIX.
Alias: MKORTHO!")

(defdoc (make-orthographic-matrix function)
  "Create an orthographic projection matrix, storing the result as a new matrix.
Alias: MKORTHO")

(defdoc (make-perspective-matrix! function)
  "Create a perspective projection matrix, storing the result in OUT-MATRIX.
Alias: MPERSP!")

(defdoc (make-perspective-matrix function)
  "Create a perspective projection matrix, storing the result as a new matrix.
Alias: MPERSP")
