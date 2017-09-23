(in-package :gamebox-math)

(defdoc (point-distance function)
  "Calculate the distance between POINT1 and POINT2.
Alias: PTDIST")

(defdoc (point-translate! function)
  "Translate POINT along DIRECTION by DISTANCE, storing the result in OUT-VEC.
Alias: PTTR!")

(defdoc (point-translate function)
  "Translate POINT along DIRECTION by DISTANCE, storing the result as a new ~
vector.
Alias: PTTR")

(defdoc (point-near-p function)
  "Check if the distance between POINT1 and POINT2 is nearly the same, ~
according to the epsilon TOLERANCE.
Alias: PTNEARP")

(defdoc (interpolate-transforms! function)
  "Interpolate between the transformations MATRIX1 and MATRIX2 by COEFF, ~
storing the result in OUT-MATRIX.
Alias :MSLERP!")

(defdoc (interpolate-transforms function)
  "Interpolate between the transformations MATRIX1 and MATRIX2 by COEFF, ~
storing the result as a new matrix.
Alias: MSLERP")


(defdoc (line-direction function)
  "Calculate the direction of the line denoted by LINE-POINT1 and LINE-POINT2.")

(defdoc (line-segment-midpoint function)
  "Find the center of the line segment denoted by POINT1 and POINT2.")

(defdoc (line-plane-intersect function)
  "Find the point where a line intersects a plane.")

(defdoc (line-point-distance function)
  "Find the shortest distance from POINT to the line denoted by the points ~
LINE-POINT1 and LINE-POINT2.")
