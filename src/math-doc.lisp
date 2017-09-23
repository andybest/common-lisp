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
  )

(defdoc (interpolate-transforms function)
  )


(defdoc (line-direction function)
  )

(defdoc (line-segment-midpoint function)
  )

(defdoc (line-plane-intersect function)
  )

(defdoc (line-point-distance function)
  )
