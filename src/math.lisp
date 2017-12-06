(in-package :gamebox-math)

(defun* point-distance ((point1 vec) (point2 vec))
    (:result single-float :abbrev ptdist)
  "Calculate the distance between POINT1 and POINT2."
  (vec-magnitude (vec- point2 point1)))

(defun* point-translate! ((out-vec vec) (point vec) (direction vec)
                          (distance single-float)) (:result vec :abbrev pttr!)
  "Translate POINT along DIRECTION by DISTANCE, storing the result in OUT-VEC."
  (vec+! out-vec point (vec-scale! direction direction distance)))

(defun* point-translate ((point vec) (direction vec) (distance single-float))
    (:result vec :abbrev pttr)
  "Translate POINT along DIRECTION by DISTANCE, storing the result as a new
vector."
  (point-translate! (vzero) point direction distance))

(defun* point-near-p ((point1 vec) (point2 vec)
                      &key ((tolerance single-float) +epsilon+))
    (:result boolean :abbrev ptnearp)
  "Check if POINT1 is within TOLERANCE distance from POINT2."
  (< (abs (point-distance point1 point2)) tolerance))

(defun* interpolate-matrices! ((out-matrix matrix) (matrix1 matrix)
                               (matrix2 matrix) (coeff single-float))
    (:result matrix :abbrev mslerp!)
  "Interpolate between the transformations MATRIX1 and MATRIX2 by COEFF, storing
the result in OUT-MATRIX."
  (let ((q1 (quat))
        (q2 (quat))
        (out-quat (quat))
        (v1 (vzero))
        (v2 (vzero))
        (out-vec (vzero)))
    (quat-from-matrix! q1 matrix1)
    (quat-from-matrix! q2 matrix2)
    (quat-slerp! out-quat q1 q2 coeff)
    (quat-to-matrix! out-matrix out-quat)
    (matrix-translation-to-vec! v1 matrix1)
    (matrix-translation-to-vec! v2 matrix2)
    (vec-lerp! out-vec v1 v2 coeff)
    (matrix-translation-from-vec! out-matrix out-vec)
    out-matrix))

(defun* interpolate-matrices ((matrix1 matrix) (matrix2 matrix)
                              (coeff single-float))
    (:result matrix :abbrev mslerp)
  "Interpolate between the transformations MATRIX1 and MATRIX2 by COEFF, storing
the result as a new matrix."
  (interpolate-matrices! (zero-matrix) matrix1 matrix2 coeff))

(defun* line-direction ((line-point1 vec) (line-point2 vec)) (:result vec)
  "Calculate the direction of the line denoted by LINE-POINT1 and LINE-POINT2."
  (vec-normalize (vec- line-point2 line-point1)))

(defun* line-segment-midpoint ((point1 vec) (point2 vec)) (:result vec)
  "Find the center of the line segment denoted by POINT1 and POINT2."
  (vec-lerp point1 point2 0.5))

(defun* line-plane-intersect ((line-point1 vec) (line-point2 vec)
                              (plane-point vec) (plane-normal vec)) (:result vec)
  "Find the point where a line intersects a plane."
  (let* ((direction (vec- line-point1 line-point2))
         (dot-dir-plane (vec-dot direction plane-normal))
         (plane-line (vec- line-point1 plane-point)))
    (if (zerop dot-dir-plane)
        +zero-vector+
        (let ((distance (/ (- (vec-dot plane-normal plane-line)) dot-dir-plane)))
          (point-translate line-point1 direction distance)))))

(defun* line-point-distance ((point vec) (line-point1 vec) (line-point2 vec))
    (:result single-float)
  "Find the shortest distance from POINT to the line denoted by the points
LINE-POINT1 and LINE-POINT2."
  (let* ((direction (line-direction line-point1 line-point2))
         (intersect (line-plane-intersect line-point1 line-point2 point
                                          direction)))
    (point-distance point intersect)))
