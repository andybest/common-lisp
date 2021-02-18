(in-package #:coherent-noise.internal)

(u:define-constant +default-lacunarity+ (float (/ (* pi 2) 3) 1f0))

(defun make-fractal-sources (generator rng count)
  (let ((sources (make-array count)))
    (dotimes (i count)
      (let ((rng (rng:make-generator rng)))
        (setf (aref sources i) (funcall generator :seed (rng:get-seed rng)))))
    sources))
