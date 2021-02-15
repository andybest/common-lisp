(in-package #:cl-user)

(defpackage #:coherent-noise.api
  (:local-nicknames
   (#:lp #:lparallel)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:sample
   #:write-image))

(in-package #:coherent-noise.api)

(defun sample (sampler &rest coords)
  "Sample from `sampler` using the position denoted by `coords`. `coords` is a list of floating
point numbers corresponding to each dimension of `sampler`. If there are fewer coordinates supplied
than the dimensionality of the sampler, any unspecified coordinates will be treated as zero."
  (apply sampler (mapcar (lambda (x) (float x 1d0)) coords)))

(define-compiler-macro sample (sampler &rest coords)
  `(funcall ,sampler
            ,@(mapcar (lambda (x)
                        (if (realp x)
                            (float x 1d0)
                            `(float ,x 1d0)))
                      coords)))

(defun write-image (out-file sampler &key (width 1024) (height 1024) (r 1.0) (g 1.0) (b 1.0))
  "Write out a PNG image file representation of `sampler` to the path `out-file`. `width` and
`height` denote the image dimensions in pixels, and each of these pixels is fed into the sampler to
produce a grayscale value to use for that pixel. Simple coloring can be applied to the result by
changing the `r`, `g`, and `b` arguments, which denote the contributions for the red, green, and
blue components in the range [0..1]."
  (let* ((lp:*kernel* (lp:make-kernel (cl-cpus:get-number-of-processors)))
         (data (make-array (* width height 3) :element-type 'u:ub8))
         (png (make-instance 'zpng:png
                             :color-type :truecolor
                             :width width
                             :height height
                             :image-data data)))
    (lp:pdotimes (y height)
      (dotimes (x width)
        (let ((sample (+ (* (coherent-noise.internal::sample sampler x y) 0.5) 0.5))
              (i (* (+ x (* y width)) 3)))
          (setf (aref data (+ i 0)) (u:clamp (floor (* sample r 255)) 0 255)
                (aref data (+ i 1)) (u:clamp (floor (* sample g 255)) 0 255)
                (aref data (+ i 2)) (u:clamp (floor (* sample b 255)) 0 255)))))
    (zpng:write-png png out-file)))
