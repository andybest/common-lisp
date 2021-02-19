(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers.fractalize
  (:local-nicknames
   (#:gen #:coherent-noise.generators)
   (#:mod #:coherent-noise.modifiers))
  (:use #:cl))

(in-package #:coherent-noise.modifiers.fractalize)

(defun mod:fractalize (source type &rest args
                       &key (octaves 4) (frequency 1.0) (lacunarity 2.0) (persistence 0.5)
                         (attenuation 2.0))
  (declare (ignorable octaves frequency lacunarity persistence attenuation))
  (let ((fractal (ecase type
                   (:fbm #'gen:fbm-4d)
                   (:billow #'gen:billow-4d)
                   (:multi #'gen:multifractal-4d)
                   (:hybrid-multi #'gen:hybrid-multifractal-4d)
                   (:ridged-multi #'gen:ridged-multifractal-4d))))
    (apply fractal :generator (constantly source) :allow-other-keys t args)))
