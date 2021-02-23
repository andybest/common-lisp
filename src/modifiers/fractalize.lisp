(in-package #:cl-user)

(defpackage #:%coherent-noise.modifiers.fractalize
  (:local-nicknames
   (#:gen #:%coherent-noise.generators)
   (#:int #:%coherent-noise.internal)
   (#:mod #:%coherent-noise.modifiers))
  (:use #:cl))

(in-package #:%coherent-noise.modifiers.fractalize)

(defun mod:fractalize (source type &rest args
                       &key (octaves 4) (frequency 1.0) (lacunarity 2.0) (persistence 0.5)
                         (attenuation 2.0))
  (unless (typep source 'int:sampler)
    (error 'int:invalid-sampler-argument
           :sampler-type 'fractalize
           :argument 'source
           :value source))
  (unless (typep octaves '(integer 1 32))
    (error 'int:invalid-fractal-octave-count
           :sampler-type 'fractalize
           :value octaves))
  (unless (realp frequency)
    (error 'int:invalid-real-argument
           :sampler-type 'fractalize
           :argument :frequency
           :value frequency))
  (unless (realp lacunarity)
    (error 'int:invalid-real-argument
           :sampler-type 'fractalize
           :argument :lacunarity
           :value lacunarity))
  (unless (realp persistence)
    (error 'int:invalid-real-argument
           :sampler-type 'fractalize
           :argument :persistence
           :value persistence))
  (unless (realp attenuation)
    (error 'int:invalid-real-argument
           :sampler-type 'fractalize
           :argument :attenuation
           :value attenuation))
  (let ((fractal (ecase type
                   (:fbm #'gen:fbm-4d)
                   (:billow #'gen:billow-4d)
                   (:multi #'gen:multifractal-4d)
                   (:hybrid-multi #'gen:hybrid-multifractal-4d)
                   (:ridged-multi #'gen:ridged-multifractal-4d))))
    (apply fractal :generator (constantly source) :allow-other-keys t args)))
