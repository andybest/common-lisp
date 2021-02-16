(in-package :cl-user)

(uiop:define-package #:coherent-noise
  (:mix #:coherent-noise.modifiers #:cl)
  (:reexport #:coherent-noise.modifiers)
  (:mix-reexport #:coherent-noise.generators
                 #:coherent-noise.internal))
