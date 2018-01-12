(in-package :cl-user)

(defpackage #:shadow
  (:use #:cl)
  (:export #:defun-gpu
           #:defstruct-gpu
           #:make-program
           #:program
           #:source
           #:attributes
           #:uniforms
           #:build-program
           #:build-dictionary
           #:with-program
           #:uniform-int
           #:uniform-int-array
           #:uniform-float
           #:uniform-float-array
           #:uniform-vec2
           #:uniform-vec2-array
           #:uniform-vec3
           #:uniform-vec3-array
           #:uniform-vec4
           #:uniform-vec4-array
           #:uniform-mat2
           #:uniform-mat2-array
           #:uniform-mat3
           #:uniform-mat3-array
           #:uniform-mat4
           #:uniform-mat4-array))
