(in-package :cl-user)

(defpackage #:shadow
  (:use #:cl)
  (:export #:defun-gpu
           #:defstruct-gpu
           #:initialize-shaders
           #:make-shader-program
           #:build-shader-program
           #:build-shader-dictionary
           #:bind-uniform-block
           #:bind-shader-storage-block
           #:create-buffer
           #:delete-buffer
           #:write-buffer-path
           #:with-shader-program
           #:uniforms
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
