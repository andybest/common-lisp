(in-package :umbra.math)

;;; Constants

(defconstant +pi+ (float pi 1.0f0))
(defconstant +half-pi+ (/ +pi+ 2))

;;; Saturate
;;; Clamp a value within the [0, 1] range. From HLSL.

(defun-gpu saturate ((value :float))
  (clamp value 0.0 1.0))

(defun-gpu saturate ((value :vec2))
  (clamp value 0.0 1.0))

(defun-gpu saturate ((value :vec3))
  (clamp value 0.0 1.0))

(defun-gpu saturate ((value :vec4))
  (clamp value 0.0 1.0))
