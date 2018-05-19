(in-package :umbra.math)

;;; Constants

(defconstant +epsilon+ 1e-20)
(defconstant +pi+ (float pi 1.0f0))
(defconstant +half-pi+ (/ +pi+ 2))
(defconstant +k-log-base-10+ (/ (log 10 2)))

;;; log10
;;; Base 10 logarithm

(defun-gpu log10 ((x :float))
  (* (log2 x) +k-log-base-10+))

(defun-gpu log10 ((x :vec2))
  (* (log2 x) +k-log-base-10+))

(defun-gpu log10 ((x :vec3))
  (* (log2 x) +k-log-base-10+))

(defun-gpu log10 ((x :vec4))
  (* (log2 x) +k-log-base-10+))

;;; Saturate
;;; Clamp a value within the [0, 1] range. From HLSL.

(defun-gpu saturate ((x :float))
  (clamp x 0.0 1.0))

(defun-gpu saturate ((x :vec2))
  (clamp x 0.0 1.0))

(defun-gpu saturate ((x :vec3))
  (clamp x 0.0 1.0))

(defun-gpu saturate ((x :vec4))
  (clamp x 0.0 1.0))
