(in-package :shadow)

(defun generate-gpu-function (func-spec)
  (destructuring-bind (name . types) func-spec
    (find types (varjo.internals::get-external-function-by-name name nil)
          :key (lambda (x) (mapcar #'second (varjo.internals:in-args x)))
          :test #'equal)))

(defun generate-shader-stage (stage-spec primitive)
  (destructuring-bind (stage-type (&key (version 330)) func-spec) stage-spec
    (let ((func (generate-gpu-function func-spec)))
      (varjo.internals:make-stage
       stage-type
       (varjo.internals:in-args func)
       (varjo.internals:uniforms func)
       `(,(alexandria:make-keyword (format nil "~a" version)))
       (varjo.internals:code func)
       t
       (when (eq stage-type :vertex)
         (varjo.internals:primitive-name-to-instance primitive))))))

(defmacro make-pipeline ((&key (primitive :triangles)) &body body)
  `(mapcar
    (lambda (x)
      (generate-shader-stage x ,primitive))
    ',body))

(defun compile-program (pipeline)
  (varjo.internals:rolling-translate pipeline))

(setf (macro-function 'defstruct-gpu) (macro-function 'varjo.internals:v-defstruct)
      (macro-function 'defun-gpu) (macro-function 'varjo.internals:v-defun))
