(in-package :shadow)

(defvar *shaders* (make-hash-table))

(defun find-gpu-function (func-spec)
  (destructuring-bind (name . types) func-spec
    (find types (varjo.internals::get-external-function-by-name name nil)
          :key (lambda (x) (mapcar #'second (varjo.internals:in-args x)))
          :test #'equal)))

(defun stage-type (stage)
  (varjo.internals::stage-obj-to-name stage))

(defun stage-type->shader-type (stage-type)
  (ecase stage-type
    (:vertex :vertex-shader)
    (:tessellation-control :tess-control-shader)
    (:tessellation-evaluation :tess-evaluation-shader)
    (:geometry :geometry-shader)
    (:fragment :fragment-shader)
    (:compute :compute-shader)))

(defun make-stage (primitive stage-spec)
  (destructuring-bind (stage-type (&key (version 330)) func-spec) stage-spec
    (let ((func (find-gpu-function func-spec)))
      (varjo:make-stage
       stage-type
       (varjo.internals:in-args func)
       (varjo.internals:uniforms func)
       `(,(ensure-keyword version))
       (varjo.internals:code func)
       t
       (when (eq stage-type :vertex)
         (varjo.internals:primitive-name-to-instance primitive))))))

(defun translate-stages (primitive stage-specs)
  (varjo:rolling-translate
   (mapcar
    (lambda (x) (make-stage primitive x))
    stage-specs)))

(defun store-source (program stage)
  (let ((source (varjo:glsl-code stage)))
    (setf (gethash (stage-type stage) (source program))
          (subseq source (1+ (position #\newline source)) (- (length source) 2)))))

(defun %make-program (name primitive stage-specs)
  (let ((program (make-instance 'program))
        (stages (translate-stages primitive stage-specs)))
    (dolist (stage stages)
      (store-source program stage)
      (store-attributes program stage)
      (store-uniforms program stage))
    (store-buffer-data program stages :ubo)
    (store-buffer-data program stages :ssbo)
    (setf (gethash name *shaders*) program)
    program))

(defmacro make-program (name (&optional (primitive :triangles)) &body body)
  `(%make-program ,name ,primitive ',body))

(setf (macro-function 'defstruct-gpu) (macro-function 'varjo:v-defstruct)
      (macro-function 'defun-gpu) (macro-function 'varjo:v-defun))
