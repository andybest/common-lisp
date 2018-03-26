(in-package :shadow)

(defvar *shader-info*)

(defclass shader-info ()
  ((%programs :reader programs
              :initform (make-hash-table))
   (%uniform-block-bindings :reader uniform-block-bindings
                            :initform (make-hash-table))
   (%buffer-block-bindings :reader buffer-block-bindings
                           :initform (make-hash-table))
   (%buffers :reader buffers
             :initform (make-hash-table))))

(defun initialize-shaders ()
  "Initialize the shaders."
  (setf *shader-info* (make-instance 'shader-info)))

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

(defun make-stage (version primitive stage-spec)
  (destructuring-bind (stage-type (&key (version version)) func-spec) stage-spec
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

(defun translate-stages (version primitive stage-specs)
  (varjo:rolling-translate
   (mapcar
    (lambda (x) (make-stage version primitive x))
    stage-specs)))

(defun store-source (program stage)
  (let ((source (varjo:glsl-code stage)))
    (setf (gethash (stage-type stage) (source program))
          (subseq source (1+ (position #\newline source)) (- (length source) 2)))))

(setf (macro-function 'defstruct-gpu) (macro-function 'varjo:v-defstruct)
      (macro-function 'defun-gpu) (macro-function 'varjo:v-defun))

(setf (documentation 'defstruct-gpu 'function)
      "Define a GPU structure. This is an alias for VARI:V-DEFSTRUCT. For more information, see the
      Varjo source.")

(setf (documentation 'defun-gpu 'function)
      "Define a GPU function. This is an alias for VARI:V-DEFUN. For more information, see the Varjo
      source.")
