(in-package :shadow)

(defvar *shader-info*)

(defclass shader-info ()
  ((%programs :reader programs
              :initform (au:dict #'eq))
   (%block-bindings :reader block-bindings
                    :initform (au:dict #'eq :uniform (au:dict) :buffer (au:dict)))
   (%dependencies :reader dependencies
                  :initform (au:dict #'eq
                                     :fn->deps (au:dict #'equal)
                                     :dep->fns (au:dict #'equal)
                                     :stage-fn->programs (au:dict #'equal)))
   (%modify-hook :accessor modify-hook
                 :initform (constantly nil))
   (%buffers :reader buffers
             :initform (au:dict #'eq))))

(defun initialize-shaders ()
  "Initialize the shaders."
  (setf *shader-info* (make-instance 'shader-info)))

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
    (setf (au:href (source program) (stage-type stage))
          (subseq source (1+ (position #\newline source)) (- (length source) 2)))))

(defmacro defstruct-gpu (name context &body slots)
  "Define a GPU structure."
  `(varjo:v-defstruct ,name ,context ,@slots))

(defmacro defmacro-gpu (name lambda-list &body body)
  "Define a GPU macro."
  `(varjo:v-defmacro ,name ,lambda-list ,@body))
