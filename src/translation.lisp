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

(defun store-attributes (program stage)
  (when (eq (stage-type stage) :vertex)
    (loop :for attr :in (varjo:input-variables stage)
          :for id = (ensure-keyword (varjo:name attr))
          :for type = (varjo:v-type-of attr)
          :do (setf (gethash id (attributes program))
                    (make-metadata :name (varjo:glsl-name attr)
                                   :type (varjo:type->type-spec type))))))

(defgeneric get-type-info (type parts)
  (:method (type parts)
    (list (list (reverse parts) (varjo:type->type-spec type)))))

(defmethod get-type-info ((type varjo:v-user-struct) parts)
  (cons
   (list (reverse parts) (ensure-keyword (varjo:type->type-spec type)))
   (loop :for (slot-name slot-type . nil) :in (varjo.internals:v-slots type)
         :append (get-type-info slot-type (cons slot-name parts)))))

(defmethod get-type-info ((type varjo:v-array) parts)
  (loop :with dimensions = (first (varjo:v-dimensions type))
        :with element-type = (varjo:v-element-type type)
        :for i :below dimensions
        :when (zerop i)
          :collect (list (reverse parts) (cons (varjo:type->type-spec element-type) dimensions))
        :append (get-type-info element-type (cons i parts))))

(defun store-uniforms (program stage)
  (flet ((get-uniform-info (stage)
             (loop :for uniform :in (varjo:uniform-variables stage)
                   :for type = (varjo:v-type-of uniform)
                   :append (get-type-info type (list (varjo:name uniform))))))
    (loop :for (parts type) :in (get-uniform-info stage)
          :for id = (ensure-keyword (parts->string parts))
          :do (setf (gethash id (uniforms program))
                    (make-metadata
                     :name (parts->string parts #'varjo.internals:safe-glsl-name-string)
                     :type type)))))

(defun %make-program (name primitive stage-specs)
  (let ((program (make-instance 'program))
        (stages (translate-stages primitive stage-specs)))
    (dolist (stage stages)
      (store-source program stage)
      (store-attributes program stage)
      (store-uniforms program stage))
    (setf (gethash name *shaders*) program)
    program))

(defmacro make-program (name (&optional (primitive :triangles)) &body body)
  `(%make-program ,name ,primitive ',body))

(setf (macro-function 'defstruct-gpu) (macro-function 'varjo:v-defstruct)
      (macro-function 'defun-gpu) (macro-function 'varjo:v-defun))
