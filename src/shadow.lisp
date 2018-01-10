(in-package :shadow)

(defvar *active-program*)
(defvar *shaders* (make-hash-table))

(defclass program ()
  ((%id :reader id
        :initform 0)
   (%source :reader source
            :initform (make-hash-table))
   (%attributes :reader attributes
                :initform (make-hash-table))
   (%uniforms :reader uniforms
              :initform (make-hash-table))))

(defstruct (metadata (:type vector)
                     (:constructor make-metadata (&key name type location))
                     (:copier nil)
                     (:predicate nil))
  name
  type
  location)

(defun ensure-keyword (x)
  (etypecase x
    ((or number string symbol)
     (alexandria:make-keyword (format nil "~a" x)))))

(defgeneric join-parts (target parts)
  (:method ((target (eql :lisp)) parts)
    (ensure-keyword (format nil "~{~a~^.~}" parts)))
  (:method ((target (eql :glsl)) parts)
    (format nil "~{~a~^.~}" (mapcar #'varjo.internals:safe-glsl-name-string parts))))

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
    (:tessllation-control :tess-control-shader)
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
          :for location :from 0
          :for id = (ensure-keyword (varjo:name attr))
          :for type = (varjo:v-type-of attr)
          :do (setf (gethash id (attributes program))
                    (make-metadata :name (varjo:glsl-name attr)
                                   :type (varjo:type->type-spec type)
                                   :location location)))))

(defun store-uniforms (program stage)
  (labels ((get-type-info (type parts)
             (if (typep type 'varjo:v-user-struct)
                 (loop :for (slot-name slot-type . nil) :in (varjo.internals:v-slots type)
                       :append (get-type-info slot-type (cons slot-name parts)))
                 (list (list (reverse parts) type))))
           (get-uniform-info (stage)
             (loop :for uniform :in (varjo:uniform-variables stage)
                   :for type = (varjo:v-type-of uniform)
                   :append (get-type-info type (list (varjo:name uniform))))))
    (loop :for (parts type) :in (get-uniform-info stage)
          :for id = (join-parts :lisp parts)
          :do (setf (gethash id (uniforms program))
                    (make-metadata :name (join-parts :glsl parts)
                                   :type (varjo:type->type-spec type)
                                   :location -1)))))

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

(defun compile-stages (program)
  (let ((shaders))
    (maphash
     (lambda (k v)
       (let* ((type (stage-type->shader-type k))
              (shader (gl:create-shader type)))
         (gl:shader-source shader v)
         (gl:compile-shader shader)
         (push shader shaders)
         (unless (gl:get-shader shader :compile-status)
           (error "Failed to compile ~a shader stage:~%~a~%"
                  type (gl:get-shader-info-log shader)))))
     (source program))
    shaders))

(defun link-program (shaders)
  (let ((program (gl:create-program)))
    (if (zerop program)
        (progn
          (dolist (shader shaders)
            (gl:delete-shader shader))
          (error "Failed to create program: ~a" (gl:get-error)))
        (progn
          (dolist (shader shaders)
            (gl:attach-shader program shader))
          (gl:link-program program)
          (unless (gl:get-program program :link-status)
            (error "Failed to link shader program: ~a"
                   (gl:get-program-info-log program)))
          (dolist (shader shaders)
            (gl:detach-shader program shader)
            (gl:delete-shader shader))))
    program))

(defun store-uniform-locations (program)
  (let ((id (id program)))
    (gl:use-program id)
    (maphash
     (lambda (k v)
       (declare (ignore k))
       (setf (metadata-location v) (gl:get-uniform-location id (metadata-name v))))
     (uniforms program))
    (gl:use-program 0)))

(defun build-program (name)
  (let* ((object (gethash name *shaders*))
         (shaders (compile-stages object))
         (program (link-program shaders)))
    (setf (slot-value object '%id) program)
    (store-uniform-locations object)
    program))

(defun build-dictionary ()
  (maphash
   (lambda (k v)
     (declare (ignore v))
     (build-program k))
   *shaders*))

(defmacro with-program (name &body body)
  `(let ((*active-program* (gethash ,name *shaders*)))
     (gl:use-program (id *active-program*))
     ,@body
     (gl:use-program 0)))

(defun get-uniform-location (uniform)
  (metadata-location (gethash uniform (uniforms *active-program*))))

(defun uniform-bool (uniform value)
  (let ((location (get-uniform-location uniform)))
    (gl:uniformi location (ecase value ((nil 0) 0) ((t 1) 1)))))

(defun uniform-int (uniform value)
  (let ((location (get-uniform-location uniform)))
    (gl:uniformi location value)))

(defun uniform-float (uniform value)
  (let ((location (get-uniform-location uniform)))
    (gl:uniformf location value)))

(defun uniform-vec (uniform value)
  (let ((location (get-uniform-location uniform)))
    (gl:uniformfv location value)))

(defun uniform-mat2 (uniform value)
  (let ((location (get-uniform-location uniform)))
    (gl:uniform-matrix-2fv location value nil)))

(defun uniform-mat3 (uniform value)
  (let ((location (get-uniform-location uniform)))
    (gl:uniform-matrix-3fv location value nil)))

(defun uniform-mat4 (uniform value)
  (let ((location (get-uniform-location uniform)))
    (gl:uniform-matrix-4fv location value nil)))
