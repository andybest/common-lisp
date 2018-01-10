(in-package :shadow)

(defvar *shaders* (make-hash-table))

(defclass program ()
  ((%source :reader source
            :initform (make-hash-table))
   (%attribute-names :reader attribute-names
                     :initform (make-hash-table))
   (%attribute-types :reader attribute-types
                     :initform (make-hash-table))
   (%attribute-locations :reader attribute-locations
                         :initform (make-hash-table))
   (%uniform-names :reader uniform-names
                   :initform (make-hash-table))
   (%uniform-types :reader uniform-types
                   :initform (make-hash-table))
   (%uniform-locations :reader uniform-locations
                       :initform (make-hash-table))))

(defun ensure-keyword (x)
  (etypecase x
    ((or number string symbol)
     (alexandria:make-keyword (format nil "~a" x)))))

(defgeneric join-parts (target parts)
  (:method ((target (eql :lisp)) parts)
    (ensure-keyword (format nil "~{~a~^.~}" parts)))
  (:method ((target (eql :glsl)) parts)
    (format nil "~{~a~^.~}" (mapcar #'safe-glsl-name-string parts))))

(defun find-gpu-function (func-spec)
  (destructuring-bind (name . types) func-spec
    (find types (varjo.internals::get-external-function-by-name name nil)
          :key (lambda (x) (mapcar #'second (in-args x)))
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

(defun %make-stage (primitive stage-spec)
  (destructuring-bind (stage-type (&key (version 330)) func-spec) stage-spec
    (let ((func (find-gpu-function func-spec))
          (context `(,(ensure-keyword version)))
          (primitive (when (eq stage-type :vertex)
                       (primitive-name-to-instance primitive))))
      (with-accessors ((in-args in-args) (uniforms uniforms) (code code)) func
        (make-stage stage-type in-args uniforms context code t primitive)))))

(defun translate-stages (primitive stage-specs)
  (rolling-translate
   (mapcar
    (lambda (x) (%make-stage primitive x))
    stage-specs)))

(defun store-source (program stage)
  (let ((source (glsl-code stage)))
    (setf (gethash (stage-type stage) (source program))
          (subseq source (1+ (position #\newline source)) (- (length source) 2)))))

(defun store-attributes (program stage)
  (when (eq (stage-type stage) :vertex)
    (loop :for attr :in (input-variables stage)
          :for id = (ensure-keyword (name attr))
          :for type = (v-type-of attr)
          :do (setf (gethash id (attribute-names program)) (glsl-name attr)
                    (gethash id (attribute-types program)) (type->type-spec type)))))

(defun store-uniforms (program stage)
  (labels ((get-type-info (type parts)
             (if (typep type 'v-user-struct)
                 (loop :for (slot-name slot-type . nil) :in (v-slots type)
                       :append (get-type-info slot-type (cons slot-name parts)))
                 (list (list (reverse parts) type))))
           (get-uniform-info (stage)
             (loop :for uniform :in (uniform-variables stage)
                   :for type = (v-type-of uniform)
                   :append (get-type-info type (list (name uniform))))))
    (loop :for (parts type) :in (get-uniform-info stage)
          :for id = (join-parts :lisp parts)
          :do (setf (gethash id (uniform-names program)) (join-parts :glsl parts)
                    (gethash id (uniform-types program)) (type->type-spec type)))))

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

(setf (macro-function 'defstruct-gpu) (macro-function 'v-defstruct)
      (macro-function 'defun-gpu) (macro-function 'v-defun))

;;;; WIP

(defun build-stage (type source)
  (let ((shader (gl:create-shader type)))
    (gl:shader-source shader source)
    (gl:compile-shader shader)
    (if (gl:get-shader shader :compile-status)
        shader
        (error "Failed to compile ~a shader stage:~% ~a~%"
               type (gl:get-shader-info-log shader)))))

(defun build-program (shaders)
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
          (if (gl:get-program program :link-status)
              program
              (error "Failed to link shader program: ~a"
                     (gl:get-program-info-log program)))
          (dolist (shader shaders)
            (gl:detach-shader program shader)
            (gl:delete-shader shader))))))
