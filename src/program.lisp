(in-package :shadow)

(defvar *active-program*)

(defclass program ()
  ((%id :reader id
        :initform 0)
   (%source :reader source
            :initform (make-hash-table))
   (%attributes :reader attributes
                :initform (make-hash-table))
   (%uniforms :reader uniforms
              :initform (make-hash-table))))

(defstruct (stage-variable (:type vector)
                           (:constructor make-stage-variable (&key name type location))
                           (:copier nil)
                           (:predicate nil))
  name
  type
  (location -1))

(defun program-by-name (program-name)
  (gethash program-name (programs *shader-info*)))

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

(defun build-program (name)
  (let* ((program (program-by-name name))
         (shaders (compile-stages program))
         (id (link-program shaders)))
    (setf (slot-value program'%id) id)
    (store-attribute-locations program)
    (store-uniform-locations program)
    id))

(defun build-dictionary ()
  (initialize-buffers)
  (maphash
   (lambda (k v)
     (build-program k)
     (bind-blocks v))
   (programs *shader-info*)))

(defun %make-program (name version primitive stage-specs)
  (let ((program (make-instance 'program))
        (stages (translate-stages version primitive stage-specs)))
    (dolist (stage stages)
      (store-source program stage)
      (store-attributes program stage)
      (store-uniforms program stage))
    (store-blocks stages)
    (setf (gethash name (programs *shader-info*)) program)
    program))

(defmacro make-program (name (&key (version :330) (primitive :triangles)) &body body)
  `(%make-program ,name ,version ,primitive ',body))

(defmacro with-program (name &body body)
  `(let ((*active-program* (program-by-name ,name)))
     (gl:use-program (id *active-program*))
     ,@body
     (gl:use-program 0)))
