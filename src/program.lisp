(in-package :shadow)

(defvar *active-shader-program*)

(defclass program ()
  ((%id :reader id
        :initform 0)
   (%stages :reader stages
            :initform nil)
   (%source :reader source
            :initform (au:dict #'eq))
   (%attributes :reader attributes
                :initform (au:dict #'eq))
   (%uniforms :reader uniforms
              :initform (au:dict #'eq))
   (%blocks :reader blocks
            :initform (au:dict #'equal))))

(defstruct (stage-variable (:type vector)
                           (:constructor make-stage-variable (&key name type location))
                           (:copier nil)
                           (:predicate nil))
  name
  type
  (location -1))

(defun find-program (program-name)
  (au:href (programs *shader-info*) program-name))

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

(defun build-shader-program (name)
  "Compile the shader stages of NAME, linking them into a program. NAME refers a a previously
defined shader program using MAKE-SHADER-PROGRAM.

See MAKE-SHADER-PROGRAM"
  (let* ((program (find-program name))
         (shaders (compile-stages program))
         (id (link-program shaders)))
    (setf (slot-value program '%id) id)
    (store-attribute-locations program)
    (store-uniform-locations program)
    id))

(defun build-shader-dictionary ()
  "Compile all shader programs defined with MAKE-SHADER-PROGRAM.

See MAKE-SHADER-PROGRAM"
  (au:maphash-keys #'build-shader-program (programs *shader-info*))
  (programs *shader-info*))

(defun %make-shader-program (name version primitive stage-specs)
  (let ((program (make-instance 'program))
        (stages (translate-stages version primitive stage-specs)))
    (dolist (stage stages)
      (store-source program stage)
      (store-attributes program stage)
      (store-uniforms program stage)
      (store-blocks program stage))
    (setf (au:href (programs *shader-info*) name) program
          (slot-value program '%stages) stages)
    program))

(defmacro make-shader-program (name (&key (version :330) (primitive :triangles)) &body body)
  "Create a new shader program using the stage-specs defined in BODY.

VERSION: The default version shader stages use, and can be overridden on a per-function basis.

PRIMITIVE: The drawing primitive to use for the vertex stage."
  `(%make-shader-program ',name ,version ,primitive ',body))

(defmacro with-shader-program (name &body body)
  "Run a body of code which uses (as in glUseProgram) the program identified by NAME."
  `(let ((*active-shader-program* (find-program ,name)))
     (gl:use-program (id *active-shader-program*))
     ,@body
     (gl:use-program 0)))
