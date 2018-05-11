(in-package :shadow)

(defvar *active-shader-program*)

(defclass program ()
  ((%id :reader id
        :initform 0)
   (%name :reader name
          :initarg :name)
   (%version :reader version
             :initarg :version)
   (%translated-stages :reader translated-stages
                       :initform nil)
   (%source :reader source
            :initform (au:dict #'eq))
   (%primitive :reader primitive
               :initarg :primitive)
   (%stage-specs :reader stage-specs
                 :initarg :stage-specs)
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
           (error "Failed to compile ~a shader stage:~%~a~%" type (gl:get-shader-info-log shader)))))
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
            (error "Failed to link shader program: ~a" (gl:get-program-info-log program)))
          (dolist (shader shaders)
            (gl:detach-shader program shader)
            (gl:delete-shader shader))))
    program))

(defun build-shader-program (name)
  "Compile the shader stages of NAME, linking them into a program. NAME refers to a previously
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

(defun store-stage-program-dependencies (program)
  (dolist (stage-spec (stage-specs program))
    (destructuring-bind (stage-type () func-spec) stage-spec
      (declare (ignore stage-type))
      (pushnew (name program)
               (au:href (dependencies *shader-info*) :stage-fn->programs func-spec)))))

(defun translate-program (program-name)
  (let ((program (find-program program-name)))
    (with-slots (%name %version %primitive %stage-specs) program
      (let ((stages (translate-stages %version %primitive %stage-specs)))
        (dolist (stage stages)
          (store-source program stage)
          (store-blocks program stage))
        (setf (slot-value program '%translated-stages) stages)))))

(defun %make-shader-program (name version primitive stage-specs)
  (let ((program (make-instance 'program
                                :name name
                                :version version
                                :primitive primitive
                                :stage-specs stage-specs)))
    (setf (au:href (programs *shader-info*) name) program)
    (translate-program name)
    (store-attributes program)
    (store-uniforms program)
    (store-stage-program-dependencies program)
    program))

(defmacro make-shader-program (name (&key (version :330) (primitive :triangles)) &body body)
  "Create a new shader program using the stage-specs defined in BODY.

VERSION: The default version shader stages use, and can be overridden on a per-function basis.

PRIMITIVE: The drawing primitive to use for the vertex stage."
  `(%make-shader-program ',name ,version ,primitive ',body))

(defun update-shader-programs (program-list)
  "Re-translate and re-compile a collection of shader programs denoted by `PROGRAM-LIST`, which is a
list of their names"
  (dolist (program program-list)
    (translate-program program)
    (build-shader-program program)))

(defun set-modify-hook (function)
  "Specify a function to be called when shader programs need to be updated."
  (setf (modify-hook *shader-info*) function))

(defmacro with-shader-program (name &body body)
  "Run a body of code which uses (as in glUseProgram) the program identified by NAME."
  `(let ((*active-shader-program* (find-program ,name)))
     (gl:use-program (id *active-shader-program*))
     ,@body
     (gl:use-program 0)))
