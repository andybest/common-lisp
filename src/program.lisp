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

(defstruct (metadata (:type vector)
                     (:constructor make-metadata (&key name type location))
                     (:copier nil)
                     (:predicate nil))
  name
  type
  (location -1))

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

(defun store-attribute-locations (program)
  (let ((id (id program)))
    (gl:use-program id)
    (maphash
     (lambda (k v)
       (declare (ignore k))
       (setf (metadata-location v) (gl:get-attrib-location id (metadata-name v))))
     (attributes program))
    (gl:use-program 0)))

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
    (store-attribute-locations object)
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
