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
  (location -1))

(defun ensure-keyword (x)
  (etypecase x
    ((or number string symbol)
     (alexandria:make-keyword (format nil "~a" x)))))

(defun parts->string (parts &optional (filter #'identity))
  (with-output-to-string (s)
    (flet ((convert (parts)
             (mapcar
              (lambda (part)
                (etypecase part
                  ((or symbol string) (funcall filter part))
                  (integer part)))
              parts)))
      (loop :for (part . rest) :on (convert parts)
            :for separator = "" :then "."
            :do (etypecase part
                  ((or symbol string) (format s "~a~a" separator part))
                  (integer (format s "[~a]" part)))))))

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
          :for id = (ensure-keyword (varjo:name attr))
          :for type = (varjo:v-type-of attr)
          :do (setf (gethash id (attributes program))
                    (make-metadata :name (varjo:glsl-name attr)
                                   :type (varjo:type->type-spec type))))))

(defgeneric get-type-info (type parts)
  (:method (type parts)
    (list (list (reverse parts) type))))

(defmethod get-type-info ((type varjo:v-user-struct) parts)
  (loop :for (slot-name slot-type . nil) :in (varjo.internals:v-slots type)
        :append (get-type-info slot-type (cons slot-name parts))))

(defmethod get-type-info ((type varjo:v-array) parts)
  (loop :for i :below (first (varjo:v-dimensions type))
        :for element-type = (varjo:v-element-type type)
        :when (zerop i)
          :append (get-type-info element-type parts)
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
                     :type (varjo:type->type-spec type))))))

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

(defun get-uniform-location (uniform)
  (metadata-location (gethash uniform (uniforms *active-program*))))

(defmacro %uniform-array (location func component-count element-type sequence)
  (alexandria:with-gensyms (count sv)
    `(let ((,count (length ,sequence)))
       (static-vectors:with-static-vector
           (,sv (* ,count ,component-count)
                :element-type ',element-type)
         ,(if (= component-count 1)
              `(replace ,sv ,sequence)
              `(let ((i 0))
                 (map nil
                      (lambda (x)
                        (replace ,sv x :start1 i)
                        (incf i ,component-count))
                      ,sequence)))
         (,func ,location ,count (static-vectors:static-vector-pointer ,sv))))))

(defun uniform-int (uniform value)
  (let ((location (get-uniform-location uniform)))
    (%gl:uniform-1i location value)))

(defun uniform-int-array (uniform value)
  (let ((location (get-uniform-location uniform)))
    (%uniform-array location %gl:uniform-1iv 1 (unsigned-byte 32) value)))

(defun uniform-float (uniform value)
  (let ((location (get-uniform-location uniform)))
    (%gl:uniform-1f location value)))

(defun uniform-float-array (uniform value)
  (let ((location (get-uniform-location uniform)))
    (%uniform-array location %gl:uniform-1fv 1 single-float value)))

(defun uniform-vec2 (uniform value)
  (let ((location (get-uniform-location uniform)))
    (%gl:uniform-2f location (aref value 0) (aref value 1))))

(defun uniform-vec2-array (uniform value)
  (let ((location (get-uniform-location uniform)))
    (%uniform-array location %gl:uniform-2fv 2 single-float value)))

(defun uniform-vec3 (uniform value)
  (let ((location (get-uniform-location uniform)))
    (%gl:uniform-3f location (aref value 0) (aref value 1) (aref value 2))))

(defun uniform-vec3-array (uniform value)
  (let ((location (get-uniform-location uniform)))
    (%uniform-array location %gl:uniform-3fv 3 single-float value)))

(defun uniform-vec4 (uniform value)
  (let ((location (get-uniform-location uniform)))
    (%gl:uniform-4f location (aref value 0) (aref value 1) (aref value 2) (aref value 3))))

(defun uniform-vec4-array (uniform value)
  (let ((location (get-uniform-location uniform)))
    (%uniform-array location %gl:uniform-4fv 4 single-float value)))

(defun uniform-mat2 (uniform value)
  (let ((location (get-uniform-location uniform)))
    (gl:uniform-matrix-2fv location value nil)))

(defun uniform-mat2-array (uniform value)
  (let ((location (get-uniform-location uniform)))
    (gl:uniform-matrix location 2 value nil)))

(defun uniform-mat3 (uniform value)
  (let ((location (get-uniform-location uniform)))
    (gl:uniform-matrix-3fv location value nil)))

(defun uniform-mat3-array (uniform value)
  (let ((location (get-uniform-location uniform)))
    (gl:uniform-matrix location 3 value nil)))

(defun uniform-mat4 (uniform value)
  (let ((location (get-uniform-location uniform)))
    (gl:uniform-matrix-4fv location value nil)))

(defun uniform-mat4-array (uniform value)
  (let ((location (get-uniform-location uniform)))
    (gl:uniform-matrix location 4 value nil)))
