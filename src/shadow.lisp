(in-package #:shadow)

(defclass state ()
  ((%shader-definitions :reader shader-definitions
                        :initform (u:dict #'eq))
   (%programs :reader programs
              :initform (u:dict #'eq))
   (%blocks :reader blocks
            :initform (u:dict #'eq
                              :bindings (u:dict #'eq
                                                :uniform (u:dict)
                                                :buffer (u:dict))
                              :aliases (u:dict #'equalp)))
   (%track-dependencies-p :reader track-dependencies-p
                          :initform nil)
   (%dependencies :reader dependencies
                  :initform (u:dict #'eq
                                    :fn->deps (u:dict #'equal)
                                    :dep->fns (u:dict #'equal)
                                    :stage-fn->programs (u:dict #'equal)))
   (%modify-hook :accessor modify-hook
                 :initform (constantly nil))
   (%buffers :reader buffers
             :initform (u:dict #'eq))))

(defvar *state* (make-instance 'state))

(defun reset-program-state ()
  (clrhash (u:href (blocks *state*) :bindings :uniform))
  (clrhash (u:href (blocks *state*) :bindings :buffer))
  (clrhash (u:href (blocks *state*) :aliases))
  (clrhash (buffers *state*)))

(defun enable-dependency-tracking ()
  (setf (slot-value *state* '%track-dependencies-p) t))

(defun disable-dependency-tracking ()
  (setf (slot-value *state* '%track-dependencies-p) nil))

(defun store-source (program stage)
  (let ((source (varjo:glsl-code stage)))
    (setf (u:href (source program) (stage-type stage))
          (subseq source
                  (1+ (position #\newline source))
                  (- (length source) 2)))))

(defun load-shaders (modify-hook)
  (reset-program-state)
  (u:do-hash-values (shader-factory (shader-definitions *state*))
    (funcall shader-factory))
  (enable-dependency-tracking)
  (set-modify-hook modify-hook)
  (build-shader-dictionary))

(defun unload-shaders ()
  (set-modify-hook (constantly nil))
  (disable-dependency-tracking))

(defun recompile-shaders (programs-list)
  (when programs-list
    (translate-shader-programs programs-list)
    (build-shader-programs programs-list)
    (rebind-blocks programs-list)))

(defmacro define-struct (name &body slots)
  "Define a GPU structure."
  `(varjo:define-vari-struct ,name () ,@slots))

(defmacro define-macro (name lambda-list &body body)
  "Define a GPU macro."
  `(varjo:define-vari-macro ,name ,lambda-list ,@body))
