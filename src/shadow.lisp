(in-package #:net.mfiano.lisp.shadow)

(defvar *metadata* (u:dict))

(defun meta (key)
  (u:href *metadata* key))

(defun (setf meta) (value key)
  (setf (u:href *metadata* key) value))

(defun reset-program-state ()
  (setf (meta :programs) (u:dict)
        (meta :block-bindings) (u:dict :uniform (u:dict) :buffer (u:dict))
        (meta :block-aliases) (u:dict #'equalp)
        (meta :buffers) (u:dict #'equalp)))

(defun enable-dependency-tracking ()
  (setf (meta :track-dependencies-p) t))

(defun disable-dependency-tracking ()
  (setf (meta :track-dependencies-p) nil))

(defun store-source (program stage)
  (let ((source (varjo:glsl-code stage)))
    (setf (u:href (source program) (stage-type stage))
          (subseq source
                  (1+ (position #\newline source))
                  (- (length source) 2)))))

(defun load-shaders (modify-hook)
  (reset-program-state)
  (u:do-hash-values (shader-factory (meta :shader-definitions))
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

(setf (meta :track-dependencies-p) nil
      (meta :fn->deps) (u:dict #'equal)
      (meta :dep->fns) (u:dict #'equal)
      (meta :stage-fn->programs) (u:dict #'equal)
      (meta :modify-hook) (constantly nil)
      (meta :shader-definitions) (u:dict))

(reset-program-state)
