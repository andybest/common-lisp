(in-package :shadow)

(defclass state ()
  ((%programs :reader programs
              :initform (au:dict #'eq))
   (%blocks :reader blocks
            :initform (au:dict #'eq :bindings (au:dict #'eq :uniform (au:dict)
                                                            :buffer (au:dict))
                                    :aliases (au:dict #'equalp)))
   (%dependencies :reader dependencies
                  :initform (au:dict #'eq :fn->deps (au:dict #'equal)
                                          :dep->fns (au:dict #'equal)
                                          :stage-fn->programs (au:dict #'equal)
                                          ;; new
                                          :fn->programs (au:dict #'equal)
                                          :program->fns (au:dict #'eq)))
   (%modify-hook :accessor modify-hook
                 :initform (constantly nil))
   (%buffers :reader buffers
             :initform (au:dict #'eq))))

(defvar *state* (make-instance 'state))

(defun store-source (program stage)
  (let ((source (varjo:glsl-code stage)))
    (setf (au:href (source program) (stage-type stage))
          (subseq source (1+ (position #\newline source)) (- (length source) 2)))))

(defmacro defstruct-gpu (name context &body slots)
  "Define a GPU structure."
  `(varjo:v-defstruct ,name ,context ,@slots))

(defmacro defmacro-gpu (name lambda-list &body body)
  "Define a GPU macro."
  `(varjo:v-defmacro ,name ,lambda-list ,@body))

;; maps fn specs to a table of program names pointing to themselves
;; this allows us to remove a program name for a given spec
;; fn->programs = spec -> prog-name -> prog-name

;; maps program names to a table of fn specs pointing to themselves
;; this allows us to remove a fn spec for a given program
;; program->fns = prog-name -> spec -> spec
