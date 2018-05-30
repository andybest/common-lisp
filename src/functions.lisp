(in-package :shadow)

(defun find-gpu-function (func-spec)
  (destructuring-bind (name . types) func-spec
    (find types (varjo.internals::get-external-function-by-name name nil)
          :key (lambda (x) (mapcar #'second (varjo.internals:in-args x)))
          :test #'equal)))

(defun get-function-spec (function)
  (cons (varjo:name function) (mapcar #'second (varjo.internals:in-args function))))

(defun generate-pseudo-lisp-function (name args-spec)
  (let ((args (au:alist-keys args-spec)))
    `(setf (symbol-function ',name)
           (lambda ,args
             (declare (ignore ,@args))
             (error "The GPU function ~a cannot be called from Lisp." ',name)))))

(defmacro defun-gpu (name args &body body)
  "Define a GPU function."
  (au:with-unique-names (fn spec)
    (destructuring-bind (in-args uniforms) (varjo.utils:split-arguments args '(&uniform))
      `(let* ((,fn (varjo:add-external-function ',name ',in-args ',uniforms ',body))
              (,spec (get-function-spec ,fn)))
         ,(generate-pseudo-lisp-function name in-args)
         (au:when-found (programs (au:href (dependencies *state*) :fn->programs ,spec))
           (funcall (modify-hook *state*) (au:hash-keys programs)))
         ,fn))))
