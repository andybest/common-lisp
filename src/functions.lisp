(in-package :shadow)

(defun find-gpu-function (func-spec)
  (destructuring-bind (name . types) func-spec
    (find types (varjo.internals::get-external-function-by-name name nil)
          :key (lambda (x) (mapcar #'second (varjo.internals:in-args x)))
          :test #'equal)))

(defun get-function-spec (function)
  (cons (varjo:name function) (mapcar #'second (varjo.internals:in-args function))))

(defun ensure-function-dependency-tables (spec fn-deps dep-fns)
  (unless (au:href dep-fns spec)
    (setf (au:href dep-fns spec) (au:dict #'equal)))
  (unless (au:href fn-deps spec)
    (setf (au:href fn-deps spec) (au:dict #'equal))))

(defun store-function-dependencies (spec dependencies)
  (symbol-macrolet ((fn-deps (au:href (dependencies *shader-info*) :fn->deps))
                    (dep-fns (au:href (dependencies *shader-info*) :dep->fns)))
    (when (au:href fn-deps spec)
      (au:do-hash-keys (k (au:href fn-deps spec))
        (au:when-found (dep-key (au:href dep-fns k))
          (remhash spec dep-key))))
    (ensure-function-dependency-tables spec fn-deps dep-fns)
    (dolist (dep dependencies)
      (let ((dep-spec (get-function-spec dep)))
        (unless (au:href dep-fns dep-spec)
          (setf (au:href dep-fns dep-spec) (au:dict #'equal)))
        (au:do-hash-keys (k (au:href dep-fns spec))
          (setf (au:href fn-deps spec k dep-spec) dep-spec
                (au:href dep-fns dep-spec k) k))
        (setf (au:href fn-deps spec dep-spec) dep-spec
              (au:href dep-fns dep-spec spec) spec)))))

(defun compute-outdated-programs (spec)
  (let ((programs)
        (spec-fns (au:href (dependencies *shader-info*) :dep->fns spec)))
    (maphash
     (lambda (k v)
       (when (or (au:href spec-fns k)
                 (equal k spec))
         (setf programs (union v programs :test #'equal))))
     (au:href (dependencies *shader-info*) :stage-fn->programs))
    programs))

(defmacro defun-gpu (name args &body body)
  "Define a GPU function."
  (au:with-unique-names (split-details deps fn spec)
    (let ((split-args (varjo.utils:split-arguments args '(&uniform &context))))
      (destructuring-bind (in-args uniforms context) split-args
        `(let* ((,split-details (varjo:test-translate-function-split-details
                                 ',name ',in-args ',uniforms ',context ',body))
                (,deps (varjo:used-external-functions (first ,split-details)))
                (,fn (varjo:add-external-function ',name ',in-args ',uniforms ',body))
                (,spec (get-function-spec ,fn)))
           (store-function-dependencies ,spec ,deps)
           (funcall (modify-hook *shader-info*)
                    (compute-outdated-programs ,spec))
           ,fn)))))
