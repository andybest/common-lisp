(in-package :shadow)

(defun find-gpu-function (func-spec)
  (destructuring-bind (name . types) func-spec
    (find types (varjo.internals::get-external-function-by-name name nil)
          :key (lambda (x) (mapcar #'second (varjo.internals:in-args x)))
          :test #'equal)))

(defun get-function-spec (function)
  (cons (varjo:name function) (mapcar #'second (varjo.internals:in-args function))))

(defun store-function-dependencies (function dependencies)
  (let ((spec (get-function-spec function)))
    (symbol-macrolet ((fn-deps (au:href (dependencies *shader-info*) :fn->fn-deps spec)))
      (setf fn-deps (au:dict #'equal))
      (dolist (dep dependencies)
        (let ((dep-spec (get-function-spec dep)))
          (setf (au:href fn-deps dep-spec) dep-spec))))))
