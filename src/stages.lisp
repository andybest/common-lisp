(in-package :shadow)

(defun stage-type (stage)
  (varjo.internals::stage-obj-to-name stage))

(defun stage-type->shader-type (stage-type)
  (ecase stage-type
    (:vertex :vertex-shader)
    (:tessellation-control :tess-control-shader)
    (:tessellation-evaluation :tess-evaluation-shader)
    (:geometry :geometry-shader)
    (:fragment :fragment-shader)
    (:compute :compute-shader)))

(defun make-stage (version primitive stage-spec)
  (destructuring-bind (stage-type func-spec) stage-spec
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

(defun generate-implicit-uniform-hook (program)
  (lambda (symbol)
    (au:if-found (uniform (au:href (uniforms program) symbol))
                 (au:href uniform :type)
                 (error "The symbol ~s is not defined as a uniform for program ~s."
                        symbol
                        (name program)))))

(defun translate-stages (program version primitive stage-specs)
  (varjo:with-constant-inject-hook #'lisp-constant->glsl-constant
    (varjo:with-stemcell-infer-hook (funcall #'generate-implicit-uniform-hook program)
      (varjo:rolling-translate
       (mapcar
        (lambda (x) (make-stage version primitive x))
        stage-specs)))))
