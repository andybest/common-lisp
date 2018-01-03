(in-package :defpackage+-1)

;; Remove this when defpackage-plus PR #2 is merged in.
(eval-when (:compile-toplevel :load-toplevel)
  (defun ensure-global-nickname (package nickname)
    (let ((package (find-package package)))
      (rename-package package
                      (package-name package)
                      (adjoin nickname
                              (package-nicknames package)
                              :key #'string
                              :test #'equal))))

  (defun add-local-nickname (package nickname local-to)
    (declare (ignorable package nickname local-to))
    #+sbcl
    (sb-ext:add-package-local-nickname nickname package local-to)
    #+(or abcl ecl)
    (ext:add-package-local-nickname nickname package local-to))

  (defmethod defpackage+-dispatch ((option (eql :local-nicknames))
                                   parameters package)
    (loop :for (nickname package-name) :in parameters
          :do (progn
                (ensure-package package-name)
                #+package-local-nicknames
                (add-local-nickname package-name nickname package)
                #-package-local-nicknames
                (ensure-global-nickname package-name nickname)))))

(defpackage+ #:box.math.vari
  (:local-nicknames (#:v2 #:box.math.vec2)
                    (#:v3 #:box.math.vec3)
                    (#:v4 #:box.math.vec4)
                    (#:m2 #:box.math.mat2)
                    (#:m3 #:box.math.mat3)
                    (#:m4 #:box.math.mat4))
  (:inherit #:cl
            #:vari)
  (:import-from #:varjo
                #:v-def-glsl-template-fun
                #:v-float
                #:v-vec2
                #:v-vec3
                #:v-vec4
                #:v-mat4)
  (:import-from #:box.math.base
                #:%swizzle/component-groups))

(in-package :box.math.vari)

;;; vec2

(v-def-glsl-template-fun v2:vec (x) "vec2(~a)" (v-float) v-vec2 :pure t)
(v-def-glsl-template-fun v2:vec (x y) "vec2(~a,~a)" (v-float v-float) v-vec2
                         :pure t)

;;; vec3

(v-def-glsl-template-fun v3:vec (x) "vec3(~a)" (v-float) v-vec3 :pure t)
(v-def-glsl-template-fun v3:vec (x y) "vec3(~a,~a)" (v-float v-vec2) v-vec3
                         :pure t)
(v-def-glsl-template-fun v3:vec (x y) "vec3(~a,~a)" (v-vec2 v-float) v-vec3
                         :pure t)
(v-def-glsl-template-fun v3:vec (x y z) "vec3(~a,~a,~a)"
                         (v-float v-float v-float) v-vec3 :pure t)

;;; vec4

(v-def-glsl-template-fun v4:vec (x) "vec4(~a)" (v-float) v-vec4 :pure t)
(v-def-glsl-template-fun v4:vec (x y) "vec4(~a,~a)" (v-vec2 v-vec2) v-vec4
                         :pure t)
(v-def-glsl-template-fun v4:vec (x y) "vec4(~a,~a)" (v-float v-vec3) v-vec4
                         :pure t)
(v-def-glsl-template-fun v4:vec (x y) "vec4(~a,~a)" (v-vec3 v-float) v-vec4
                         :pure t)
(v-def-glsl-template-fun v4:vec (x y z) "vec4(~a,~a,~a)"
                         (v-vec2 v-float v-float) v-vec4 :pure t)
(v-def-glsl-template-fun v4:vec (x y z) "vec4(~a,~a,~a)"
                         (v-float v-vec2 v-float) v-vec4 :pure t)
(v-def-glsl-template-fun v4:vec (x y z) "vec4(~a,~a,~a)"
                         (v-float v-float v-vec2) v-vec4 :pure t)
(v-def-glsl-template-fun v4:vec (x y z w) "vec4(~a,~a,~a,~a)"
                         (v-float v-float v-float v-float) v-vec4 :pure t)

;;; mat2

(v-def-glsl-template-fun m2:matrix (a b c d) "mat2(~a,~a,~a,~a)"
                         (v-float v-float v-float v-float) v-mat2 :pure t)
(v-def-glsl-template-fun m2:matrix (a b) "mat2(~a,~a)" (v-vec2 v-vec2) v-mat2
                         :pure t)

;;; mat3

(v-def-glsl-template-fun m3:matrix (a b c d e f) "mat3(~a,~a,~a,~a,~a,~a)"
                         (v-float v-float v-float v-float v-float v-float)
                         v-mat3 :pure t)
(v-def-glsl-template-fun m3:matrix (a b c) "mat3(~a,~a,~a)"
                         (v-vec3 v-vec3 v-vec3) v-mat3 :pure t)

;;; mat4

(v-def-glsl-template-fun m4:matrix (a b c d e f g h i j k l m n o p)
                         "mat4(~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a)"
                         (v-float v-float v-float v-float v-float v-float
                                  v-float v-float v-float v-float v-float
                                  v-float v-float v-float v-float v-float)
                         v-mat4 :pure t)
(v-def-glsl-template-fun m4:matrix (a b c d) "mat4(~a,~a,~a,~a)"
                         (v-vec4 v-vec4 v-vec4 v-vec4) v-mat4 :pure t)

;;; swizzling

(defmacro define-varjo-swizzle-macros (size)
  `(progn
     ,@(loop :for masks :in (%swizzle/component-groups size)
             :collect `(v-defmacro ,(alexandria:symbolicate "." masks) (vector)
                         `(swizzle ,vector ,,(alexandria:make-keyword masks))))))

(define-varjo-swizzle-macros 2)
(define-varjo-swizzle-macros 3)
(define-varjo-swizzle-macros 4)
