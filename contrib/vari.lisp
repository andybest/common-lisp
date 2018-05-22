(in-package :defpackage+-user-1)

(defpackage+ #:box.math.vari
  (:inherit #:cl
            #:vari)
  (:import-from #:varjo
                #:v-def-glsl-template-fun
                #:v-float
                #:v-vec2
                #:v-vec3
                #:v-vec4
                #:v-mat4)
  (:import-from #:box.math.common
                #:%swizzle/component-groups
                #:%swizzle/char-position))

(in-package :box.math.vari)

;;; vec2

(v-def-glsl-template-fun v2:make (x) "vec2(~a)" (v-float) v-vec2 :pure t)
(v-def-glsl-template-fun v2:make (x y) "vec2(~a,~a)" (v-float v-float) v-vec2 :pure t)

;;; vec3

(v-def-glsl-template-fun v3:make (x) "vec3(~a)" (v-float) v-vec3 :pure t)
(v-def-glsl-template-fun v3:make (x y) "vec3(~a,~a)" (v-float v-vec2) v-vec3 :pure t)
(v-def-glsl-template-fun v3:make (x y) "vec3(~a,~a)" (v-vec2 v-float) v-vec3 :pure t)
(v-def-glsl-template-fun v3:make (x y z) "vec3(~a,~a,~a)" (v-float v-float v-float) v-vec3 :pure t)

;;; vec4

(v-def-glsl-template-fun v4:make (x) "vec4(~a)" (v-float) v-vec4 :pure t)
(v-def-glsl-template-fun v4:make (x y) "vec4(~a,~a)" (v-vec2 v-vec2) v-vec4 :pure t)
(v-def-glsl-template-fun v4:make (x y) "vec4(~a,~a)" (v-float v-vec3) v-vec4 :pure t)
(v-def-glsl-template-fun v4:make (x y) "vec4(~a,~a)" (v-vec3 v-float) v-vec4 :pure t)
(v-def-glsl-template-fun v4:make (x y z) "vec4(~a,~a,~a)" (v-vec2 v-float v-float) v-vec4 :pure t)
(v-def-glsl-template-fun v4:make (x y z) "vec4(~a,~a,~a)" (v-float v-vec2 v-float) v-vec4 :pure t)
(v-def-glsl-template-fun v4:make (x y z) "vec4(~a,~a,~a)" (v-float v-float v-vec2) v-vec4 :pure t)
(v-def-glsl-template-fun v4:make (x y z w) "vec4(~a,~a,~a,~a)" (v-float v-float v-float v-float)
                         v-vec4 :pure t)

;;; mat2

(v-def-glsl-template-fun m2:make (a b c d) "mat2(~a,~a,~a,~a)" (v-float v-float v-float v-float)
                         v-mat2 :pure t)
(v-def-glsl-template-fun m2:make (a b) "mat2(~a,~a)" (v-vec2 v-vec2) v-mat2 :pure t)

;;; mat3

(v-def-glsl-template-fun m3:make (a b c d e f g h i) "mat3(~a,~a,~a,~a,~a,~a,~a,~a,~a)"
                         (v-float v-float v-float v-float v-float v-float v-float v-float v-float)
                         v-mat3 :pure t)
(v-def-glsl-template-fun m3:make (a b c) "mat3(~a,~a,~a)" (v-vec3 v-vec3 v-vec3) v-mat3 :pure t)

;;; mat4

(v-def-glsl-template-fun m4:make (a b c d e f g h i j k l m n o p)
                         "mat4(~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a)"
                         (v-float v-float v-float v-float v-float v-float v-float v-float v-float
                                  v-float v-float v-float v-float v-float v-float v-float)
                         v-mat4 :pure t)
(v-def-glsl-template-fun m4:make (a b c d) "mat4(~a,~a,~a,~a)" (v-vec4 v-vec4 v-vec4 v-vec4)
                         v-mat4 :pure t)

;;; Swizzling

(defmacro define-vari-swizzle-macros ()
  (flet ((map-swizzle (mask)
           (au:make-keyword
            (map 'string
                 (lambda (x)
                   (elt "XYZW" (%swizzle/char-position mask (position x mask))))
                 mask))))
    `(progn
       ,@(loop :for mask :in (%swizzle/component-groups 4)
               :for op = (au:symbolicate "." mask)
               :collect `(export ',op)
               :collect `(v-defmacro ,op (vector) `(swizzle ,vector ,,(map-swizzle mask)))))))

(define-vari-swizzle-macros)
