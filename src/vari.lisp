(defpackage #:gamebox-math.vari
  (:use #:cl
        #:alexandria
        #:varjo)
  (:shadowing-import-from #:gamebox-math
                          #:vec2
                          #:vec3
                          #:vec4)
  (:import-from #:gamebox-math
                #:%swizzle/component-groups))

(in-package :gamebox-math.vari)

;;; vec2

(v-def-glsl-template-fun vec2 (x) "vec2(~a)" (v-float) v-vec2 :pure t)
(v-def-glsl-template-fun vec2 (x y) "vec2(~a,~a)" (v-float v-float) v-vec2
                         :pure t)

;;; vec3

(v-def-glsl-template-fun vec3 (x) "vec3(~a)" (v-float) v-vec3 :pure t)
(v-def-glsl-template-fun vec3 (x y) "vec3(~a,~a)" (v-float v-vec2) v-vec3
                         :pure t)
(v-def-glsl-template-fun vec3 (x y) "vec3(~a,~a)" (v-vec2 v-float) v-vec3
                         :pure t)
(v-def-glsl-template-fun vec3 (x y z) "vec3(~a,~a,~a)" (v-float v-float v-float)
                         v-vec3 :pure t)

;;; vec4

(v-def-glsl-template-fun vec4 (x) "vec4(~a)" (v-float) v-vec4 :pure t)
(v-def-glsl-template-fun vec4 (x y) "vec4(~a,~a)" (v-vec2 v-vec2) v-vec4
                         :pure t)
(v-def-glsl-template-fun vec4 (x y) "vec4(~a,~a)" (v-float v-vec3) v-vec4
                         :pure t)
(v-def-glsl-template-fun vec4 (x y) "vec4(~a,~a)" (v-vec3 v-float) v-vec4
                         :pure t)
(v-def-glsl-template-fun vec4 (x y z) "vec4(~a,~a,~a)" (v-vec2 v-float v-float)
                         v-vec4 :pure t)
(v-def-glsl-template-fun vec4 (x y z) "vec4(~a,~a,~a)" (v-float v-vec2 v-float)
                         v-vec4 :pure t)
(v-def-glsl-template-fun vec4 (x y z) "vec4(~a,~a,~a)" (v-float v-float v-vec2)
                         v-vec4 :pure t)
(v-def-glsl-template-fun vec4 (x y z w) "vec4(~a,~a,~a,~a)"
                         (v-float v-float v-float v-float) v-vec4 :pure t)

;;; mat4

(v-def-glsl-template-fun mat4 (a b c d e f g h i j k l m n o p)
                         "mat4(~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a)"
                         (v-float v-float v-float v-float v-float v-float
                                  v-float v-float v-float v-float v-float
                                  v-float v-float v-float v-float v-float)
                         v-mat4 :pure t)
(v-def-glsl-template-fun mat4 (a b c d) "mat4(~a,~a,~a,~a)"
                         (v-vec4 v-vec4 v-vec4 v-vec4) v-mat4 :pure t)

;;; swizzling

(defmacro def-varjo-swizzle-macros ()
  (let ((macros (loop :for masks :in (%swizzle/component-groups)
                      :collect `(v-defmacro ,(symbolicate "." masks) (vector)
                                  `(vari:swizzle ,vector
                                                 ,,(make-keyword masks))))))
    `(progn ,@macros)))

(def-varjo-swizzle-macros)
