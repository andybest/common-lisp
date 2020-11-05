(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.shadow
  (:local-nicknames
   (#:u #:net.mfiano.lisp.golden-utils)
   (#:m2 #:net.mfiano.lisp.origin.mat2)
   (#:m3 #:net.mfiano.lisp.origin.mat3)
   (#:m4 #:net.mfiano.lisp.origin.mat4)
   (#:v2 #:net.mfiano.lisp.origin.vec2)
   (#:v3 #:net.mfiano.lisp.origin.vec3)
   (#:v4 #:net.mfiano.lisp.origin.vec4))
  (:use #:cl)
  (:export
   #:bind-block
   #:bind-buffer
   #:buffer-name
   #:clear-buffer
   #:create-block-alias
   #:create-buffer
   #:define-shader
   #:defmacro
   #:defstruct
   #:defun
   #:delete-buffer
   #:find-block
   #:find-buffer
   #:find-program
   #:get-program-id
   #:load-shaders
   #:read-buffer-path
   #:recompile-shaders
   #:unbind-block
   #:unbind-buffer
   #:unload-shaders
   #:uniforms
   #:uniform-bool
   #:uniform-bool-array
   #:uniform-float
   #:uniform-float-array
   #:uniform-int
   #:uniform-int-array
   #:uniform-mat2
   #:uniform-mat2-array
   #:uniform-mat3
   #:uniform-mat3-array
   #:uniform-mat4
   #:uniform-mat4-array
   #:uniform-vec2
   #:uniform-vec2-array
   #:uniform-vec3
   #:uniform-vec3-array
   #:uniform-vec4
   #:uniform-vec4-array
   #:view-source
   #:with-shader
   #:write-buffer-path))

(defpackage #:net.mfiano.lisp.shadow.glsl
  (:local-nicknames
   (#:s #:net.mfiano.lisp.shadow)
   (#:u #:net.mfiano.lisp.golden-utils))
  (:use #:cl #:vari)
  (:shadow
   #:defun
   #:defstruct
   #:defmacro)
  ;; export external CL and VARI symbols
  #.(cons
     :export
     (flet ((find-symbols (&rest packages)
              (let (symbols)
                (dolist (package packages)
                  (do-external-symbols (x package)
                    (push x symbols)))
                (nreverse symbols))))
       (loop :for symbol :in (find-symbols '#:cl '#:vari)
             :unless (member (symbol-name symbol)
                             '("DEFUN" "DEFSTRUCT" "DEFMACRO"))
               :collect symbol)))
  (:export
   #:defun
   #:defstruct
   #:defmacro
   #:define-shader))
