(in-package #:cl-user)

(defpackage #:shadow
  (:local-nicknames
   (#:math #:gfxmath)
   (#:u #:mfiano-utils))
  (:use #:cl)
  (:export
   #:bind-block
   #:bind-buffer
   #:buffer-name
   #:build-shader-dictionary
   #:build-shader-program
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
   #:find-shader-definition
   #:get-program-id
   #:load-shaders
   #:program
   #:read-buffer-path
   #:recompile-shaders
   #:set-uniform
   #:unbind-block
   #:unbind-buffer
   #:unload-shaders
   #:uniforms
   #:view-source
   #:with-shader
   #:write-buffer-path))

(defpackage #:shadow.glsl
  (:local-nicknames
   (#:s #:shadow)
   (#:u #:mfiano-utils))
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
