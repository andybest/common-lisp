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

(defpackage+ #:box.math.test
  (:local-nicknames (#:v2 #:box.math.vec2)
                    (#:v3 #:box.math.vec3)
                    (#:v4 #:box.math.vec4)
                    (#:m2 #:box.math.mat2)
                    (#:m3 #:box.math.mat3)
                    (#:m4 #:box.math.mat4)
                    (#:q #:box.math.quat)
                    (#:dq #:box.math.dquat))
  (:use #:cl
        #:prove))
