(in-package :defpackage+-1)

;; Remove this when defpackage-plus PR #2 and #3 is available with the next
;; Quicklisp dist so that my remote testing on TravisCI passes.
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
    #+package-local-nicknames
    (progn
      #+sbcl
      (sb-ext:add-package-local-nickname nickname package local-to)
      #+(or abcl ecl)
      (ext:add-package-local-nickname nickname package local-to)))

  (defmethod defpackage+-dispatch ((option (eql :local-nicknames)) parameters package)
    (loop :for (nickname package-name) :in parameters
          :do (progn
                (ensure-package package-name)
                #+package-local-nicknames
                (add-local-nickname package-name nickname package)
                #-package-local-nicknames
                (ensure-global-nickname package-name nickname)))))

(in-package :defpackage+-user-1)

(defpackage+ #:box.math.base
  (:inherit #:cl)
  (:export #:+epsilon+
           #:%make-accessor-symbol
           #:%~))
