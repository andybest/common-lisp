;; Show current package in REPL prompt
#+sbcl
(let ((last-package nil)
      (cached-prompt nil))
  (flet ((package-prompt (stream)
           (unless (eq last-package *package*)
             (setf cached-prompt (format nil "~%~a> "
                                         (or (first (package-nicknames *package*))
                                             (package-name *package*)))
                   last-package *package*))
           (terpri)
           (princ cached-prompt stream)))
    (setf sb-int:*repl-prompt-fun* #'package-prompt)))

;; Set maximum DEBUG and SAFETY optimizations
#+sbcl (sb-ext:restrict-compiler-policy 'debug 3 3)
#+sbcl (sb-ext:restrict-compiler-policy 'safety 3 3)

;; Error when redefining a package with exported symbol variance, giving a restart to unexport them
#+sbcl (setf sb-ext:*on-package-variance* '(:warn nil :error t))

;; Load support for the statistical profiler
#+sbcl (require :sb-sprof)
