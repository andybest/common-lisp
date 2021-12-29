;; Bootstrap Quicklisp
#-quicklisp
(let ((quicklisp-init (merge-pathnames ".quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Enable versose output when loading systems
#+quicklisp (setf ql:*quickload-verbose* t)

;; Convenience function to load Quicklisp projects
#+quicklisp
(defun :load (&rest systems)
  (ql:update-all-dists)
  (when systems
    (ql:quickload systems :force :all)))

;; Load some systems that should always be available
#+quicklisp
(let ((systems '(:mfiano.misc.utils
                 :printv)))
  (apply #':load systems)
  #+sbcl
  (sb-ext:add-package-local-nickname '#:u '#:mfiano.misc.utils '#:cl-user))
