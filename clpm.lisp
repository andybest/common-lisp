;; Load CLPM client
#-clpm-client
(when (asdf:find-system "clpm-client" nil)
  (asdf:load-system "clpm-client")
  (when (uiop:symbol-call :clpm-client '#:active-context)
    (uiop:symbol-call :clpm-client '#:activate-asdf-integration)))

;; Find a CLPM bundle
#+clpm-client
(defun :find-bundle (system-name &optional profile)
  (let ((system (asdf:find-system system-name nil)))
    (if system
        (let* ((clpm-file (format nil "clpm~@[-~(~a~)~]" profile))
               (path (asdf:system-relative-pathname system clpm-file)))
          (if (probe-file path)
              path
              (error "No bundle named ~a for system ~a." clpm-file system-name)))
        (error "System ~a does not exist." system-name))))

;; Activate a CLPM bundle
#+clpm-client
(defun :use-bundle (system-name &optional profile)
  (let ((path (:find-bundle system-name profile)))
    (clpm-client:activate-context path :activate-asdf-integration t)))

;; Install a CLPM bundle
#+clpm-client
(defun :install-bundle (system-name &optional profile)
  (let ((path (:find-bundle system-name profile)))
    (clpm-client:install :context path)))

;; Load an asdf system after activating its bundle
#+clpm-client
(defun :load-bundle (system &optional profile)
  (:use-bundle system profile)
  (asdf:load-system system))

;; Update the loose dependencies of the current CLPM bundle
#+clpm-client
(defun :update-bundle (&rest systems)
  (clpm-client:update :systems systems))


