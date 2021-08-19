;; Load some systems that should always be available
#+clpm-client
(let ((clpm-client:*asdf-system-not-found-behavior* :install)
      (clpm-client:*context-diff-approval-method* t)
      (systems '("printv")))
  (clpm-client:install :systems systems)
  (dolist (system systems)
    (asdf:load-system system)))
