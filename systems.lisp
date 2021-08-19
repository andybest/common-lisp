;; Load some systems that should always be available
#+clpm-client
(let ((clpm-client:*asdf-system-not-found-behavior* :install)
      (clpm-client:*context-diff-approval-method* t)
      ;; FIXME: Needed until clpm issue #28 is resolved, which would allow us to query the clpmfile
      ;; for a list of systems.
      (systems '("printv")))
  (clpm-client:activate-context #p"/home/mfiano/Projects/Lisp/init/clpm"
                                :activate-asdf-integration t)
  (clpm-client:install)
  (dolist (system systems)
    (asdf:load-system system)))
