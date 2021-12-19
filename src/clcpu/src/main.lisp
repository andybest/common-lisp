(in-package #:freebsd-tools.clcpu)

(defun run (&rest options)
  (lib:with-options (*ui* options)
    (when (lib:get-option 'bars)
      (check-progress-bar-length))
    (prepare-terminal-output 0)
    (print-all-reports)))

(defun app ()
  (lib:run-non-interactively #'run))
