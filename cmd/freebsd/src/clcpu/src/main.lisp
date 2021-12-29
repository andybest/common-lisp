(in-package #:mfiano.cmd.freebsd.clcpu)

(defun run (&rest options)
  (lib:with-options (*ui* options)
    (when (lib:get-option 'bar-enabled)
      (check-progress-bar-length))
    (lib:prepare-terminal-output 0 :replace (lib:get-option 'replace-enabled))
    (print-all-reports)))

(defun app ()
  (lib:run-non-interactively #'run))
