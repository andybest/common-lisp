(in-package #:mfiano.cmd.freebsd.clmem)

(defun run (&rest options)
  (lib:with-options (*ui* options)
    (print-report)))

(defun app ()
  (lib:run-non-interactively #'run))
