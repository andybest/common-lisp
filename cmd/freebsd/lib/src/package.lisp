(in-package #:cl-user)

(defpackage #:mfiano.cmd.freebsd.lib
  (:local-nicknames
   (#:bsd #:mfiano.ffi.freebsd)
   (#:c #:cffi)
   (#:lt #:local-time)
   (#:pf #:parse-float)
   (#:u #:mfiano.misc.utils)
   (#:ui #:adopt)
   (#:wua #:with-user-abort))
  (:use #:cl)
  (:shadow
   #:parse-integer)
  ;; program interface
  (:export
   #:*authors*
   #:*initial-year*
   #:*interactive*
   #:*option-help*
   #:*option-version*
   #:*program-name*
   #:*version*
   #:define-boolean-options
   #:define-option
   #:get-option
   #:in-range
   #:parse-float
   #:parse-integer
   #:parse-options
   #:run-non-interactively
   #:string-match
   #:user-error
   #:user-error-message
   #:validate-option
   #:with-options)
  ;; terminal api
  (:export
   #:erase-down
   #:finish-terminal-output
   #:get-terminal-column-count
   #:move-cursor-up
   #:prepare-terminal-output
   #:print-color-code
   #:repeat-character
   #:reset-display-attributes
   #:restore-cursor
   #:save-cursor)
  ;; progress bar api
  (:export
   #:get-max-progress-bar-length
   #:get-progress-bar-length
   #:print-progress-bar))
