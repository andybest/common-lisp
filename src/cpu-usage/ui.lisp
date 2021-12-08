(in-package #:mfiano.scripts.cpu-usage)

(setf base:*program-name* "cpu-usage")
(setf base:*version* "0.1.0")
(setf base:*authors* '("Michael Fiano"))
(setf base:*initial-year* 2021)

(base:define-option count
  :short #\c
  :parameter "count"
  :key #'base:parse-integer
  :reduce #'ui:last
  :validity-check (u:disjoin #'null #'u:positive-integer-p)
  :validity-error "must be a positive integer."
  :help "Repeat printing of the report 'count' times.~%~
         The default value is 1 if -d/--delay is not specified, or infinity if it is.")

(base:define-option delay
  :short #\d
  :parameter "seconds"
  :key #'base:parse-float
  :reduce #'ui:last
  :validity-check (u:disjoin #'null #'u:positive-real-p)
  :validity-error "must be a positive floating-point number."
  :help "Wait 'delay' seconds between each report printed. (default: 0.2)")

(base:define-option precision
  :short #\p
  :parameter "digits"
  :initial-value 1
  :key #'base:parse-integer
  :reduce #'ui:last
  :validity-check (base:in-range 0 8)
  :validity-error "must be an integer between 0 and 8, inclusive."
  :help "Display 'digits' number of digits for the fractional component of a percentage. ~
        (default: 1)")

(base:define-boolean-options suffix
  :long "show-suffix"
  :long-no "hide-suffix"
  :short #\s
  :initial-value t
  :help "Enable displaying of the suffix '%'. This is enabled by default."
  :help-no "Disable displaying of the suffix '%'. The default is to display the suffix.")

(ui:define-string *help-text*
  "Help text")

(defparameter *ui*
  (ui:make-interface
   :name base:*program-name*
   :summary "Display the current CPU usage."
   :usage "[options]"
   :help *help-text*
   :contents (list *option-count*
                   *option-delay*
                   base:*option-help*
                   *option-precision*
                   *option-suffix*
                   *option-no-suffix*
                   base:*option-version*)))
