(in-package #:freebsd-tools.clmem)

(setf lib:*program-name* "clmem")
(setf lib:*version* "0.1.0")
(setf lib:*authors* '("Michael Fiano"))
(setf lib:*initial-year* 2021)

(lib:define-option precision
  :parameter "digits"
  :short #\p
  :initial-value 1
  :key #'lib:parse-integer
  :reduce #'ui:last
  :validity-check (lib:in-range 0 8)
  :validity-error "must be an integer between 0 and 8, inclusive."
  :help "Display 'digits' number of digits for the fractional component of a value. (default: 1)")

(lib:define-boolean-options suffixes
  :long "show-suffixes"
  :long-no "hide-suffixes"
  :short #\s
  :initial-value t
  :help "Enable displaying of unit suffixes. This is the default."
  :help-no "Disable displaying of unit suffixes. The default is to display suffixes as if by ~
            '--show-suffixes'.")

(lib:define-boolean-options verbose
  :short #\v
  :help "Enable verbose output, instead of displaying only the percentage of memory used. This ~
         option is disabled by default."
  :help-no "Disable verbose output, displaying only the percentage of memory used. This is the ~
            default.")

(ui:define-string *help-text*
  "The clmem utility retrieves the current total system memory usage.")

(defparameter *ui*
  (ui:make-interface
   :name lib:*program-name*
   :summary "Display the current memory usage."
   :usage "[options]"
   :help *help-text*
   :contents (list *option-precision*
                   *option-suffixes*
                   *option-no-suffixes*
                   *option-verbose*
                   lib:*option-help*
                   lib:*option-version*)))
