(in-package #:mfiano.scripts.cpu-usage)

(setf base:*program-name* "cpu-usage")
(setf base:*version* "0.1.0")
(setf base:*authors* '("Michael Fiano"))
(setf base:*initial-year* 2021)

(base:define-option count
  :parameter "count"
  :short #\c
  :initial-value 1
  :key #'base:parse-integer
  :reduce #'ui:last
  :validity-check (u:disjoin #'u:non-negative-integer-p)
  :validity-error "must be a non-negative integer."
  :help "Repeat printing a report 'count' times.~%~
         'count' can be either a positive integer, or 0 to repeat infinitely. (default: 1)")

(base:define-boolean-options color
  :long "enable-color"
  :long-no "disable-color"
  :help "Enable colored progress bars. This option is disabled by default."
  :help-no "Disable colored progress bars. This is the default.")

(base:define-option delay
  :parameter "delay"
  :short #\d
  :initial-value 0.25
  :key #'base:parse-float
  :reduce #'ui:last
  :validity-check (u:disjoin #'null #'u:positive-real-p)
  :validity-error "must be a positive floating-point number."
  :help "Wait 'delay' seconds between each pair of samples taken to produce a report.~%~
         (default: 0.25)")

(base:define-option precision
  :parameter "digits"
  :short #\p
  :initial-value 1
  :key #'base:parse-integer
  :reduce #'ui:last
  :validity-check (base:in-range 0 8)
  :validity-error "must be an integer between 0 and 8, inclusive."
  :help "Display 'digits' number of digits for the fractional component of a percentage. ~
        (default: 1)")

(base:define-boolean-options bars
  :long "show-bars"
  :long-no "hide-bars"
  :short #\b
  :help "Display progress bars in reports. This option is disabled by default."
  :help-no "Do not display progress bars in reports. This is the default.")

(base:define-option bar-width
  :parameter "width"
  :long "bar-width"
  :short #\w
  :initial-value 20
  :key #'base:parse-integer
  :reduce #'ui:last
  :validity-check (base:in-range 1 255)
  :validity-error "must be an integer between 1 and 255, inclusive."
  :help "The width of progress bars to render when the '-b/--show-bars' option is specified. ~
         (default: 20)")

(base:define-option bar-color-base
  :parameter "fg[,bg]"
  :initial-value '(90 49)
  :key #'parse-color-pair
  :reduce #'ui:last
  :help "The foreground, and optionally background color to use for the trim of progress bars. ~
         Both '--enable-color' and '--show-bars' must be supplied to have any effect. Valid colors ~
         are integers between 0 and 15, which map to the ANSI 16-color pallete (8-15 are bright ~
         variants of 0-7). (default: 8)")

(base:define-option bar-color-low
  :parameter "fg[,bg]"
  :initial-value '(32 49)
  :key #'parse-color-pair
  :reduce #'ui:last
  :help "The foreground, and optionally background color to use for the trim of progress bars when ~
         their fill is low. Both '--enable-color' and '--show-bars' must be supplied to have any ~
         effect. Valid colors are integers between 0 and 15, which map to the ANSI 16-color ~
         pallete (8-15 are bright variants of 0-7). (default: 2)")

(base:define-option bar-color-medium
  :parameter "fg[,bg]"
  :initial-value '(33 49)
  :key #'parse-color-pair
  :reduce #'ui:last
  :help "The foreground, and optionally background color to use for the trim of progress bars when ~
         their fill is medium. Both '--enable-color' and '--show-bars' must be supplied to have ~
         any effect. Valid colors are integers between 0 and 15, which map to the ANSI 16-color ~
         pallete (8-15 are bright variants of 0-7). (default: 3)")

(base:define-option bar-color-high
  :parameter "fg[,bg]"
  :initial-value '(31 49)
  :key #'parse-color-pair
  :reduce #'ui:last
  :help "The foreground, and optionally background color to use for the trim of progress bars when ~
         their fill is high. Both '--enable-color' and '--show-bars' must be supplied to have any ~
         effect. Valid colors are integers between 0 and 15, which map to the ANSI 16-color ~
         pallete (8-15 are bright variants of 0-7). (default: 1)")

(base:define-boolean-options replace
  :long "replace"
  :short #\r
  :help "Enable replacing the old report with each subsequent report, instead of appending new ~
         output lines. The default is to append as if by '--no-replace'."
  :help-no "Disable replacing the old report with each subsequent report. This is the default.")

(base:define-boolean-options suffix
  :long "show-suffix"
  :long-no "hide-suffix"
  :short #\s
  :initial-value t
  :help "Enable displaying of the suffix '%'. This is the default."
  :help-no "Disable displaying of the suffix '%'. The default is to display the suffix as if by ~
            '--show-suffix'.")

(ui:define-string *help-text*
  "Help text")

(defparameter *ui*
  (ui:make-interface
   :name base:*program-name*
   :summary "Display the current CPU usage."
   :usage "[options]"
   :help *help-text*
   :contents (list *option-bars*
                   *option-no-bars*
                   *option-bar-width*
                   *option-bar-color-base*
                   *option-bar-color-low*
                   *option-bar-color-medium*
                   *option-bar-color-high*
                   *option-color*
                   *option-count*
                   *option-delay*
                   *option-precision*
                   *option-replace*
                   *option-no-replace*
                   *option-suffix*
                   *option-no-suffix*
                   base:*option-help*
                   base:*option-version*)))
