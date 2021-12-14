(in-package #:mfiano.scripts.cpu-usage)

;; This prints a progress bar using varying widths of unicode vertical bars, to give a smoother
;; animation illusion, instead of the choppy look you get with fixed-width character 'frames'.
(defun print-progress-bar (percent)
  (u:mvlet* ((blocks "▏▎▍▌▋▊▉█")
             (covered (* (/ percent 100d0) *arg-bar-width*))
             (full partial (floor covered))
             (index (floor partial 1/8)))
    (format t "~c[ ~v,,,'█<~>~c~v,,,' <~>] "
            #\return
            full
            (aref blocks index)
            (- *arg-bar-width* full))))

(defun format-percentage (usage)
  (let* (;; If progress bar is enabled, add some leading padding to account for the variable width
         ;; of a percentage (up to 3 digits to the left of the decimal point). This ensures that the
         ;; percentage is a fixed width, which is important if displaying multiple reports per line.
         (padding (if *arg-bars* (+ *arg-precision* 4) 0))
         ;; Continue processing the percentage string by integrating the above leading padding along
         ;; with the user-specified precision for the digits to the right of the decimal point.
         (percent (format nil "~v,vf" padding *arg-precision* usage)))
    ;; When precision is 0, this removes the trailing decimal point.
    (string-right-trim '(#\.) percent)))

(defun print-percentage (usage)
  (write-string (format-percentage usage))
  (when *arg-suffix*
    (write-char #\%))
  (cond
    ;; Line is overwritten each call in 'replace' mode, so we return the cursor to BOL.
    (*arg-replace*
     ;; Before returning the cursor to BOL, output precision+4 spaces, so we don't end up with an
     ;; extra '%' suffix when the next report is 1 less digit to the left of the decimal point.
     (format t "~v,,,' <~>~c" (+ *arg-precision* 4) #\return)
     (force-output))
    ;; A new line is written when not in 'replace' mode.
    (t
     (fresh-line)
     (finish-output))))

(defun print-report (usage)
  ;; If progress bars are to be shown, do so.
  (when *arg-bars*
    (print-progress-bar usage))
  ;; Print the percentage after the progress bar, if any.
  (print-percentage usage))
