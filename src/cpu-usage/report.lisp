(in-package #:mfiano.scripts.cpu-usage)

;; The number of static characters a progress bar is composed of.
;;
;; [ ██████████████ ]
;; 12______________345
;;
;; NOTE: The bar itself does not contribute, as it varies based on the '--bar-width/-w' argument.
;; displayed.
(u:define-constant +static-length/bar+ 5)

;; The number of static characters a percentage is composed of, when rendered with '--show-bars/-b'
;; enabled.
;;
;;   1.23%
;; 123____
;;
;; NOTE: The decimal point does not contribute because when '--precision=0/-p0' is supplied, it is
;; not displayed.
;; NOTE: The digits after the decimal point do not contribute because they vary based on the
;; --precision/-p' argument.
;; NOTE: The '%' suffix does not contribute because when '--hide-suffix/-S' is supplied, it is not
;; displayed.
(u:define-constant +static-length/percentage+ 3)

;; Calculate the total length of a report by summing all static and dynamic character counts.
;; NOTE: This function is currently un-used, but seems like it'd be useful to have at some point.
(defun calculate-report-length ()
  (+ (if *arg-bars* (+ +static-length/bar+ *arg-bar-width*) 0)
      +static-length/percentage+
      (if *arg-suffix* 1 0)
      *arg-precision*
      ;; This part might not be so obvious; this adds 1 more to the sum if a decimal point needs to
      ;; be displayed.
      (if (plusp *arg-precision*) 1 0)))

;; This prints a progress bar using varying widths of unicode vertical bars, to give a smoother
;; animating appearance, instead of the choppy look you get with full-width character 'frames'.
;; This tends to look better when the '--delay/-d' argument is low, as there could be a lot of
;; variance between larger delays. On the flip, side, a smaller delay means less time between two
;; samples used to calculate the percentage, which means more noisy/less accurate percentage
;; results. As such, the delay option needs to be manually balanced for the best user experience.
(defun print-progress-bar (percent)
  (u:mvlet* ((blocks "▏▎▍▌▋▊▉█")
             (covered (* (/ percent 100d0) *arg-bar-width*))
             (full partial (floor covered))
             (index (floor partial 1/8)))
    (format t "[ ~v,,,'█<~>~c~v,,,' <~>] "
            full
            (aref blocks index)
            (- *arg-bar-width* full))))

(defun format-percentage (usage)
  (let* (;; If progress bars are enabled, add some leading padding to account for the variable width
         ;; of a percentage (up to 3 digits to the left of the decimal point). This ensures that the
         ;; percentage is a fixed width, which is important if displaying multiple reports per line.
         ;; If progress bars are not enabled, this padding ensures that any numbers before the
         ;; decimal point are not left-truncated.
         (padding (+ (if *arg-bars* *arg-precision* 0) *arg-precision* 2))
         ;; Continue processing the percentage string by integrating the above leading padding along
         ;; with the user-specified precision for the digits to the right of the decimal point.
         (percent (format nil "~v,vf" padding *arg-precision* usage)))
    ;; When precision is 0, this removes the trailing decimal point.
    (string-right-trim '(#\.) percent)))

(defun print-percentage (usage)
  ;; Format the percentage number and write it to the stream.
  (write-string (format-percentage usage))
  ;; Emite the '%' suffix character if it should be shown.
  (when *arg-suffix*
    (write-char #\%)))

(defun print-report (usage)
  ;; If progress bars are to be shown, do so first.
  (when *arg-bars*
    (print-progress-bar usage))
  ;; Print the percentage after the progress bar, if any.
  (print-percentage usage)
  ;; Force displaying the stream.
  ;; TODO: We might want to use #'finish-output which waits for a sync first.
  (force-output)
  ;; Prepare the next line of output, depending on if replace mode is enabled or not.
  (if *arg-replace*
      ;; If replace mode is enabled, return the cursor to BOL, emit a number of spaces equal to the
      ;; terminal width in characters, and then return the cursor back to BOL. This effectively
      ;; clears the entire line so we don't end up with any artifacts on the next frame.
      (format t "~c~v,,,' <~>~c" #\return (get-column-count) #\return)
      ;; If replace mode is not enabled, emit a newline character if needed.
      (fresh-line)))
