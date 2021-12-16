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

;; Calculate the length in characters of a percentage.
(defun calculate-percentage-length ()
  (let ((precision (b:get-option 'precision)))
    (+ (if (b:get-option 'bars) +static-length/percentage+ 1)
       (if (plusp precision) (1+ precision) 0))))

;; Calculate the length in characters of a progress bar.
(defun calculate-progress-bar-length ()
  (if (b:get-option 'bars)
      (+ +static-length/bar+ (b:get-option 'bar-width))
      0))

;; Calculate the total length of a report by summing all static and dynamic character counts.
;; NOTE: This function is currently un-used, but seems like it'd be useful to have at some point.
(defun calculate-report-length ()
  (+ (calculate-progress-bar-length)
     (calculate-percentage-length)))

(defun print-progress-bar/head (percent)
  (if (b:get-option 'color)
      (format t "[~{~a~^;~}m[ [0m[~{~a~^;~}m"
              (b:get-option 'bar-color-base)
              (cond
                ((>= percent 200/3)
                 (b:get-option 'bar-color-high))
                ((>= percent 100/3)
                 (b:get-option 'bar-color-medium))
                (t
                 (b:get-option 'bar-color-low))))
      (write-string "[ ")))

(defun print-progress-bar/tail ()
  (if (b:get-option 'color)
      (format t "[0m[~{~a~^;~}m][0m " (b:get-option 'bar-color-base))
      (write-string "] ")))

;; This prints a progress bar using varying widths of unicode vertical bars, to give a smoother
;; animating appearance, instead of the choppy look you get with full-width character 'frames'.
;; This tends to look better when the '--delay/-d' argument is low, as there could be a lot of
;; variance between larger delays. On the flip side, a smaller delay means less time between two
;; samples used to calculate the percentage, which means more noisy/less accurate percentage
;; results. As such, the delay option needs to be manually balanced for the best user experience.
(defun print-progress-bar (percent)
  (u:mvlet* ((width (b:get-option 'bar-width))
             (covered (* (/ percent 100d0) width))
             (full partial (floor covered))
             (remaining (- width full))
             (char (aref "▏▎▍▌▋▊▉█" (floor partial 1/8))))
    (print-progress-bar/head percent)
    (format t "~v,,,'█<~>~c~v,,,' <~>" full char remaining)
    (print-progress-bar/tail)))

(defun format-percentage (usage)
  (let* ((precision (b:get-option 'precision))
         ;; If progress bars are enabled, add some leading padding to account for the variable width
         ;; of a percentage (up to 3 digits to the left of the decimal point). This ensures that the
         ;; percentage is a fixed width, which is important if displaying multiple reports per line.
         ;; If progress bars are not enabled, this padding ensures that any numbers before the
         ;; decimal point are not left-truncated.
         (padding (calculate-percentage-length))
         ;; Continue processing the percentage string by integrating the above leading padding along
         ;; with the user-specified precision for the digits to the right of the decimal point.
         (percent (format nil "~v,vf" padding precision usage)))
    ;; When precision is 0, this removes the trailing decimal point.
    (string-right-trim '(#\.) percent)))

(defun print-percentage (usage)
  ;; Format the percentage number and write it to the stream.
  (write-string (format-percentage usage))
  ;; Emite the '%' suffix character if it should be shown.
  (when (b:get-option 'suffix)
    (write-char #\%)))

(defun print-report (usage)
  ;; If progress bars are to be shown, do so first.
  (when (b:get-option 'bars)
    (print-progress-bar usage))
  ;; Print the percentage after the progress bar, if any.
  (print-percentage usage)
  ;; Force displaying the stream.
  ;; TODO: We might want to use #'finish-output which waits for a sync first.
  (force-output)
  ;; Prepare the next line of output, depending on if replace mode is enabled or not.
  (if (b:get-option 'replace)
      ;; If replace mode is enabled, return the cursor to BOL, emit a number of spaces equal to the
      ;; terminal width in characters, and then return the cursor back to BOL. This effectively
      ;; clears the entire line so we don't end up with any artifacts on the next frame.
      (format t "~c~v,,,' <~>~c" #\return (get-column-count) #\return)
      ;; If replace mode is not enabled, emit a newline character if needed.
      (fresh-line)))
