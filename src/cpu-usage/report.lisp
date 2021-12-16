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
;; The length returned only includes the numeric portion (everything except for the '%' suffix).
;; A second return value indicating whether the additional '%' suffix character is enabled.
(defun calculate-percentage-length ()
  (let* ((precision (b:get-option 'precision))
         (padding (+ (if (b:get-option 'bars) +static-length/percentage+ 1)
                     (if (plusp precision) (1+ precision) 0))))
    (values padding
            (b:get-option 'suffix))))

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

;; This prints a progress bar using varying widths of unicode vertical bars, to give a smoother
;; animating appearance, instead of the choppy look you get with full-width character 'frames'.
;; This tends to look better when the '--delay/-d' argument is low, as there could be a lot of
;; variance between larger delays. On the flip side, a smaller delay means less time between two
;; samples used to calculate the percentage, which means more noisy/less accurate percentage
;; results. As such, the delay option needs to be manually balanced for the best user experience.
(defun print-progress-bar (percent)
  (u:mvlet* ((color (b:get-option 'color-enabled))
             (base-color (b:get-option 'bar-color-base))
             (width (b:get-option 'bar-width))
             (full partial (floor (* (/ percent 100d0) width)))
             (char (aref "▏▎▍▌▋▊▉█" (floor partial 1/8))))
    (flet ((print-progress-bar/head ()
             (if color
                 (format t "[~{~a~^;~}m[ [0m[~{~a~^;~}m"
                         base-color
                         (cond
                           ((<= 0 percent 100/3)
                            (b:get-option 'bar-color-low))
                           ((<= 100/3 percent 200/3)
                            (b:get-option 'bar-color-medium))
                           ((<= 200/3 percent)
                            (b:get-option 'bar-color-high))))
                 (write-string "[ ")))
           (print-progress-bar/tail ()
             (if color
                 (format t "[0m[~{~a~^;~}m][0m " base-color)
                 (write-string "] "))))
      (print-progress-bar/head)
      (format t "~v,,,'█<~>~c~v,,,' <~>" full char (- width full))
      (print-progress-bar/tail))))

(defun print-percentage (usage)
  (u:mvlet* ((padding suffix? (calculate-percentage-length))
             (precision (b:get-option 'precision))
             (percentage (string-right-trim '(#\.) (format nil "~v,vf" padding precision usage))))
    ;; Write the formatted percentage value to the stream.
    (write-string percentage)
    ;; Emite the '%' suffix character if it should be shown.
    (when suffix?
      (write-char #\%))))

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
      (format t "~c~v,,,' <~>~c" #\return (get-terminal-column-count) #\return)
      ;; If replace mode is not enabled, emit a newline character if needed.
      (fresh-line)))
