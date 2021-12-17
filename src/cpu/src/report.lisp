(in-package #:freebsd-tools.cpu)

;; The number of static characters a progress bar is composed of.
;;
;; [ ██████████████ ]
;; 12______________345
;;
;; NOTE: The bar itself does not contribute, as it varies based on the '--bar-width/-w' argument.
;; displayed.
(u:define-constant +static-length/progress-bar+ 5)

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
  (let* ((precision (lib:get-option 'precision))
         (padding (+ (if (lib:get-option 'bars) +static-length/percentage+ 1)
                     (if (plusp precision) (1+ precision) 0))))
    (values padding
            (lib:get-option 'suffix))))

;; Calculate the length in characters of a progress bar.
(defun calculate-progress-bar-length ()
  (if (lib:get-option 'bars)
      (+ +static-length/progress-bar+ (lib:get-option 'bar-width))
      0))

;; Calculate the maximum allowed progress bar length that will fit on a terminal line.
(defun calculate-progress-bar-length/maximum ()
  (u:mvlet ((percentage-length suffix? (calculate-percentage-length)))
    (- (get-terminal-column-count)
       +static-length/progress-bar+
       (calculate-percentage-length)
       (if suffix? 1 0))))

;; Calculate the total length of a report by summing all static and dynamic character counts.
;; NOTE: This function is currently un-used, but seems like it'd be useful to have at some point.
(defun calculate-report-length ()
  (u:mvlet ((percentage-length suffix? (calculate-percentage-length)))
    (+ (calculate-progress-bar-length)
       percentage-length
       (if suffix? 1 0))))

;; This prints a progress bar using varying widths of unicode vertical bars, to give a smoother
;; animating appearance, instead of the choppy look you get with full-width character 'frames'.
;; This tends to look better when the '--delay/-d' argument is low, as there could be a lot of
;; variance between larger delays. On the flip side, a smaller delay means less time between two
;; samples used to calculate the percentage, which means more noisy/less accurate percentage
;; results. As such, the delay option needs to be manually balanced for the best user experience.
(defun print-progress-bar (percent)
  (u:mvlet* ((width (lib:get-option 'bar-width))
             (full partial (floor (* (/ percent 100d0) width)))
             (char (aref "▏▎▍▌▋▊▉█" (floor partial 1/8)))
             (color-enabled (lib:get-option 'color-enabled))
             (base-color (lib:get-option 'bar-color-base))
             (fill-color (cond
                           ((< 0 percent 100/3)
                            (lib:get-option 'bar-color-low))
                           ((< 100/3 percent 200/3)
                            (lib:get-option 'bar-color-medium))
                           ((< 200/3 percent)
                            (lib:get-option 'bar-color-high)))))
    (when color-enabled
      (print-color-code base-color))
    (write-string "[ ")
    (when color-enabled
      (reset-display-attributes)
      (print-color-code fill-color))
    (repeat-character #\full_block full)
    (write-char char)
    (repeat-character #\space (- width full))
    (when color-enabled
      (reset-display-attributes)
      (print-color-code base-color))
    (write-string "] ")
    (when color-enabled
      (reset-display-attributes))))

(defun print-percentage (percentage)
  (u:mvlet* ((padding suffix? (calculate-percentage-length))
             (precision (lib:get-option 'precision))
             (string (string-right-trim '(#\.) (format nil "~v,vf" padding precision percentage))))
    ;; Write the formatted percentage value to the stream.
    (write-string string)
    ;; Emite the '%' suffix character if it should be shown.
    (when suffix?
      (write-char #\%))))

(defun print-report (percentage)
  ;; If progress bars are to be shown, do so first.
  (when (lib:get-option 'bars)
    (print-progress-bar percentage))
  ;; Print the percentage after the progress bar, if any.
  (print-percentage percentage)
  ;; Finish the output for this frame.
  (finish-terminal-output))
