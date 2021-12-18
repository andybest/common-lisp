(in-package #:freebsd-tools.cpu)

;; The number of static characters a progress bar is composed of.
;;
;; [ ██████████████ ]
;; 12______________345
;;
;; NOTE: The bar itself does not contribute, as it varies based on the '--bar-width/-w' argument.
;; displayed.
(u:define-constant +static-length/progress-bar+ 5)

(defun check-progress-bar-length ()
  (when (lib:get-option 'bars)
    (if lib:*interactive*
        (warn "Not checking progress bar length in interactive development environment.")
        (let ((column-count (get-terminal-column-count))
              (report-length (calculate-report-length))
              (max-bar-length (calculate-progress-bar-length/maximum)))
          (when (> report-length column-count)
            (lib:user-error "The value of '--bar-width/-w' (~d) is too wide to fit on the terminal ~
                             (maximum allowed: ~d)"
                            (lib:get-option 'bar-width)
                            max-bar-length))))))

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
