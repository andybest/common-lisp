(in-package #:mfiano.cmd.freebsd.lib)

(u:define-constant +progress-bar-padding+ 5)

(defun get-progress-bar-length (enabled fill-width)
  (if enabled
      (+ +progress-bar-padding+ fill-width)
      0))

(defun get-max-progress-bar-length (&key (left-padding 0) (right-padding 0))
  (- (get-terminal-column-count)
     left-padding
     +progress-bar-padding+
     right-padding))

(defun get-progress-bar-fill-color (value max &optional low medium high)
  (when (and low medium high)
    (cond
      ((<= 0 value (* max 1/3))
       low)
      ((< (* max 1/3) value (* max 2/3))
       medium)
      ((< (* max 2/3) value)
       high))))

(defun print-progress-bar (value max width &key base low medium high)
  (u:mvlet* ((full partial (floor (* (/ value max) width)))
             (char (aref "▏▎▍▌▋▊▉█" (floor partial 1/8)))
             (fill (get-progress-bar-fill-color value max low medium high))
             (color-enabled (and base fill)))
    (when color-enabled
      (print-color-code base))
    (write-string "[ ")
    (when color-enabled
      (reset-display-attributes)
      (print-color-code fill))
    (repeat-character #\full_block full)
    (write-char char)
    (repeat-character #\space (- width full))
    (when color-enabled
      (reset-display-attributes)
      (print-color-code base))
    (write-string "] ")
    (when color-enabled
      (reset-display-attributes))))
