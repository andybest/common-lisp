(in-package #:freebsd-tools.clcpu)

(defun get-cpu-ticks ()
  (bsd:with-sysctl-by-name (ptr "kern.cp_time")
    (c:foreign-array-to-lisp ptr '(:array :long 5) :element-type 'u:b64)))

;; Calculate the total length of a report by summing all static and dynamic character counts.
;; NOTE: This function is currently un-used, but seems like it'd be useful to have at some point.
(defun calculate-report-length ()
  (u:mvlet ((percentage-length suffix? (calculate-percentage-length)))
    (+ (calculate-progress-bar-length)
       percentage-length
       (if suffix? 1 0))))

(defun print-report (percentage)
  ;; If progress bars are to be shown, do so first.
  (when (lib:get-option 'bars)
    (print-progress-bar percentage))
  ;; Print the percentage after the progress bar, if any.
  (print-percentage percentage)
  ;; Finish the output for this frame.
  (finish-terminal-output))

(defun print-all-reports ()
  (when (zerop (lib:get-option 'count))
    (print-report 0))
  (loop :for sample1 = nil :then sample2
        :for sample2 = (get-cpu-ticks)
        :for report-count :from 0
        :when (plusp report-count)
          :do (let ((percentage (calculate-cpu-percentage sample1 sample2)))
                (print-report percentage)
                (when (eql report-count (lib:get-option 'count))
                  (loop-finish)))
        :do (sleep (lib:get-option 'delay))))
