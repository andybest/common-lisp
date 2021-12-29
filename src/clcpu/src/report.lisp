(in-package #:mfiano.cmd.freebsd.clcpu)

(u:define-constant +static-length/percentage+ 3)

(defun get-cpu-ticks ()
  (bsd:with-sysctl-by-name (ptr "kern.cp_time")
    (c:foreign-array-to-lisp ptr '(:array :long 5) :element-type 'u:b64)))

(defun get-percentage-length ()
  (let* ((precision (lib:get-option 'precision))
         (padding (+ (if (lib:get-option 'bar-enabled) +static-length/percentage+ 1)
                     (if (plusp precision) (1+ precision) 0))))
    (values padding
            (lib:get-option 'suffix))))

(defun get-report-length ()
  (u:mvlet ((percentage-length suffix? (get-percentage-length))
            (bar-enabled (lib:get-option 'bar-enabled))
            (bar-width (lib:get-option 'bar-width)))
    (+ (lib:get-progress-bar-length bar-enabled bar-width)
       percentage-length
       (if suffix? 1 0))))

(defun check-progress-bar-length ()
  (when (null lib:*interactive*)
    (u:mvlet* ((percentage-length suffix? (get-percentage-length))
               (right-padding (+ percentage-length (if suffix? 1 0)))
               (column-count (lib:get-terminal-column-count))
               (report-length (get-report-length))
               (max-bar-length (lib:get-max-progress-bar-length :right-padding right-padding)))
      (when (> report-length column-count)
        (lib:user-error "The value of '--bar-width/-w' (~d) is too wide to fit on the terminal ~
                         (maximum allowed: ~d)"
                        (lib:get-option 'bar-width)
                        max-bar-length)))))

(declaim (inline split-sample))
(defun split-sample (sample)
  (values (aref sample 0)
          (aref sample 1)
          (aref sample 2)
          (aref sample 3)
          (aref sample 4)))

(defun get-cpu-percentage (sample1 sample2)
  (u:mvlet* ((user1 nice1 sys1 intr1 idle1 (split-sample sample1))
             (user2 nice2 sys2 intr2 idle2 (split-sample sample2))
             (user+sys (+ (- user2 user1) (- nice2 nice1) (- sys2 sys1) (- intr2 intr1)))
             (total (float (+ user+sys (- idle2 idle1)) 1f0)))
    (if (zerop total)
        total
        (* (/ user+sys total) 100))))

(defun print-percentage (percentage)
  (u:mvlet* ((padding suffix? (get-percentage-length))
             (precision (lib:get-option 'precision))
             (string (string-right-trim '(#\.) (format nil "~v,vf" padding precision percentage))))
    (write-string string)
    (when suffix?
      (write-char #\%))))

(defun print-progress-bar (percentage)
  (when (lib:get-option 'bar-enabled)
    (let* ((width (lib:get-option 'bar-width))
           (color-enabled (lib:get-option 'color-enabled))
           (base (when color-enabled (lib:get-option 'bar-color-base)))
           (low (when color-enabled (lib:get-option 'bar-color-low)))
           (medium (when color-enabled (lib:get-option 'bar-color-medium)))
           (high (when color-enabled (lib:get-option 'bar-color-high))))
      (lib:print-progress-bar percentage 100 width :base base :low low :medium medium :high high))))

(defun print-report (percentage)
  (print-progress-bar percentage)
  (print-percentage percentage)
  (lib:finish-terminal-output :replace (lib:get-option 'replace-enabled)))

(defun print-all-reports ()
  (let ((count (lib:get-option 'count))
        (delay (lib:get-option 'delay)))
    (when (zerop count)
      (print-report 0))
    (loop :for sample1 = nil :then sample2
          :for sample2 = (get-cpu-ticks)
          :for report-count :from 0
          :when (plusp report-count)
            :do (let ((percentage (get-cpu-percentage sample1 sample2)))
                  (print-report percentage)
                  (when (eql report-count count)
                    (loop-finish)))
          :do (sleep delay))))
