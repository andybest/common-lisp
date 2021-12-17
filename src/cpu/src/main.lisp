(in-package #:freebsd-tools.cpu)

(defun calculate-cpu-percentage (sample1 sample2)
  (destructuring-bind (user1 nice1 sys1 intr1 idle1) sample1
    (destructuring-bind (user2 nice2 sys2 intr2 idle2) sample2
      (let* ((user (+ (- user2 user1) (- nice2 nice1)))
             (sys (+ (- sys2 sys1) (- intr2 intr1)))
             (total (+ user sys (- idle2 idle1))))
        (if (zerop total)
            0d0
            (* (/ (+ user sys) total) 100d0))))))

(defun print-all-reports ()
  (when (zerop (lib:get-option 'count))
    (print-report 0))
  (loop :for sample1 = nil :then sample2
        :for sample2 = (get-cpu-ticks/total)
        :for report-count :from 0
        :when (plusp report-count)
          :do (let ((percentage (calculate-cpu-percentage sample1 sample2)))
                (print-report percentage)
                (when (eql report-count (lib:get-option 'count))
                  (loop-finish)))
        :do (sleep (lib:get-option 'delay))))

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

(defun prepare-terminal-output (line-count)
  (when (plusp line-count)
    (dotimes (i line-count)
      (write-char #\newline))
    (format t "[~dA" line-count))
  (write-string "7"))

(defun initialize-terminal ()
  (when (lib:get-option 'bars)
    (check-progress-bar-length))
  (when (and (lib:get-option 'replace)
             (null lib:*interactive*))
    (prepare-terminal-output 0)))

(defun run (&rest options)
  (lib:with-options (*ui* options)
    (initialize-terminal)
    (print-all-reports)))

(defun app ()
  (lib:run-non-interactively #'run))
