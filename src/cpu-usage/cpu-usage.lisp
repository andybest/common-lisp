(in-package #:mfiano.scripts.cpu-usage)

(defun get-cpu-usage (sample1 sample2)
  (destructuring-bind (user1 nice1 sys1 intr1 idle1) sample1
    (destructuring-bind (user2 nice2 sys2 intr2 idle2) sample2
      (let* ((user (+ (- user2 user1) (- nice2 nice1)))
             (sys (+ (- sys2 sys1) (- intr2 intr1)))
             (total (+ user sys (- idle2 idle1))))
        (if (zerop total)
            0d0
            (* (/ (+ user sys) total) 100d0))))))

(defun print-report (usage &key precision suffix)
  (write-string (string-right-trim '(#\.) (format nil "~,vf" precision usage)))
  (when suffix
    (write-char #\%))
  (fresh-line)
  (finish-output))

(defun print-all-reports (&key precision suffix delay count)
  (loop :for sample1 = nil :then sample2
        :for sample2 = (get-cpu-times)
        :for report-count :from 0
        :when (plusp report-count)
          :do (print-report (get-cpu-usage sample1 sample2) :precision precision :suffix suffix)
              (when (eql report-count count)
                (loop-finish))
        :do (sleep delay)))

(defun run (&rest options)
  (u:mvlet ((args options (base:parse-options *ui* options)))
    (unless (base:dispatch-terminating-options *ui* options)
      (let ((precision (u:href options 'precision))
            (suffix (u:href options 'suffix))
            (delay (or (u:href options 'delay) 0.25))
            (count (u:href options 'count)))
        (print-all-reports :precision precision :suffix suffix :delay delay :count count)))))

(defun toplevel ()
  (base:run-non-interactively #'run))
