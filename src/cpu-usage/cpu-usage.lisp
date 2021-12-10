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

(defun print-progress-bar (percent)
  (u:mvlet* ((blocks "▏▎▍▌▋▊▉█")
             (covered (* (/ percent 100d0) *arg-bar-width*))
             (full partial (floor covered))
             (index (floor partial 1/8)))
    (format t "~V,,,' <~>~C[ ~V,,,'█<~>~C"
            *arg-bar-width*
            #\return
            full
            (aref blocks index))
    (if *arg-replace*
        (write-char #\return))
    (force-output)))

(defun print-report (usage)
  (let ((percent (string-right-trim '(#\.) (format nil "~,vf" *arg-precision* usage))))
    (if *arg-bars*
        (progn
          (print-progress-bar usage)
          (write-string (format nil "~V,,,' <~>] ~a" (* 2 *arg-bar-width*) percent)))
        (write-string percent))
    (when *arg-suffix*
      (write-char #\%))
    (cond
      (*arg-replace*
       (force-output)
       (write-char #\return))
      (t
       (fresh-line)
       (finish-output)))))

(defun print-all-reports ()
  (loop :for sample1 = nil :then sample2
        :for sample2 = (get-cpu-times)
        :for report-count :from 0
        :when (plusp report-count)
          :do (print-report (get-cpu-usage sample1 sample2))
              (when (eql report-count *arg-count*)
                (loop-finish))
        :do (sleep *arg-delay*)))

(defun run (&rest options)
  (u:mvlet ((args options (base:parse-options *ui* options)))
    (unless (base:dispatch-terminating-options *ui* options)
      (let ((*arg-precision* (u:href options 'precision))
            (*arg-suffix* (u:href options 'suffix))
            (*arg-replace* (u:href options 'replace))
            (*arg-delay* (u:href options 'delay))
            (*arg-count* (u:href options 'count))
            (*arg-bars* (u:href options 'bars))
            (*arg-bar-width* (u:href options 'bar-width)))
        #++(loop :for i :from 0 :to 100
                 :do (write-string #.(format nil "~C[1F~:*~C[2K" #\Escape) *standard-output*)
                     (print-progress-bar1 i)
                     (fresh-line)
                     (print-progress-bar2 i)
                     (sleep 0.01))
        (print-all-reports)))))

(defun toplevel ()
  (base:run-non-interactively #'run))
