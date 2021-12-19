(in-package #:freebsd-tools.clcpu)

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

(declaim (inline split-sample))
(defun split-sample (sample)
  (values (aref sample 0)
          (aref sample 1)
          (aref sample 2)
          (aref sample 3)
          (aref sample 4)))

(defun calculate-cpu-percentage (sample1 sample2)
  (u:mvlet* ((user1 nice1 sys1 intr1 idle1 (split-sample sample1))
             (user2 nice2 sys2 intr2 idle2 (split-sample sample2))
             (user+sys (+ (- user2 user1) (- nice2 nice1) (- sys2 sys1) (- intr2 intr1)))
             (total (float (+ user+sys (- idle2 idle1)) 1f0)))
    (if (zerop total)
        total
        (* (/ user+sys total) 100))))

(defun print-percentage (percentage)
  (u:mvlet* ((padding suffix? (calculate-percentage-length))
             (precision (lib:get-option 'precision))
             (string (string-right-trim '(#\.) (format nil "~v,vf" padding precision percentage))))
    ;; Write the formatted percentage value to the stream.
    (write-string string)
    ;; Emite the '%' suffix character if it should be shown.
    (when suffix?
      (write-char #\%))))
