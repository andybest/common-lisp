(in-package #:%syntex.wfc)

;;; Analyze sample

(defun analyze-patterns (core &key pattern-size periodic-p)
  (pat:extract (core:patterns core)
               (core:frequencies core)
               (core:sample core)
               :size pattern-size
               :periodic-p periodic-p))

(defun analyze-origin-colors (core)
  (let* ((patterns (core:patterns core))
         (pattern-count (pat:get-count patterns))
         (origin-colors (u:make-ub32-array pattern-count)))
    (dotimes (id pattern-count)
      (let* ((pattern (pat:get-pattern patterns id))
             (color (pat:get-origin-color pattern)))
        (setf (aref origin-colors id) color)))
    (setf (core:origin-colors core) origin-colors)))

(defun analyze (core &key pattern-size periodic-p)
  (analyze-patterns core :pattern-size pattern-size :periodic-p periodic-p)
  (analyze-origin-colors core)
  (adj:generate core :pattern-size pattern-size))

;;; Main entry point

(defun wfc (sample-path
            &key
              (pattern-size 2)
              (periodic-p t)
              (output-width 10)
              (output-height 10)
              seed)
  (declare (ignore output-width output-height))
  (let* ((sample (sample:load sample-path))
         (core (core:make-core seed sample)))
    (analyze core :pattern-size pattern-size :periodic-p periodic-p)
    core))
