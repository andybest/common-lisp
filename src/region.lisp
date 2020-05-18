(in-package #:net.mfiano.lisp.dungen)

(defun make-region ()
  (incf (state-current-region *state*)))

(defun add-cell-to-region (cell)
  (let* ((region (state-current-region *state*))
         (regions (state-regions *state*))
         (cells (u:href regions region)))
    (unless cells
      (setf (u:href regions region) (u:dict #'eq)))
    (setf (u:href regions region cell) cell
          (cell-region cell) region)))

(defun cell-regions-distinct-p (&rest cells)
  (let ((regions (remove 0 (mapcar #'cell-region cells))))
    (and (> (length regions) 1)
         (apply #'/= regions))))
