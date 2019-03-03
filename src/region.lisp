(in-package :dungen)

(defun make-region ()
  (incf (state-current-region *state*)))

(defun add-cell-to-region (cell)
  (let* ((region (state-current-region *state*))
         (regions (state-regions *state*))
         (cells (au:href regions region)))
    (unless cells
      (setf (au:href regions region) (au:dict #'eq)))
    (setf (au:href regions region cell) cell
          (cell-region cell) region)))

(defun cell-regions-distinct-p (&rest cells)
  (let ((regions (remove 0 (mapcar #'cell-region cells))))
    (and (> (length regions) 1)
         (apply #'/= regions))))
