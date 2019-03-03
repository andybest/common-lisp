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

(defun cell-regions-distinct-p (cell1 cell2)
  (let ((region1 (cell-region cell1))
        (region2 (cell-region cell2)))
    (and (plusp region1)
         (plusp region2)
         (/= region1 region2))))
