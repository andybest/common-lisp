(in-package #:dungen)

(defstruct (cell (:constructor %make-cell)
                 (:copier nil)
                 (:predicate nil))
  x
  y
  (features (list :wall))
  (region 0))

(defun get-cell (stage x y)
  (when (and (< -1 x (stage-width stage))
             (< -1 y (stage-height stage)))
    (aref (stage-grid stage) x y)))

(defun make-cell (stage x y)
  (setf (aref (stage-grid stage) x y) (%make-cell :x x :y y)))

(defun add-feature (cell feature)
  (pushnew feature (cell-features cell)))

(defun remove-feature (cell feature)
  (u:deletef (cell-features cell) feature))

(defun feature-intersect (cell &rest features)
  (intersection features (cell-features cell)))

(defun has-feature-p (cell feature)
  (member feature (cell-features cell)))

(defun carve (cell feature &key (change-region-p t))
  (add-feature cell feature)
  (remove-feature cell :wall)
  (when change-region-p
    (add-cell-to-region cell)))

(defun carved-p (cell)
  (when cell
    (not (has-feature-p cell :wall))))

(defun uncarve (cell)
  (remhash cell (u:href (state-regions *state*) (cell-region cell)))
  (setf (cell-region cell) 0
        (cell-features cell) '(:wall)))
