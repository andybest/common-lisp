(in-package :dungen)

(defstruct (cell (:constructor %make-cell))
  x
  y
  (features (list :wall))
  (region 0))

(defun cell-index (stage x y)
  (let ((width (stage-width stage)))
    (+ (* y width) x)))

(defun get-cell (stage x y)
  (when (and (not (minusp x))
             (not (minusp y))
             (< x (stage-width stage))
             (< y (stage-height stage)))
    (aref (stage-grid stage) (cell-index stage x y))))

(defun make-cell (stage x y)
  (let ((index (cell-index stage x y)))
    (setf (aref (stage-grid stage) index) (%make-cell :x x :y y))))

;;; Cell features

(defun feature-present-p (cell feature)
  (member feature (cell-features cell)))

(defun add-feature (cell feature)
  (pushnew feature (cell-features cell)))

(defun remove-feature (cell feature)
  (au:deletef (cell-features cell) feature))

(defun feature-intersect (cell &rest features)
  (intersection features (cell-features cell)))

(defun carve (cell feature &key (change-region-p t))
  (add-feature cell feature)
  (remove-feature cell :wall)
  (when change-region-p
    (add-cell-to-region cell))
  (values))

(defun carved-p (cell)
  (when cell
    (not (feature-present-p cell :wall))))

(defun uncarve (cell)
  (remhash cell (au:href (state-regions *state*) (cell-region cell)))
  (setf (cell-region cell) 0
        (cell-features cell) '(:wall)))
