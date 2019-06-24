(in-package #:dungen)

(a:define-constant +none+ 0)
(a:define-constant +wall+ 1)
(a:define-constant +corridor+ 2)
(a:define-constant +room+ 4)
(a:define-constant +door/horizontal+ 8)
(a:define-constant +door/vertical+ 16)
(a:define-constant +stairs/up+ 32)
(a:define-constant +stairs/down+ 64)
(a:define-constant +connector+ 128)
(a:define-constant +junction+ 256)

(defstruct (cell (:constructor %make-cell)
                 (:copier nil)
                 (:predicate nil))
  x
  y
  (features 1)
  (region 0))

(defun get-cell (stage x y)
  (when (and (< -1 x (stage-width stage))
             (< -1 y (stage-height stage)))
    (aref (stage-grid stage) x y)))

(defun make-cell (stage x y)
  (setf (aref (stage-grid stage) x y) (%make-cell :x x :y y)))

(defun add-feature (cell feature)
  (incf (cell-features cell) feature))

(defun remove-feature (cell feature)
  (decf (cell-features cell) feature))

(defun has-feature-p (cell feature)
  (/= 0 (logand feature (cell-features cell))))

(defun carve (cell feature &key (change-region-p t))
  (add-feature cell feature)
  (remove-feature cell +wall+)
  (when change-region-p
    (add-cell-to-region cell)))

(defun carved-p (cell)
  (when cell
    (not (has-feature-p cell +wall+))))

(defun uncarve (cell)
  (remhash cell (u:href (state-regions *state*) (cell-region cell)))
  (setf (cell-region cell) 0
        (cell-features cell) +wall+))
