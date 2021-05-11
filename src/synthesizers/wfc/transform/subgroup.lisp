(in-package #:%syntex.synthesizers.wfc.transform)

(defclass subgroup ()
  ((%entries :accessor entries
             :initarg :entries
             :initform (make-array 0 :fill-pointer 0 :adjustable t))
   (%tiles :accessor tiles
           :initarg :tiles
           :initform (u:dict #'eq))
   (%treatment :accessor treatment
               :initarg :treatment
               :initform nil)
   (%treatment-set-by :accessor treatment-set-by
                      :initarg :treatment-set-by
                      :initform nil)))

(defclass entry ()
  ((%transform :accessor transform
               :initarg :transform)
   (%source :accessor source
            :initarg :source)
   (%target :accessor target
            :initarg :target)))

(defun get-transforms (subgroup tile)
  (let ((rotations (make-array 0 :fill-pointer 0 :adjustable t)))
    (u:do-hash (k v (tiles subgroup))
      (when (eq v tile)
        (vector-push-extend k rotations)))
    rotations))

(defun set-tile (subgroup transform tile)
  (let ((tiles (tiles subgroup)))
    (u:when-let ((current (u:href tiles transform)))
      (unless (eq current tile)
        (error "Conflict between ~s and ~s." current tile))
      (return-from set-tile nil))
    (setf (u:href tiles transform) tile)
    t))

(defun permute (subgroup func)
  (let ((tiles (u:dict #'eq)))
    (u:do-hash (k v (tiles subgroup))
      (setf (u:href tiles (funcall func k)) v))
    (setf (tiles subgroup) tiles)))

(defun expand (subgroup)
  (let ((expanded-p nil))
    (u:until expanded-p
      (map nil
           (lambda (x)
             (let ((transform (transform x)))
               (u:do-hash (k v (tiles subgroup))
                 (when (eq v (source x))
                   (setf expanded-p (or expanded-p
                                        (set-tile subgroup (* k transform) (target x)))))
                 (when (eq v (target x))
                   (setf expanded-p (or expanded-p
                                        (set-tile
                                         subgroup
                                         (* k (invert transform))
                                         (source x))))))))
           (entries subgroup)))))

(defun copy-subgroup (subgroup)
  (make-instance 'subgroup
                 :entries (u:copy-array (entries subgroup))
                 :tiles (u:copy-hash-table (tiles subgroup))
                 :treatment (treatment subgroup)
                 :treatment-set-by (treatment-set-by subgroup)))
