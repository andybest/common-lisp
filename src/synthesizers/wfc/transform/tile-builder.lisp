(in-package #:%syntex.synthesizers.wfc.transform)

(defclass builder ()
  ((%tile->subgroup :reader tile->subgroup
                    :initform (u:dict #'eq))
   (%group :reader group
           :initarg :group)
   (%default-treatment :reader default-treatment
                       :initarg :default-treatment
                       :initform :unchanged)))

(defun make-builder (&key group (rotation-count 1) reflect-p)
  (make-instance 'builder :group (or group (make-group rotation-count reflect-p))))

(defun ensure-group (builder tile)
  (or (u:href (tile->subgroup builder) tile)
      (let ((group (make-instance 'subgroup)))
        (setf (u:href (tiles group) (make-transform)) tile
              (u:href (tile->subgroup builder) tile) group)
        group)))

(defun insert (builder transform source target)
  (check (group builder) transform)
  (let ((source-group (ensure-group builder source))
        (target-group (ensure-group builder target)))
    (unless (eq source-group target-group)
      (let ((source-transform (aref (get-transforms source-group source) 0))
            (inverse-target-transform (invert (aref (get-transforms target-group target) 0))))
        (permute target-group
                 (lambda (x)
                   (* (* (* inverse-target-transform source-transform) transform) x)))
        (map nil
             (lambda (x)
               (vector-push-extend x (entries source-group)))
             (entries target-group))
        (u:do-hash (k v (tiles target-group))
          (set-tile source-group k v)
          (setf (u:href (tile->subgroup builder) v) source-group))))
    (vector-push-extend
     (make-instance 'entry
                    :transform transform
                    :source source
                    :target target)
     (entries source-group))
    (expand source-group)))

(defun set-treatment (builder tile treatment)
  (let* ((group (ensure-group builder tile))
         (group-treatment (treatment group)))
    (when (and group-treatment (not (eq group-treatment treatment)))
      (error "Cannot set treatment of tile ~s." tile))
    (setf (treatment group) treatment
          (treatment-set-by group) tile)))

(defun add-symmetry (builder tile symmetry)
  (ecase symmetry
    (:none
     (ensure-group builder tile))
    (:reflect-x
     (insert builder (make-transform 180 t) tile tile))
    (:reflect-y
     (insert builder (make-transform 0 t) tile tile))
    (:reflect-xy
     (insert builder (make-transform 0 t) tile tile)
     (insert builder (make-transform 180) tile tile))
    (:reflect-main-diagonal
     (insert builder (make-transform 270 t) tile tile))
    (:reflect-anti-diagonal
     (insert builder (make-transform 90 t) tile tile))
    (:reflect-diagonal
     (insert builder (make-transform 90 t) tile tile)
     (insert builder (make-transform 180) tile tile))
    (:reflect-any
     (insert builder (make-transform 0 t) tile tile)
     (insert builder (make-transform 90) tile tile))
    (:rotate-180
     (insert builder (make-transform 180) tile tile))
    (:rotate-any
     (insert builder (make-transform 90) tile tile))))

(defun generate (builder subgroup)
  (let ((group (group builder))
        (tiles (tiles subgroup)))
    (tagbody
     start
       (dotimes (ref1 (if (reflect-p group) 2 1))
         (loop :for rot1 :below 360 :by (angle group)
               :for tfm1 = (make-transform rot1 (plusp ref1))
               :unless (u:href tiles tfm1)
                 :do (dotimes (ref2 (if (reflect-p group) 2 1))
                       (loop :for rot2 :below 360 :by (angle group)
                             :for tfm2 = (make-transform rot2
                                                         (not (eq (plusp ref2)
                                                                  (plusp ref1))))
                             :for source = (u:href tiles tfm2)
                             :when source
                               :do (when (typep source 'transformed-tile)
                                     (setf tfm2 (* (invert (transform source)) tfm2)))
                                   (let ((source->target (* (invert tfm2) tfm1))
                                         (target nil))
                                     (if (and (not (reflect-x source->target))
                                              (zerop (rotation source->target)))
                                         (setf target source)
                                         (setf target (make-transformed-tile
                                                       :tile source
                                                       :transform source->target)))
                                     (vector-push-extend
                                      (make-instance 'entry
                                                     :transform source->target
                                                     :source source
                                                     :target target)
                                      (entries subgroup))
                                     (expand subgroup)
                                     (go start)))))))))

(defun build (builder)
  (flet ((get-dict (tile subgroup)
           (let ((subgroup subgroup)
                 (treatment (or (treatment subgroup) (default-treatment builder))))
             (when (eq treatment :generated)
               (setf subgroup (copy-subgroup subgroup))
               (generate builder subgroup))
             (let ((table (u:dict #'eq))
                   (r1 (aref (get-transforms subgroup tile) 0)))
               (map nil
                    (lambda (r2)
                      (u:when-let ((target (u:href (tiles subgroup) r2)))
                        (setf (u:href table (* (invert r1) r2)) target)))
                    (transforms (group builder)))
               table))))
    (let ((transforms (u:dict #'eq))
          (treatments (u:dict #'eq)))
      (u:do-hash (k v (tile->subgroup builder))
        (setf (u:href transforms k) (get-dict k v))
        (u:when-let ((treatment (treatment v)))
          (setf (u:href treatments k) treatment)))
      (make-instance 'tile-transform :transforms transforms
                                     :group (group builder)
                                     :treatments treatments
                                     :default-treatment (default-treatment builder)))))
