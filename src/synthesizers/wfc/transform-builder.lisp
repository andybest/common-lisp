(in-package #:%syntex.synthesizers.wfc.transform-builder)

(defclass builder ()
  ((%tile->subgroup :reader tile->subgroup
                    :initform (u:dict #'eq))
   (%group :reader group
           :initarg :group)
   (%default-treatment :reader default-treatment
                       :initarg :default-treatment
                       :initform :unchanged)))

(defun make-builder (&key group (rotation-count 1) reflect-p)
  (make-instance 'builder :group (or group (tfm:make-group rotation-count reflect-p))))

(defun ensure-group (builder tile)
  (or (u:href (tile->subgroup builder) tile)
      (let ((group (make-instance 'sg:subgroup)))
        (setf (u:href (sg:tiles group) (tfm:make-transform)) tile
              (u:href (tile->subgroup builder) tile) group)
        group)))

(defun insert (builder transform source target)
  (tfm:check (group builder) transform)
  (let ((source-group (ensure-group builder source))
        (target-group (ensure-group builder target)))
    (unless (eq source-group target-group)
      (let ((source-transform (aref (sg:get-transforms source-group source) 0))
            (inverse-target-transform (tfm:invert
                                       (aref (sg:get-transforms target-group target) 0))))
        (sg:permute target-group
                    (lambda (x)
                      (tfm:* (tfm:* (tfm:* inverse-target-transform
                                           source-transform)
                                    transform)
                             x)))
        (map nil
             (lambda (x)
               (vector-push-extend x (sg:entries source-group)))
             (sg:entries target-group))
        (u:do-hash (k v (sg:tiles target-group))
          (sg:set-tile source-group k v)
          (setf (u:href (tile->subgroup builder) v) source-group))))
    (vector-push-extend
     (make-instance 'sg:entry
                    :transform transform
                    :source source
                    :target target)
     (sg:entries source-group))
    (sg:expand source-group)))

(defun set-treatment (builder tile treatment)
  (let* ((group (ensure-group builder tile))
         (group-treatment (sg:treatment group)))
    (when (and group-treatment (not (eq group-treatment treatment)))
      (error "Cannot set treatment of tile ~s." tile))
    (setf (sg:treatment group) treatment
          (sg:treatment-set-by group) tile)))

(defun add-symmetry (builder tile symmetry)
  (ecase symmetry
    (:none
     (ensure-group builder tile))
    (:reflect-x
     (insert builder (tfm:make-transform 180 t) tile tile))
    (:reflect-y
     (insert builder (tfm:make-transform 0 t) tile tile))
    (:reflect-xy
     (insert builder (tfm:make-transform 0 t) tile tile)
     (insert builder (tfm:make-transform 180) tile tile))
    (:reflect-main-diagonal
     (insert builder (tfm:make-transform 270 t) tile tile))
    (:reflect-anti-diagonal
     (insert builder (tfm:make-transform 90 t) tile tile))
    (:reflect-diagonal
     (insert builder (tfm:make-transform 90 t) tile tile)
     (insert builder (tfm:make-transform 180) tile tile))
    (:reflect-any
     (insert builder (tfm:make-transform 0 t) tile tile)
     (insert builder (tfm:make-transform 90) tile tile))
    (:rotate-180
     (insert builder (tfm:make-transform 180) tile tile))
    (:rotate-any
     (insert builder (tfm:make-transform 90) tile tile))))

(defun generate (builder subgroup)
  (let ((group (group builder))
        (tiles (sg:tiles subgroup)))
    (tagbody
     start
       (dotimes (ref1 (if (tfm:reflect-p group) 2 1))
         (loop :for rot1 :below 360 :by (tfm:angle group)
               :for tfm1 = (tfm:make-transform rot1 (plusp ref1))
               :unless (u:href tiles tfm1)
                 :do (dotimes (ref2 (if (tfm:reflect-p group) 2 1))
                       (loop :for rot2 :below 360 :by (tfm:angle group)
                             :for tfm2 = (tfm:make-transform rot2
                                                             (not (eq (plusp ref2)
                                                                      (plusp ref1))))
                             :for source = (u:href tiles tfm2)
                             :when source
                               :do (when (typep source 'tfm.tile:tile)
                                     (setf tfm2 (tfm:* (tfm:invert (tfm.tile:transform source))
                                                       tfm2)))
                                   (let ((source->target (tfm:* (tfm:invert tfm2) tfm1))
                                         (target nil))
                                     (if (and (not (tfm:reflect-x source->target))
                                              (zerop (tfm:rotation source->target)))
                                         (setf target source)
                                         (setf target (tfm.tile:make-tile
                                                       :tile source
                                                       :transform source->target)))
                                     (vector-push-extend
                                      (make-instance 'sg:entry
                                                     :transform source->target
                                                     :source source
                                                     :target target)
                                      (sg:entries subgroup))
                                     (sg:expand subgroup)
                                     (go start)))))))))

(defun build (builder)
  (flet ((get-dict (tile subgroup)
           (let ((subgroup subgroup)
                 (treatment (or (sg:treatment subgroup) (default-treatment builder))))
             (when (eq treatment :generated)
               (setf subgroup (sg:copy subgroup))
               (generate builder subgroup))
             (let ((table (u:dict #'eq))
                   (r1 (aref (sg:get-transforms subgroup tile) 0)))
               (map nil
                    (lambda (r2)
                      (u:when-let ((target (u:href (sg:tiles subgroup) r2)))
                        (setf (u:href table (tfm:* (tfm:invert r1) r2)) target)))
                    (tfm:transforms (group builder)))
               table))))
    (let ((transforms (u:dict #'eq))
          (treatments (u:dict #'eq)))
      (u:do-hash (k v (tile->subgroup builder))
        (setf (u:href transforms k) (get-dict k v))
        (u:when-let ((treatment (sg:treatment v)))
          (setf (u:href treatments k) treatment)))
      (make-instance 'tfm.tile:transforms
                     :transforms transforms
                     :group (group builder)
                     :treatments treatments
                     :default-treatment (default-treatment builder)))))
