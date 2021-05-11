(in-package #:%syntex.synthesizers.wfc.overlapping-model)

(defclass model (tm:model)
  ((%nx :accessor nx
        :initarg :nx)
   (%ny :accessor ny
        :initarg :ny)
   (%nz :accessor nz
        :initarg :nz)
   (%pattern-indices :reader pattern-indices
                     :initform (u:dict #'equalp))
   (%pattern-arrays :reader pattern-arrays
                    :initform (make-array 0 :fill-pointer 0 :adjustable t))
   (%frequencies :reader frequencies
                 :initform (make-array 0 :fill-pointer 0 :adjustable t))
   (%propagator :reader propagator
                :initform (make-array 0 :fill-pointer 0 :adjustable t))
   (%patterns->tiles :reader patterns->tiles
                     :initform (u:dict #'eql))
   (%tiles->patterns :reader tiles->patterns
                     :initform (u:dict #'eq))))

(defmethod tm:tiles ((object model))
  (u:hash-keys (tiles->patterns object)))

(defun make-model (sample n periodic-p symmetries)
  (let ((topology-data (top.dat:to-tiles
                        (top.dat:make-data-2d sample :periodic-p periodic-p)))
        (symmetries>1 (> symmetries 1)))
    (make-model/symmetry topology-data
                         n
                         (if symmetries>1 (truncate symmetries 2) 1)
                         symmetries>1)))

(defun make-model/symmetry (sample n rotation-count reflect-p)
  (let ((model (make-model/uniform n))
        (tile-transform (tfm:make-tile-transform :rotation-count rotation-count
                                                 :reflect-p reflect-p)))
    (add-sample model sample tile-transform)
    model))

(defun make-model/uniform (n)
  (make-model/cuboid n n n))

(defun make-model/cuboid (nx ny nz)
  (make-instance 'model :nx nx :ny ny :nz nz))

(defun add-sample (model sample &optional tile-transform)
  (let ((topology (top:topology sample))
        (propagator (propagator model))
        (pattern-arrays (pattern-arrays model))
        (patterns->tiles (patterns->tiles model))
        (tiles->patterns (tiles->patterns model)))
    (unless (typep topology 'grid:grid)
      (error "Expected a grid-based topology."))
    (when (= (top:depth topology) 1)
      (setf (nz model) 1))
    (map nil
         (lambda (x)
           (oa:get-patterns x
                            (point:point (nx model) (ny model) (nz model))
                            (grid:periodicity topology)
                            (pattern-indices model)
                            pattern-arrays
                            (frequencies model)))
         (oa:get-transformed-samples sample tile-transform))
    (let* ((directions (grid:directions topology))
           (direction-count (dir:direction-count directions)))
      (dotimes (p (length pattern-arrays))
        (let ((hash-set-array (make-array direction-count)))
          (vector-push-extend hash-set-array propagator)
          (dotimes (d direction-count)
            (let ((l (u:dict #'eql)))
              (dotimes (p2 (length pattern-arrays))
                (let ((x (aref (dir:direction-x directions) d))
                      (y (aref (dir:direction-y directions) d))
                      (z (aref (dir:direction-z directions) d)))
                  (when (agrees-p (aref pattern-arrays p) (aref pattern-arrays p2) x y z)
                    (setf (u:href l p2) p2))))
              (setf (aref (aref propagator p) d) l)))))
      (loop :for x :across pattern-arrays
            :for i :from 0
            :do (setf (u:href patterns->tiles i) (aref x 0 0 0)))
      (u:do-hash (k v patterns->tiles)
        (setf (u:href tiles->patterns v) k)))))

(defun agrees-p (a b dx dy dz)
  (let ((x-min (max 0 dx))
        (x-max (if (minusp dx) (+ dx (pa:get-width b)) (pa:get-width a)))
        (y-min (max 0 dy))
        (y-max (if (minusp dy) (+ dy (pa:get-height b)) (pa:get-height a)))
        (z-min (max 0 dz))
        (z-max (if (minusp dz) (+ dz (pa:get-depth b)) (pa:get-depth a))))
    (loop :for x :from x-min :below x-max
          :do (loop :for y :from y-min :below y-max
                    :do (loop :for z :from z-min :below z-max
                              :unless (= (aref a x y z)
                                         (aref b (- x dx) (- y dy) (- z dz)))
                                :do (return-from agrees-p nil))))
    t))

(defun overlap-coord (x width)
  (if (< x width)
      (values x 0)
      (let ((px (1- width)))
        (values px (- x px)))))

(defun combine-offsets (model x y z)
  (let ((nx (nx model))
        (ny (ny model)))
    (* (+ x (* y nx) z) nx ny)))

(defun make-pattern-topology (model topology)
  (let* ((width (top:width topology))
         (height (top:height topology))
         (depth (top:depth topology))
         (periodicity (grid:periodicity topology))
         (periodic-x (top:periodic-x periodicity))
         (periodic-y (top:periodic-y periodicity))
         (periodic-z (top:periodic-z periodicity)))
    (grid:make-resized-copy topology
                            :width (if periodic-x width (1+ (- width (nx model))))
                            :height (if periodic-y height (1+ (- height (ny model))))
                            :depth (if periodic-z depth (1+ (- depth (nz model)))))))

(defun make-pc->tcio (tc->pcio topology pattern-topology)
  (let ((pc->tciov (make-array (list (top:width pattern-topology)
                                     (top:height pattern-topology)
                                     (top:depth pattern-topology)))))
    (map nil
         (lambda (index)
           (let* ((coords (top:get-coords topology index))
                  (values (top:get tc->pcio index))
                  (point (aref values 0))
                  (px (point:x point))
                  (py (point:y point))
                  (pz (point:z point))
                  (offset (aref values 2)))
             (unless (aref pc->tciov px py pz)
               (setf (aref pc->tciov px py pz) (make-array 0 :fill-pointer 0 :adjustable t)))
             (vector-push-extend (list coords index offset) (aref pc->tciov px py pz))))
         (top:get-indices topology))
    (top.dat:make-data-3d pc->tciov :topology pattern-topology)))

(defun make-tc->pcio (model topology pattern-topology)
  (flet ((%map (point)
           (u:mvlet* ((px ox (overlap-coord (point:x point) (top:width pattern-topology)))
                      (py oy (overlap-coord (point:y point) (top:height pattern-topology)))
                      (pz oz (overlap-coord (point:z point) (top:depth pattern-topology)))
                      (new-point (point:point px py pz))
                      (pattern-index (top:get-index pattern-topology new-point)))
             (list new-point pattern-index (combine-offsets model ox oy oz)))))
    (top.dat:make-data-by-coords topology #'%map)))

(defun get-topology-mask (topology x y z)
  (let ((width (top:width topology))
        (height (top:height topology))
        (depth (top:depth topology))
        (periodicity (grid:periodicity topology)))
    (cond
      ((or (and (not (top:periodic-x periodicity))
                (>= x width))
           (and (not (top:periodic-y periodicity))
                (>= y height))
           (and (not (top:periodic-z periodicity))
                (>= z depth)))
       nil)
      (t
       (let ((x (mod x width))
             (y (mod y height))
             (z (mod z depth)))
         (aref (top:mask topology) (top:get-index topology (point:point x y z))))))))

(defun make-masked-pattern-topology (model topology pattern-topology)
  (flet ((get-mask (point)
           (dotimes (oz (nz model))
             (dotimes (oy (ny model))
               (dotimes (ox (nx model))
                 (when (get-topology-mask topology
                                          (+ (point:x point) ox)
                                          (+ (point:y point) oy)
                                          (+ (point:z point) oz))
                   (return-from get-mask t)))))))
    (let ((pattern-mask (top.dat:make-data-by-coords pattern-topology #'get-mask)))
      (grid:make-masked-copy pattern-topology pattern-mask))))

(defmethod tm:get-mapping ((model model) (topology grid:grid))
  (let* ((frequencies (frequencies model))
         (pattern-model (make-instance 'pm:pattern-model
                                       :propagator (propagator model)
                                       :frequencies frequencies))
         (pattern-arrays (pattern-arrays model))
         (periodicity (grid:periodicity topology))
         (pattern-topology nil)
         (tc->pcio nil)
         (pc->tcio nil)
         (t->pbo (u:dict #'eql))
         (p->tbo (u:dict #'eql)))
    (cond
      ((not (and (top:periodic-x periodicity)
                 (top:periodic-y periodicity)
                 (top:periodic-z periodicity)))
       (setf pattern-topology (make-pattern-topology model topology)
             tc->pcio (make-tc->pcio model topology pattern-topology)
             pc->tcio (make-pc->tcio tc->pcio topology pattern-topology))
       (dotimes (ox (nx model))
         (dotimes (oy (ny model))
           (dotimes (oz (nz model))
             (let ((o (combine-offsets model ox oy oz))
                   (tiles->patterns (u:dict #'eq))
                   (patterns->tiles (u:dict #'eql)))
               (setf (u:href t->pbo o) tiles->patterns
                     (u:href p->tbo o) patterns->tiles)
               (dotimes (pattern (length pattern-arrays))
                 (let* ((pattern-array (aref pattern-arrays pattern))
                        (tile (aref pattern-array ox oy oz))
                        (pattern-set (u:href tiles->patterns tile)))
                   (setf (u:href patterns->tiles pattern) tile)
                   (unless pattern-set
                     (setf (u:href tiles->patterns tile) (u:dict #'eql)
                           pattern-set (u:href tiles->patterns tile)))
                   (setf (u:href pattern-set pattern) pattern))))))))
      (t
       (let ((t->p (u:dict #'eq)))
         (u:do-hash (k v (tiles->patterns model))
           (setf (u:href t->p k) (u:dict #'eql v v)))
         (setf pattern-topology topology
               (u:href t->pbo 0) t->p
               (u:href p->tbo 0) (patterns->tiles model)))))
    (when (top:mask topology)
      (setf pattern-topology (make-masked-pattern-topology model topology pattern-topology)))
    (make-instance 'tmm:mapping
                   :pattern-topology pattern-topology
                   :pattern-model pattern-model
                   :patterns->tiles-by-offset p->tbo
                   :tiles->patterns-by-offset t->pbo
                   :tile-coord->pattern-coord-index/offset tc->pcio
                   :pattern-coord->tile-coord-index/offset pc->tcio)))

(defmethod tm:multiply-frequency ((model model) (tile tile:tile) (multiplier float))
  (let ((pattern-arrays (pattern-arrays model))
        (frequencies (frequencies model)))
    (dotimes (p (length pattern-arrays))
      (let ((pattern-array (aref pattern-arrays p)))
        (dotimes (x (pa:get-width pattern-array))
          (dotimes (y (pa:get-height pattern-array))
            (dotimes (z (pa:get-depth pattern-array))
              (when (eq (aref pattern-array x y z) tile)
                (setf (aref frequencies p) (* (aref frequencies p) multiplier))))))))))
