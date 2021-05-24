(in-package #:%syntex.wfc)

(deftype kernel-dimension () '(and u:ub8 (integer 1)))

(deftype rotation () '(integer 0 3))

(u:eval-always
  (defclass kernel ()
    ((%grid :reader grid
            :initarg :grid)
     (%width :reader width
             :initarg :width)
     (%height :reader height
              :initarg :height)
     (%x :accessor x
         :initform 0)
     (%y :accessor y
         :initform 0))))

(u:fn-> make-kernel (&key (:grid grid) (:width kernel-dimension) (:height kernel-dimension)) kernel)
(defun make-kernel (&key grid (width 2) (height 2))
  (declare (optimize speed))
  (values (make-instance 'kernel :grid grid :width width :height height)))

(u:fn-> transform-kernel/identity (kernel-dimension kernel-dimension) list)
(declaim (inline transform-kernel/identity))
(defun transform-kernel/identity (width height)
  (declare (optimize speed))
  (loop :with coords = nil
        :for y :below height
        :do (loop :for x :below width
                  :do (push (cons x y) coords))
        :finally (return (nreverse coords))))

(u:fn-> transform-kernel/reflect (kernel-dimension kernel-dimension) list)
(declaim (inline transform-kernel/reflect))
(defun transform-kernel/reflect (width height)
  (declare (optimize speed))
  (loop :with coords = nil
        :for y :below height
        :do (loop :for x :from (1- width) :downto 0
                  :do (push (cons x y) coords))
        :finally (return (nreverse coords))))

(u:fn-> transform-kernel/rotate-90 (kernel-dimension kernel-dimension) list)
(declaim (inline transform-kernel/rotate-90))
(defun transform-kernel/rotate-90 (width height)
  (declare (optimize speed))
  (loop :with coords = nil
        :for x :below width
        :do (loop :for y :from (1- height) :downto 0
                  :do (push (cons x y) coords))
        :finally (return (nreverse coords))))

(u:fn-> transform-kernel/rotate-90-reflect (kernel-dimension kernel-dimension) list)
(declaim (inline transform-kernel/rotate-90-reflect))
(defun transform-kernel/rotate-90-reflect (width height)
  (declare (optimize speed))
  (loop :with coords = nil
        :for x :from (1- width) :downto 0
        :do (loop :for y :from (1- height) :downto 0
                  :do (push (cons x y) coords))
        :finally (return (nreverse coords))))

(u:fn-> transform-kernel/rotate-180 (kernel-dimension kernel-dimension) list)
(declaim (inline transform-kernel/rotate-180))
(defun transform-kernel/rotate-180 (width height)
  (declare (optimize speed))
  (loop :with coords = nil
        :for y :from (1- height) :downto 0
        :do (loop :for x :from (1- width) :downto 0
                  :do (push (cons x y) coords))
        :finally (return (nreverse coords))))

(u:fn-> transform-kernel/rotate-180-reflect (kernel-dimension kernel-dimension) list)
(declaim (inline transform-kernel/rotate-180-reflect))
(defun transform-kernel/rotate-180-reflect (width height)
  (declare (optimize speed))
  (loop :with coords = nil
        :for y :from (1- height) :downto 0
        :do (loop :for x :below width
                  :do (push (cons x y) coords))
        :finally (return (nreverse coords))))

(u:fn-> transform-kernel/rotate-270 (kernel-dimension kernel-dimension) list)
(declaim (inline transform-kernel/rotate-270))
(defun transform-kernel/rotate-270 (width height)
  (declare (optimize speed))
  (loop :with coords = nil
        :for x :from (1- width) :downto 0
        :do (loop :for y :below height
                  :do (push (cons x y) coords))
        :finally (return (nreverse coords))))

(u:fn-> transform-kernel/rotate-270-reflect (kernel-dimension kernel-dimension) list)
(declaim (inline transform-kernel/rotate-270-reflect))
(defun transform-kernel/rotate-270-reflect (width height)
  (declare (optimize speed))
  (loop :with coords = nil
        :for x :below width
        :do (loop :for y :below height
                  :do (push (cons x y) coords))
        :finally (return (nreverse coords))))

(u:fn-> transform-kernel (kernel &key (:rotation rotation) (:reflect-p boolean)) list)
(defun transform-kernel (kernel &key (rotation 0) reflect-p)
  (declare (optimize speed))
  (let ((width (width kernel))
        (height (height kernel)))
    (ecase rotation
      (0
       (if reflect-p
           (transform-kernel/reflect width height)
           (transform-kernel/identity width height)))
      (1
       (if reflect-p
           (transform-kernel/rotate-90-reflect width height)
           (transform-kernel/rotate-90 width height)))
      (2
       (if reflect-p
           (transform-kernel/rotate-180-reflect width height)
           (transform-kernel/rotate-180 width height)))
      (3
       (if reflect-p
           (transform-kernel/rotate-270-reflect width height)
           (transform-kernel/rotate-270 width height))))))

(u:fn-> align-kernel (kernel fixnum fixnum) kernel)
(declaim (inline align-kernel))
(defun align-kernel (kernel x y)
  (declare (optimize speed))
  (setf (x kernel) x
        (y kernel) y)
  kernel)

(u:fn-> resolve-kernel (kernel u:b16 u:b16 &key (:periodic-p boolean)) (or cell null))
(defun resolve-kernel (kernel x y &key periodic-p)
  (declare (optimize speed))
  (get-cell (grid kernel)
            (+ (the u:b16 (x kernel)) x)
            (+ (the u:b16 (y kernel)) y)
            :periodic-p periodic-p))

(u:fn-> map-kernel
        (kernel function &key (:rotation rotation) (:reflect-p boolean) (:periodic-p boolean))
        null)
(defun map-kernel (kernel func &key (rotation 0) reflect-p periodic-p)
  (declare (optimize speed))
  (loop :for (x . y) :in (transform-kernel kernel :rotation rotation :reflect-p reflect-p)
        :for cell = (resolve-kernel kernel x y :periodic-p periodic-p)
        :when cell
          :do (funcall func cell)))

(u:fn-> map-kernel/left (kernel function &key (:periodic-p boolean)) null)
(declaim (inline map-kernel/left))
(defun map-kernel/left (kernel func &key periodic-p)
  (declare (optimize speed))
  (u:when-let ((cell (resolve-kernel kernel -1 0 :periodic-p periodic-p)))
    (funcall func cell)
    nil))

(u:fn-> map-kernel/right (kernel function &key (:periodic-p boolean)) null)
(declaim (inline map-kernel/right))
(defun map-kernel/right (kernel func &key periodic-p)
  (declare (optimize speed))
  (u:when-let ((cell (resolve-kernel kernel 1 0 :periodic-p periodic-p)))
    (funcall func cell)
    nil))

(u:fn-> map-kernel/up (kernel function &key (:periodic-p boolean)) null)
(declaim (inline map-kernel/up))
(defun map-kernel/up (kernel func &key periodic-p)
  (declare (optimize speed))
  (u:when-let ((cell (resolve-kernel kernel 0 -1 :periodic-p periodic-p)))
    (funcall func cell)
    nil))

(u:fn-> map-kernel/down (kernel function &key (:periodic-p boolean)) null)
(declaim (inline map-kernel/down))
(defun map-kernel/down (kernel func &key periodic-p)
  (declare (optimize speed))
  (u:when-let ((cell (resolve-kernel kernel 0 1 :periodic-p periodic-p)))
    (funcall func cell)
    nil))

(u:fn-> count-kernel (kernel &key (:test function)) u:ub8)
(defun count-kernel (kernel &key (test (constantly t)))
  (declare (optimize speed))
  (let ((count 0))
    (declare (u:ub8 count))
    (map-kernel kernel
                (lambda (x)
                  (when (funcall test x)
                    (incf count))))
    count))

(u:fn-> convolve (kernel function &key (:test function)) null)
(defun convolve (kernel func &key (test (constantly t)))
  (declare (optimize speed))
  (let ((grid (grid kernel)))
    (dotimes (y (the u:ub16 (height grid)))
      (dotimes (x (the u:ub16 (width grid)))
        (setf (x kernel) x
              (y kernel) y)
        (when (funcall test kernel)
          (funcall func kernel))))))
