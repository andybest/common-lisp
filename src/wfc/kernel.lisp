(in-package #:%syntex.wfc.kernel)

(deftype dimension () '(and u:ub8 (integer 2)))

(deftype rotation () '(integer 0 3))

(declaim (inline %make-kernel))
(defstruct (kernel
            (:constructor %make-kernel)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (grid nil :type grid:grid)
  (width 2 :type dimension)
  (height 2 :type dimension)
  (x 0 :type u:b32)
  (y 0 :type u:b32))

(u:fn-> make-kernel (&key (:grid grid:grid) (:width dimension) (:height dimension)) kernel)
(defun make-kernel (&key grid (width 2) (height 2))
  (declare (optimize speed))
  (%make-kernel :grid grid :width width :height height))

(u:fn-> transform/identity (dimension dimension) list)
(declaim (inline transform/identity))
(defun transform/identity (width height)
  (declare (optimize speed))
  (loop :with coords = nil
        :for y :below height
        :do (loop :for x :below width
                  :do (push (cons x y) coords))
        :finally (return (nreverse coords))))

(u:fn-> transform/reflect (dimension dimension) list)
(declaim (inline transform/reflect))
(defun transform/reflect (width height)
  (declare (optimize speed))
  (loop :with coords = nil
        :for y :below height
        :do (loop :for x :from (1- width) :downto 0
                  :do (push (cons x y) coords))
        :finally (return (nreverse coords))))

(u:fn-> transform/rotate-90 (dimension dimension) list)
(declaim (inline transform/rotate-90))
(defun transform/rotate-90 (width height)
  (declare (optimize speed))
  (loop :with coords = nil
        :for x :below width
        :do (loop :for y :from (1- height) :downto 0
                  :do (push (cons x y) coords))
        :finally (return (nreverse coords))))

(u:fn-> transform/rotate-90-reflect (dimension dimension) list)
(declaim (inline transform/rotate-90-reflect))
(defun transform/rotate-90-reflect (width height)
  (declare (optimize speed))
  (loop :with coords = nil
        :for x :from (1- width) :downto 0
        :do (loop :for y :from (1- height) :downto 0
                  :do (push (cons x y) coords))
        :finally (return (nreverse coords))))

(u:fn-> transform/rotate-180 (dimension dimension) list)
(declaim (inline transform/rotate-180))
(defun transform/rotate-180 (width height)
  (declare (optimize speed))
  (loop :with coords = nil
        :for y :from (1- height) :downto 0
        :do (loop :for x :from (1- width) :downto 0
                  :do (push (cons x y) coords))
        :finally (return (nreverse coords))))

(u:fn-> transform/rotate-180-reflect (dimension dimension) list)
(declaim (inline transform/rotate-180-reflect))
(defun transform/rotate-180-reflect (width height)
  (declare (optimize speed))
  (loop :with coords = nil
        :for y :from (1- height) :downto 0
        :do (loop :for x :below width
                  :do (push (cons x y) coords))
        :finally (return (nreverse coords))))

(u:fn-> transform/rotate-270 (dimension dimension) list)
(declaim (inline transform/rotate-270))
(defun transform/rotate-270 (width height)
  (declare (optimize speed))
  (loop :with coords = nil
        :for x :from (1- width) :downto 0
        :do (loop :for y :below height
                  :do (push (cons x y) coords))
        :finally (return (nreverse coords))))

(u:fn-> transform/rotate-270-reflect (dimension dimension) list)
(declaim (inline transform/rotate-270-reflect))
(defun transform/rotate-270-reflect (width height)
  (declare (optimize speed))
  (loop :with coords = nil
        :for x :below width
        :do (loop :for y :below height
                  :do (push (cons x y) coords))
        :finally (return (nreverse coords))))

(u:fn-> transform (kernel &key (:rotation rotation) (:reflect-p boolean)) list)
(defun transform (kernel &key (rotation 0) reflect-p)
  (declare (optimize speed))
  (let ((width (width kernel))
        (height (height kernel)))
    (ecase rotation
      (0
       (if reflect-p
           (transform/reflect width height)
           (transform/identity width height)))
      (1
       (if reflect-p
           (transform/rotate-90-reflect width height)
           (transform/rotate-90 width height)))
      (2
       (if reflect-p
           (transform/rotate-180-reflect width height)
           (transform/rotate-180 width height)))
      (3
       (if reflect-p
           (transform/rotate-270-reflect width height)
           (transform/rotate-270 width height))))))

(u:fn-> align (kernel fixnum fixnum) kernel)
(declaim (inline align))
(defun align (kernel x y)
  (declare (optimize speed))
  (setf (x kernel) x
        (y kernel) y)
  kernel)

(u:fn-> resolve (kernel u:ub16 u:ub16 &key (:periodic-p boolean)) (or grid:cell null))
(defun resolve (kernel x y &key periodic-p)
  (declare (optimize speed))
  (grid:get-cell (grid kernel)
                 (+ (x kernel) x)
                 (+ (y kernel) y)
                 :periodic-p periodic-p))

(u:fn-> map
        (kernel function &key (:rotation rotation) (:reflect-p boolean) (:periodic-p boolean))
        null)
(defun map (kernel func &key (rotation 0) reflect-p periodic-p)
  (declare (optimize speed))
  (loop :for (x . y) :in (transform kernel :rotation rotation :reflect-p reflect-p)
        :for cell = (resolve kernel x y :periodic-p periodic-p)
        :when cell
          :do (funcall func cell)))

(u:fn-> count (kernel &key (:test function)) u:ub8)
(defun count (kernel &key (test (constantly t)))
  (declare (optimize speed))
  (let ((count 0))
    (declare (u:ub8 count))
    (map kernel
         (lambda (x)
           (when (funcall test x)
             (incf count))))
    count))

(u:fn-> convolve (kernel function &key (:test function)) null)
(defun convolve (kernel func &key (test (constantly t)))
  (declare (optimize speed))
  (let ((grid (grid kernel)))
    (dotimes (y (grid:height grid))
      (dotimes (x (grid:width grid))
        (setf (x kernel) x
              (y kernel) y)
        (when (funcall test kernel)
          (funcall func kernel))))))
