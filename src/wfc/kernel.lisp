(in-package #:cl-user)

(defpackage #:%syntex.wfc.kernel
  (:local-nicknames
   (#:grid #:%syntex.wfc.grid)
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow
   #:count
   #:map)
  (:export
   #:align
   #:convolve
   #:count
   #:kernel
   #:make-kernel
   #:map
   #:x
   #:y))

(in-package #:%syntex.wfc.kernel)

(defstruct (kernel
            (:constructor %make-kernel)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (grid nil :type grid:grid)
  (width 2 :type u:positive-fixnum)
  (height 2 :type u:positive-fixnum)
  (x 0 :type u:non-negative-fixnum)
  (y 0 :type u:non-negative-fixnum))

(defun make-kernel (&key grid (width 2) (height 2))
  (%make-kernel :grid grid :width width :height height))

(defun transform/identity (width height)
  (loop :with coords = nil
        :for y :below height
        :do (loop :for x :below width
                  :do (push (cons x y) coords))
        :finally (return (nreverse coords))))

(defun transform/reflect (width height)
  (loop :with coords = nil
        :for y :below height
        :do (loop :for x :from (1- width) :downto 0
                  :do (push (cons x y) coords))
        :finally (return (nreverse coords))))

(defun transform/rotate-90 (width height)
  (loop :with coords = nil
        :for x :below width
        :do (loop :for y :from (1- height) :downto 0
                  :do (push (cons x y) coords))
        :finally (return (nreverse coords))))

(defun transform/rotate-90-reflect (width height)
  (loop :with coords = nil
        :for x :from (1- width) :downto 0
        :do (loop :for y :from (1- height) :downto 0
                  :do (push (cons x y) coords))
        :finally (return (nreverse coords))))

(defun transform/rotate-180 (width height)
  (loop :with coords = nil
        :for y :from (1- height) :downto 0
        :do (loop :for x :from (1- width) :downto 0
                  :do (push (cons x y) coords))
        :finally (return (nreverse coords))))

(defun transform/rotate-180-reflect (width height)
  (loop :with coords = nil
        :for y :from (1- height) :downto 0
        :do (loop :for x :below width
                  :do (push (cons x y) coords))
        :finally (return (nreverse coords))))

(defun transform/rotate-270 (width height)
  (loop :with coords = nil
        :for x :from (1- width) :downto 0
        :do (loop :for y :below height
                  :do (push (cons x y) coords))
        :finally (return (nreverse coords))))

(defun transform/rotate-270-reflect (width height)
  (loop :with coords = nil
        :for x :below width
        :do (loop :for y :below height
                  :do (push (cons x y) coords))
        :finally (return (nreverse coords))))

(defun transform (kernel &key (rotation 0) reflect-p)
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

(defun align (kernel x y)
  (setf (x kernel) x
        (y kernel) y)
  kernel)

(defun resolve (kernel x y &key periodic-p)
  (grid:get-cell (grid kernel)
                 (+ (x kernel) x)
                 (+ (y kernel) y)
                 :periodic-p periodic-p))

(defun map (kernel func &key (rotation 0) reflect-p periodic-p)
  (loop :for (x . y) :in (transform kernel :rotation rotation :reflect-p reflect-p)
        :for cell = (resolve kernel x y :periodic-p periodic-p)
        :when cell
          :do (funcall func cell)))

(defun count (kernel &key (test (constantly t)))
  (let ((count 0))
    (map kernel
         (lambda (x)
           (when (funcall test x)
             (incf count))))
    count))

(defun convolve (kernel func &key (test (constantly t)))
  (let ((grid (grid kernel)))
    (dotimes (y (grid:height grid))
      (dotimes (x (grid:width grid))
        (setf (x kernel) x
              (y kernel) y)
        (when (funcall test kernel)
          (funcall func kernel))))))
