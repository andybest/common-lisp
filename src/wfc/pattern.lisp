(in-package #:%syntex.wfc)

(u:eval-always
  (defclass pattern ()
    ((%id :accessor id
          :initarg :id)
     (%grid :reader grid
            :initarg :grid)
     (%size :reader size
            :initarg :size)
     (%x :reader x
         :initarg :x)
     (%y :reader y
         :initarg :y)
     (%frequency :accessor frequency
                 :initform 1)
     (%data :reader data
            :initarg :data))))

(u:fn-> make-data
        (grid kernel &key (:size u:ub8) (:rotation rotation) (:reflect-p boolean)
              (:periodic-p boolean))
        u:ub32a)
(declaim (inline make-data))
(defun make-data (grid kernel &key size rotation reflect-p periodic-p)
  (declare (optimize speed))
  (let ((data (u:make-ub32-array (expt size 2)))
        (cells (cells grid))
        (index 0))
    (declare (u:ub8 index)
             ((simple-array t) cells))
    (map-kernel kernel
                (lambda (x)
                  (let ((value (value x)))
                    (setf (aref data index) value
                          (value (aref cells index)) value)
                    (incf index)))
                :rotation rotation
                :reflect-p reflect-p
                :periodic-p periodic-p)
    data))

(u:fn-> make-pattern
        (kernel &key (:size u:ub8) (:rotation rotation) (:reflect-p boolean)
                (:periodic-p boolean))
        pattern)
(defun make-pattern (kernel &key size (rotation 0) reflect-p periodic-p)
  (declare (optimize speed))
  (let* ((grid (make-grid size size))
         (data (make-data grid
                          kernel
                          :size size
                          :rotation rotation
                          :reflect-p reflect-p
                          :periodic-p periodic-p)))
    (values
     (make-instance 'pattern
                    :grid grid
                    :size size
                    :x (x kernel)
                    :y (y kernel)
                    :data data))))

(u:fn-> register (core pattern) pattern)
(defun register (core pattern)
  (declare (optimize speed))
  (let ((data (data pattern))
        (data->pattern (data->pattern core)))
    (u:if-let ((existing (u:href data->pattern data)))
      (incf (the u:ub32 (frequency existing)))
      (setf (id pattern) (hash-table-count data->pattern)
            (u:href data->pattern data) pattern))
    pattern))

(u:fn-> make-id-map (core) null)
(defun make-id-map (core)
  (declare (optimize speed))
  (let* ((data->pattern (data->pattern core))
         (pattern-count (hash-table-count data->pattern))
         (id->pattern (make-array pattern-count)))
    (u:do-hash-values (pattern data->pattern)
      (let ((id (id pattern)))
        (setf (aref id->pattern id) pattern)))
    (setf (id->pattern core) id->pattern)
    nil))

(u:fn-> extract-patterns (core &key (:size u:ub8) (:periodic-p boolean)) null)
(declaim (inline extract-patterns))
(defun extract-patterns (core &key size periodic-p)
  (declare (optimize speed))
  (let* ((sample (sample core))
         (kernel (make-kernel :grid sample :width size :height size)))
    (flet ((%extract (kernel)
             (dotimes (rotation 4)
               (dolist (reflect-p '(nil t))
                 (let ((pattern (make-pattern kernel
                                              :size size
                                              :rotation rotation
                                              :reflect-p reflect-p
                                              :periodic-p periodic-p)))
                   (register core pattern)))))
           (%extract-test (kernel)
             (or periodic-p
                 (= (count-kernel kernel) (expt size 2)))))
      (convolve kernel #'%extract :test #'%extract-test)
      (make-id-map core)
      nil)))

(u:fn-> get-pattern (core u:ub32) pattern)
(declaim (inline get-pattern))
(defun get-pattern (core pattern-id)
  (declare (optimize speed))
  (aref (the (simple-array t) (id->pattern core)) pattern-id))

(u:fn-> get-pattern-count (core) u:ub32)
(declaim (inline get-pattern-count))
(defun get-pattern-count (core)
  (declare (optimize speed))
  (length (the (simple-array t) (id->pattern core))))

(u:fn-> get-origin-color (core u:ub32) u:ub32)
(declaim (inline get-origin-color))
(defun get-origin-color (core pattern-id)
  (declare (optimize speed))
  (let ((pattern (get-pattern core pattern-id)))
    (aref (the u:ub32a (data pattern)) 0)))

(u:fn-> get-frequency (core u:ub32) u:ub32)
(declaim (inline get-frequency))
(defun get-frequency (core pattern-id)
  (declare (optimize speed))
  (values (frequency (get-pattern core pattern-id))))

(u:fn-> make-edge-kernel (pattern direction) kernel)
(declaim (inline make-edge-kernel))
(defun make-edge-kernel (pattern edge)
  (declare (optimize speed))
  (let ((grid (grid pattern))
        (size (size pattern)))
    (declare (u:positive-fixnum size))
    (ecase edge
      ((:left :right)
       (make-kernel :grid grid :width (1- size) :height size))
      ((:up :down)
       (make-kernel :grid grid :width size :height (1- size))))))

(u:fn-> align-edge (kernel &key (:edge direction)) kernel)
(declaim (inline align-edge))
(defun align-edge (kernel &key edge)
  (declare (optimize speed))
  (ecase edge
    ((:left :up)
     (align-kernel kernel 0 0))
    (:right
     (align-kernel kernel 1 0))
    (:down
     (align-kernel kernel 0 1))))

(u:fn-> compatible-p
        (pattern pattern &key (:data1 u:ub32a) (:data2 u:ub32a) (:edge direction))
        boolean)
(defun compatible-p (pattern1 pattern2 &key data1 data2 edge)
  (declare (optimize speed))
  (flet ((%map (pattern data edge)
           (let ((kernel (make-edge-kernel pattern edge))
                 (index 0))
             (declare (u:ub8 index))
             (align-edge kernel :edge edge)
             (map-kernel kernel
                         (lambda (x)
                           (setf (aref data index) (value x))
                           (incf index))))))
    (declare (inline %map))
    (%map pattern1 data1 edge)
    (%map pattern2 data2 (invert-direction edge))
    (equalp data1 data2)))

(u:fn-> generate-adjacencies (core &key (:pattern-size u:ub8)) null)
(declaim (inline generate-adjacencies))
(defun generate-adjacencies (core &key pattern-size)
  (declare (optimize speed))
  (let* ((pattern-count (get-pattern-count core))
         (adjacencies (make-array pattern-count))
         (data-size (- (expt pattern-size 2) pattern-size))
         (data1 (u:make-ub32-array data-size))
         (data2 (u:make-ub32-array data-size)))
    (dotimes (i pattern-count)
      (let ((pattern1 (get-pattern core i))
            (pattern-adjacencies (u:dict #'eq)))
        (setf (aref adjacencies i) pattern-adjacencies)
        (dotimes (j pattern-count)
          (let ((pattern2 (get-pattern core j)))
            (dolist (edge '(:left :right :up :down))
              (when (compatible-p pattern1 pattern2 :data1 data1 :data2 data2 :edge edge)
                (push j (u:href pattern-adjacencies edge))))))))
    (setf (adjacencies core) adjacencies)
    nil))

(u:fn-> analyze-patterns (core &key (:size u:ub8) (:periodic-p boolean)) null)
(defun analyze-patterns (core &key size periodic-p)
  (declare (optimize speed))
  (format t "~&Analyzing sample patterns...~%")
  (extract-patterns core :size size :periodic-p periodic-p)
  (generate-adjacencies core :pattern-size size)
  (format t "Sample patterns analyzed.~%")
  nil)
