(in-package #:%syntex.wfc.pattern)

(declaim (inline %make-pattern))
(defstruct (pattern
            (:constructor %make-pattern)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (id 0 :type u:ub32)
  (grid nil :type grid:grid)
  (size 2 :type u:positive-fixnum)
  (x 0 :type u:non-negative-fixnum)
  (y 0 :type u:non-negative-fixnum)
  (frequency 1 :type u:ub32)
  (data (u:make-ub32-array 0) :type u:ub32a))

(u:define-printer (pattern stream)
  (format stream "id: ~d" (id pattern)))

(u:fn-> make-data
        (grid:grid kernel:kernel &key (:size u:ub8) (:rotation kernel:rotation) (:reflect-p boolean)
                   (:periodic-p boolean))
        u:ub32a)
(declaim (inline make-data))
(defun make-data (grid kernel &key size rotation reflect-p periodic-p)
  (declare (optimize speed))
  (let ((data (u:make-ub32-array (expt size 2)))
        (cells (grid:cells grid))
        (index 0))
    (declare (u:ub16 index))
    (kernel:map kernel
                (lambda (x)
                  (let ((value (grid:value x)))
                    (setf (aref data index) value
                          (grid:value (aref cells index)) value)
                    (incf index)))
                :rotation rotation
                :reflect-p reflect-p
                :periodic-p periodic-p)
    data))

(u:fn-> make-pattern
        (kernel:kernel &key (:size u:ub8) (:rotation kernel:rotation) (:reflect-p boolean)
                       (:periodic-p boolean))
        pattern)
(defun make-pattern (kernel &key size (rotation 0) reflect-p periodic-p)
  (declare (optimize speed))
  (let* ((grid (grid:make-grid size size))
         (data (make-data grid
                          kernel
                          :size size
                          :rotation rotation
                          :reflect-p reflect-p
                          :periodic-p periodic-p)))
    (%make-pattern :grid grid
                   :size size
                   :x (kernel:x kernel)
                   :y (kernel:y kernel)
                   :data data)))

(u:fn-> register (core:core pattern) pattern)
(defun register (core pattern)
  (declare (optimize speed))
  (let ((data (data pattern))
        (data->pattern (core:data->pattern core)))
    (u:if-let ((existing (u:href data->pattern data)))
      (incf (frequency existing))
      (setf (id pattern) (hash-table-count data->pattern)
            (u:href data->pattern data) pattern))
    pattern))

(u:fn-> make-id-map (core:core) null)
(defun make-id-map (core)
  (declare (optimize speed))
  (let* ((data->pattern (core:data->pattern core))
         (pattern-count (hash-table-count data->pattern))
         (id->pattern (make-array pattern-count)))
    (u:do-hash-values (pattern data->pattern)
      (let ((id (id pattern)))
        (setf (aref id->pattern id) pattern)))
    (setf (core:id->pattern core) id->pattern)
    nil))

(u:fn-> extract (core:core &key (:size u:ub8) (:periodic-p boolean)) null)
(declaim (inline extract))
(defun extract (core &key size periodic-p)
  (declare (optimize speed))
  (let* ((sample (core:sample core))
         (kernel (kernel:make-kernel :grid sample :width size :height size)))
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
                 (= (kernel:count kernel) (expt size 2)))))
      (kernel:convolve kernel #'%extract :test #'%extract-test)
      (make-id-map core)
      nil)))

(u:fn-> get-pattern (core:core u:ub32) pattern)
(declaim (inline get-pattern))
(defun get-pattern (core pattern-id)
  (declare (optimize speed))
  (aref (core:id->pattern core) pattern-id))

(u:fn-> get-count (core:core) u:ub32)
(declaim (inline get-count))
(defun get-count (core)
  (declare (optimize speed))
  (length (core:id->pattern core)))

(u:fn-> get-origin-color (core:core u:ub32) u:ub32)
(declaim (inline get-origin-color))
(defun get-origin-color (core pattern-id)
  (declare (optimize speed))
  (let ((pattern (get-pattern core pattern-id)))
    (aref (data pattern) 0)))

(u:fn-> get-frequency (core:core u:ub32) u:ub32)
(declaim (inline get-frequency))
(defun get-frequency (core pattern-id)
  (declare (optimize speed))
  (frequency (get-pattern core pattern-id)))
