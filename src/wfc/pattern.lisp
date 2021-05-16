(in-package #:cl-user)

(defpackage #:%syntex.wfc.pattern
  (:local-nicknames
   (#:grid #:%syntex.wfc.grid)
   (#:kernel #:%syntex.wfc.kernel)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:extract
   #:get-count
   #:get-origin-color
   #:get-pattern
   #:grid
   #:id
   #:make-pattern
   #:make-pattern-collection
   #:pattern-collection
   #:reflect-p
   #:rotation
   #:size
   #:x
   #:y))

(in-package #:%syntex.wfc.pattern)

(defstruct (pattern
            (:constructor %make-pattern)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (id 0 :type u:non-negative-fixnum)
  (grid nil :type grid:grid)
  (size 2 :type u:positive-fixnum)
  (x 0 :type u:non-negative-fixnum)
  (y 0 :type u:non-negative-fixnum)
  (data (u:make-ub32-array 0) :type u:ub32a))

(u:define-printer (pattern stream)
  (format stream "id: ~d" (id pattern)))

(defstruct (pattern-collection
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (data->pattern (u:dict #'equalp) :type hash-table)
  (id->pattern (make-array 0 :fill-pointer 0 :adjustable t) :type vector))

(u:define-printer (pattern-collection stream)
  (format stream "count: ~d" (hash-table-count (data->pattern pattern-collection))))

(defun make-data (grid kernel &key size rotation reflect-p periodic-p)
  (let ((data (u:make-ub32-array (expt size 2)))
        (cells (grid:cells grid))
        (index 0))
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

(defun make-pattern (kernel &key size (rotation 0) reflect-p periodic-p)
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

(defun register (collection frequencies pattern)
  (let ((data (data pattern))
        (data->pattern (data->pattern collection)))
    (u:if-let ((existing (u:href data->pattern data)))
      (incf (aref frequencies (id existing)))
      (progn
        (setf (id pattern) (hash-table-count data->pattern)
              (u:href data->pattern data) pattern)
        (vector-push-extend pattern (id->pattern collection))
        (vector-push-extend 1 frequencies)))
    pattern))

(defun extract (collection frequencies sample &key size periodic-p)
  (let ((kernel (kernel:make-kernel :grid sample :width size :height size)))
    (flet ((%extract (kernel)
             (dotimes (rotation 4)
               (dolist (reflect-p '(nil t))
                 (let ((pattern (make-pattern kernel
                                              :size size
                                              :rotation rotation
                                              :reflect-p reflect-p
                                              :periodic-p periodic-p)))
                   (register collection frequencies pattern)))))
           (%extract-test (kernel)
             (or periodic-p
                 (= (kernel:count kernel) (expt size 2)))))
      (kernel:convolve kernel #'%extract :test #'%extract-test))))

(defun get-count (collection)
  (length (id->pattern collection)))

(defun get-origin-color (pattern)
  (aref (data pattern) 0))

(defun get-pattern (collection id)
  (aref (id->pattern collection) id))
