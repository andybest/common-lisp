(in-package #:cl-user)

(defpackage #:%syntex.wfc.adjacency
  (:local-nicknames
   (#:grid #:%syntex.wfc.grid)
   (#:kernel #:%syntex.wfc.kernel)
   (#:pat #:%syntex.wfc.pattern)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:generate))

(in-package #:%syntex.wfc.adjacency)

(defun make-edge-kernel (pattern edge)
  (let ((grid (pat:grid pattern))
        (size (pat:size pattern)))
    (ecase edge
      ((:left :right)
       (kernel:make-kernel :grid grid :width (1- size) :height size))
      ((:up :down)
       (kernel:make-kernel :grid grid :width size :height (1- size))))))

(defun align-edge (kernel &key edge)
  (ecase edge
    ((:left :up)
     (kernel:align kernel 0 0))
    (:right
     (kernel:align kernel 1 0))
    (:down
     (kernel:align kernel 0 1))))

(defun invert-edge (edge)
  (ecase edge
    (:left :right)
    (:right :left)
    (:up :down)
    (:down :up)))

(defun compatible-p (pattern1 pattern2 &key data1 data2 edge)
  (flet ((%map (pattern data edge)
           (let ((kernel (make-edge-kernel pattern edge))
                 (index 0))
             (align-edge kernel :edge edge)
             (kernel:map kernel
                         (lambda (x)
                           (setf (aref data index) (grid:value x))
                           (incf index))))))
    (%map pattern1 data1 edge)
    (%map pattern2 data2 (invert-edge edge))
    (equalp data1 data2)))

(defun generate (adjacencies patterns pattern-size)
  (let* ((pattern-count (pat:get-count patterns))
         (data-size (- (expt pattern-size 2) pattern-size))
         (data1 (u:make-ub32-array data-size))
         (data2 (u:make-ub32-array data-size)))
    (dotimes (i pattern-count)
      (let ((pattern1 (pat:get-pattern patterns i)))
        (setf (u:href adjacencies i) (u:dict #'eq))
        (dotimes (j pattern-count)
          (let ((pattern2 (pat:get-pattern patterns j)))
            (dolist (edge '(:left :right :up :down))
              (when (compatible-p pattern1 pattern2 :data1 data1 :data2 data2 :edge edge)
                (push j (u:href adjacencies i edge))))))))))
