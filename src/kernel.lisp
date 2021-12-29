(in-package #:mfiano.graphics.procgen.dungen)

(defstruct (extent
            (:copier nil)
            (:predicate nil))
  (min-x 0)
  (min-y 0)
  (max-x 1)
  (max-y 1))

(defstruct (kernel
            (:copier nil)
            (:predicate nil))
  stage
  origin-x
  origin-y
  extent
  selector
  mapper)

(defun make-kernel-factory (selector mapper &rest extent-args)
  (let ((extent (apply #'make-extent extent-args)))
    (lambda (stage x y)
      (make-kernel :stage stage
                   :origin-x x
                   :origin-y y
                   :extent extent
                   :selector selector
                   :mapper mapper))))

(defun resolve-coords (kernel x y)
  (values (+ (kernel-origin-x kernel) x)
          (+ (kernel-origin-y kernel) y)))

(defun select (kernel x y)
  (when (selector (kernel-selector kernel) x y (kernel-extent kernel))
    (multiple-value-bind (stage-x stage-y) (resolve-coords kernel x y)
      (get-cell (kernel-stage kernel) stage-x stage-y))))

(defun kernel-map (kernel func)
  (mapper (kernel-mapper kernel) kernel func))

(defun kernel-detect (kernel func)
  (block nil
    (kernel-map kernel
                (lambda (cell)
                  (u:when-let ((value (funcall func cell)))
                    (return value))))
    nil))

(defun cell->kernel (stage cell layout)
  (funcall layout stage (cell-x cell) (cell-y cell)))

(defun kernel-filter (kernel filter)
  (remove nil
          (kernel-map kernel
                      (lambda (x)
                        (when (funcall filter x)
                          x)))))

(defun %selector/orthogonal (x y extent)
  (or (and (zerop y)
           (<= (extent-min-x extent) (abs x) (extent-max-x extent)))
      (and (zerop x)
           (<= (extent-min-y extent) (abs y) (extent-max-y extent)))))

(defun %selector/rectangle (x y extent)
  (let ((min-x (extent-min-x extent))
        (min-y (extent-min-y extent))
        (max-x (extent-max-x extent))
        (max-y (extent-max-y extent)))
    (and (>= x (- max-x))
         (>= y (- max-y))
         (<= x max-x)
         (<= y max-y)
         (not (and (> x (- min-x))
                   (> y (- min-y))
                   (< x min-x)
                   (< y min-y))))))

(defun selector (shape x y extent)
  (case shape
    (:orthogonal (%selector/orthogonal x y extent))
    (t (%selector/rectangle x y extent))))

(defun %mapper/orthogonal (kernel func)
  (let* ((results)
         (extent (kernel-extent kernel))
         (max-x (extent-max-x extent))
         (max-y (extent-max-y extent)))
    (loop :for y :from (- max-y) :to max-y
          :for cell = (select kernel 0 y)
          :when cell
            :do (push (funcall func cell) results))
    (loop :for x :from (- max-x) :below 0
          :for cell = (select kernel x 0)
          :when cell
            :do (push (funcall func cell) results))
    (loop :for x :from 1 :to max-x
          :for cell = (select kernel x 0)
          :when cell
            :do (push (funcall func cell) results))
    results))

(defun %mapper/rectangle (kernel func)
  (loop :with extent = (kernel-extent kernel)
        :with max-x = (extent-max-x extent)
        :with max-y = (extent-max-y extent)
        :for y :from max-y :downto (- max-y)
        :append (loop :for x :from (- max-x) :to max-x
                      :for cell = (select kernel x y)
                      :when cell
                        :collect (funcall func cell))))

(defun mapper (shape kernel func)
  (case shape
    (:orthogonal (%mapper/orthogonal kernel func))
    (t (%mapper/rectangle kernel func))))

(defun %layout/orthogonal (extent-args)
  (apply #'make-kernel-factory :orthogonal :orthogonal extent-args))

(defun %layout/rectangle (extent-args)
  (apply #'make-kernel-factory :rectangle :default extent-args))

(defun layout (shape &rest extent-args)
  (case shape
    (:orthogonal (%layout/orthogonal extent-args))
    (t (%layout/rectangle extent-args))))

(defun convolve (stage layout filter effect)
  (loop :for x :from 1 :below (1- (stage-width stage))
        :do (loop :for y :from 1 :below (1- (stage-height stage))
                  :for kernel = (funcall layout stage x y)
                  :when (funcall filter kernel)
                    :do (funcall effect kernel))))

(defun collect (stage layout filter)
  (let (items)
    (convolve stage layout filter (lambda (x) (push x items)))
    items))

(defun process (stage layout filter processor &key items (generator #'identity))
  (let ((items (or items (collect stage layout filter))))
    (u:while items
      (let ((kernel (funcall generator (pop items))))
        (when (funcall filter kernel)
          (u:when-let ((new (funcall processor kernel)))
            (push new items)))))))
