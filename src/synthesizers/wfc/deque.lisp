(in-package #:%syntex.synthesizers.wfc.deque)

(defclass deque ()
  ((%head :accessor head
          :initform 0)
   (%tail :accessor tail
          :initform 0)
   (%buffer :accessor buffer
            :initarg :buffer
            :initform (make-array 0))))

(u:define-printer (deque stream)
  (if (empty-p deque)
      (format stream "[empty]")
      (format stream "~{~s~^ ~}" (to-list deque))))

(defun make-deque (&key (size 32) (element-type t))
  (make-instance 'deque :buffer (make-array (1+ size) :element-type element-type)))

(defun empty-p (deque)
  (= (head deque) (tail deque)))

(defun full-p (deque)
  (let ((length (cl:length (buffer deque))))
    (= (mod (head deque) length)
       (mod (1+ (tail deque)) length))))

(defun length (deque)
  (let ((head (head deque))
        (tail (tail deque)))
    (cond
      ((< head tail)
       (- tail head))
      ((> head tail)
       (- (+ (cl:length (buffer deque)) tail) head))
      (t
       0))))

(defun shift-head (deque)
  (setf (head deque) (mod (1+ (head deque)) (cl:length (buffer deque)))))

(defun unshift-head (deque)
  (setf (head deque) (mod (1- (head deque)) (cl:length (buffer deque)))))

(defun shift-tail (deque)
  (setf (tail deque) (mod (1+ (tail deque)) (cl:length (buffer deque)))))

(defun unshift-tail (deque)
  (setf (tail deque) (mod (1- (tail deque)) (cl:length (buffer deque)))))

(defmacro do-elements ((deque binding) &body body)
  (u:with-gensyms (i head tail buffer)
    `(let ((,head (head ,deque))
           (,tail (tail ,deque))
           (,buffer (buffer ,deque)))
       (block nil
         (symbol-macrolet ((,binding (aref ,buffer ,i)))
           (cond
             ((< ,tail ,head)
              (loop :for ,i :from ,head :below (cl:length ,buffer)
                    :do (progn ,@body))
              (loop :for ,i :below ,tail
                    :do (progn ,@body)))
             (t
              (loop :for ,i :from ,head :below ,tail
                    :do (progn ,@body)))))))))

(defun resize (deque)
  (let* ((old-buffer (buffer deque))
         (new-buffer (make-array (* (cl:length old-buffer) 2)
                                 :element-type (array-element-type old-buffer)))
         (index 0))
    (do-elements (deque x)
      (setf (aref new-buffer index) x)
      (incf index))
    (setf (head deque) 0
          (tail deque) index
          (buffer deque) new-buffer)
    deque))

(defun peek-head (deque)
  (aref (buffer deque) (head deque)))

(defun peek-tail (deque)
  (let ((buffer (buffer deque)))
    (aref buffer (mod (1- (tail deque)) (cl:length buffer)))))

(defun push-head (deque value)
  (when (full-p deque)
    (resize deque))
  (unshift-head deque)
  (setf (aref (buffer deque) (head deque)) value)
  deque)

(defun push-tail (deque value)
  (when (full-p deque)
    (resize deque))
  (setf (aref (buffer deque) (tail deque)) value)
  (shift-tail deque)
  deque)

(defun pop-head (deque)
  (if (empty-p deque)
      (values nil nil)
      (let ((head (peek-head deque)))
        (shift-head deque)
        (values head t))))

(defun pop-tail (deque)
  (if (empty-p deque)
      (values nil nil)
      (let ((tail (peek-tail deque)))
        (unshift-tail deque)
        (values tail t))))

(defun to-list (deque)
  (let ((list nil))
    (do-elements (deque x)
      (push x list))
    (nreverse list)))
