(in-package #:mfiano.data-structures.identifier-pool)

(u:define-constant +id-bits+ 24 :test #'=)

(u:define-constant +id-mask+ (1- (expt 2 +id-bits+)) :test #'=)

(u:define-constant +version-bits+ 32 :test #'=)

(u:define-constant +version-mask+ (1- (expt 2 +version-bits+)) :test #'=)

(declaim (inline %make-pool))
(defstruct (pool
            (:constructor %make-pool)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (store (da:make-array) :type da:dynamic-array)
  free-head
  (count 0 :type u:ub24))

(u:fn-> unpack (fixnum) (values u:ub24 u:ub32))
(u:defun-inline unpack (identifier)
  (declare (optimize speed))
  (values (ldb (byte +id-bits+ 0) identifier)
          (ldb (byte +version-bits+ +id-bits+) identifier)))

(u:fn-> pack (u:ub24 u:ub32) fixnum)
(u:defun-inline pack (id version)
  (declare (optimize speed))
  (dpb version (byte +version-bits+ +id-bits+)
       (dpb id (byte +id-bits+ 0) 0)))

(u:fn-> id (fixnum) u:ub24)
(u:defun-inline id (identifier)
  (declare (optimize speed))
  (nth-value 0 (unpack identifier)))

(u:fn-> version (fixnum) u:ub32)
(u:defun-inline version (identifier)
  (declare (optimize speed))
  (nth-value 1 (unpack identifier)))

(defun make-pool (&key (capacity 128))
  (%make-pool :store (da:make-array :capacity capacity)))

(u:fn-> generate (pool) fixnum)
(defun generate (pool)
  (declare (optimize speed))
  (let ((store (store pool))
        (free-head (free-head pool)))
    (incf (count pool))
    (if free-head
        (u:mvlet ((id version (unpack (da:aref store free-head))))
          (setf (free-head pool) (if (= id +id-mask+) nil id)
                (da:aref store free-head) (pack free-head version)))
        (let ((identifier (pack (da:length store) 0)))
          (da:push store identifier)
          identifier))))

(u:fn-> free (pool fixnum) boolean)
(defun free (pool identifier)
  (declare (optimize speed))
  (let ((store (store pool))
        (index (unpack identifier)))
    (when (< index (da:length store))
      (u:mvlet ((id version (unpack (da:aref store index))))
        (when (= index id)
          (setf (da:aref store id) (pack (or (free-head pool) +id-mask+)
                                         (logand (1+ version) +version-mask+))
                (free-head pool) id)
          (decf (count pool))
          t)))))

(u:fn-> active-p (pool fixnum) boolean)
(defun active-p (pool identifier)
  (declare (optimize speed))
  (let ((store (store pool))
        (index (logand identifier +id-mask+)))
    (and (< index (da:length store))
         (= (unpack (da:aref store index)) identifier))))

(u:fn-> map (pool function) null)
(defun map (pool func)
  (declare (optimize speed))
  (let* ((store (store pool))
         (length (da:length store)))
    (if (free-head pool)
        (loop :for i :below length
              :for identifier = (da:aref store i)
              :when (= (logand identifier +id-mask+) i)
                :do (funcall func identifier))
        (dotimes (i length)
          (funcall func (da:aref store i))))))
