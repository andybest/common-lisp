(in-package #:gfxmath.test)

(defun v (&rest args)
  (apply #'m:make-vector args))

(defun v0 (size)
  (m:make-vector/zero size))

(defun m (&rest args)
  (apply #'m:make-matrix/from-vectors args))

(defun m0 (size)
  (m:make-matrix size))

(defun mid (size)
  (m:make-matrix/identity size))

(defun q (w x y z)
  (m:make-quaternion w x y z))

(defun qid ()
  (m:make-quaternion/identity))

(defun ~= (key1 key2)
  (lambda (x y)
    (every (lambda (c1 c2) (m::~= c1 c2 :rel 1d-6))
           (funcall key1 x)
           (funcall key2 y))))

(defun o= (object expected desc)
  (is object expected desc :test (~= #'m::components #'m::components)))

(defun o/= (object expected desc)
  (isnt object expected desc :test (~= #'m::components #'m::components)))

(defun oa= (object expected desc)
  (is object expected desc :test (~= #'m::components #'identity)))

(defun a= (object expected desc)
  (is object expected desc :test (~= #'identity #'identity)))

(defun s= (object expected desc)
  (is object expected desc :test #'m::~=))
