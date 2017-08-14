(in-package :parsley)

(defvar *object*)

(defclass parse-object ()
  ((parse-tree :accessor parse-tree)))

(defgeneric parse (node)
  (:method (node)))

(defun load-stream (stream)
  (fast-io:with-fast-input (*byte-buffer* nil stream)
    (let ((*object* (make-instance 'parse-object)))
      (setf (parse-tree *object*) (parse :root))
      *object*)))

(defun load-file (path)
  (with-open-file (in path :element-type 'ub8)
    (load-stream in)))
