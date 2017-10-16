(in-package :flac-read)

(defvar *flac*)

(deftype ub8 () '(unsigned-byte 8))

(defclass flac ()
  ((file-path :accessor file-path)
   (parse-tree :accessor parse-tree)))

(defun load-stream (stream &optional path)
  (with-buffer-read (:stream stream)
    (let ((*flac* (make-instance 'flac)))
      (setf (parse-tree *flac*) (parse-datastream)
            (file-path *flac*) (or path :in-memory))
      *flac*)))

(defun load-file (path)
  (with-open-file (in path :element-type 'ub8)
    (load-stream in path)))
