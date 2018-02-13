(in-package :flac-metadata)

(defvar *flac*)

(deftype ub8 () '(unsigned-byte 8))

(defclass flac ()
  ((file-path :accessor file-path)
   (parse-tree :accessor parse-tree
               :documentation "The concrete syntax tree of the FLAC stream.")))

(defun %load-stream (stream &optional path)
  (with-buffer-read (:stream stream)
    (let ((*flac* (make-instance 'flac)))
      (setf (parse-tree *flac*) (parse-datastream)
            (file-path *flac*) (or path :in-memory))
      *flac*)))

(defun load-file (path)
  "Load the FLAC file located at the given filesystem PATH. Returns an object with the concrete
syntax tree of the FLAC specification."
  (with-open-file (in path :element-type 'ub8)
    (%load-stream in path)))
