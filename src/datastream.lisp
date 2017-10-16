(in-package :flac-metadata)

(defclass datastream ()
  ((%marker :reader marker)
   (%stream-info :reader stream-info)
   (%metadata-blocks :reader metadata-blocks)
   (%frames :reader frames)))

(defun parse-datastream ()
  (let ((datastream (make-instance 'datastream)))
    (with-slots (%marker %stream-info %metadata-blocks %frames) datastream
      (setf %marker (parse-marker))
      (destructuring-bind (stream-info . metadata-blocks) (parse-metadata-blocks)
        (setf %stream-info stream-info
              %metadata-blocks metadata-blocks
              %frames (parse-frames))))
    datastream))

(defun parse-marker ()
  (with-buffer-read (:sequence (read-bytes 4))
    (let ((marker (read-string)))
      (if (string= marker "fLaC")
          marker
          (error "Invalid FLAC.")))))

(defun parse-metadata-blocks ()
  (loop :for metadata-block = (parse-metadata-block)
        :until (last-metadata-p metadata-block)
        :collect metadata-block))

(defun parse-frames ()
  :not-implemented)
