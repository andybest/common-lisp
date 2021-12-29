(in-package #:mfiano.file-formats.flac)

(defvar *metadata-block*)

(defclass metadata-block ()
  ((%header :reader header
           :initform (make-instance 'metadata-block-header))
   (%data :reader data)))

(defclass metadata-block-header ()
  ((%last-flag :reader last-flag)
   (%type :reader %metadata-type)
   (%length :reader %metadata-size)))

(u:define-printer (metadata-block stream)
  (let ((*metadata-block* metadata-block))
    (format stream "~S" (metadata-type))))

(defun metadata-type ()
  (case (%metadata-type (header *metadata-block*))
    (0 :streaminfo)
    (1 :padding)
    (2 :application)
    (3 :seektable)
    (4 :vorbis-comment)
    (5 :cuesheet)
    (6 :picture)
    (127 :invalid)
    (otherwise :reserved)))

(defun metadata-size ()
  (%metadata-size (header *metadata-block*)))

(defun last-metadata-p (metadata-block)
  (= (last-flag (header metadata-block)) 1))

(defun parse-metadata-block ()
  (let ((*metadata-block* (make-instance 'metadata-block)))
    (with-slots (%header %data) *metadata-block*
      (with-slots (%last-flag %type %length) %header
        (setf %last-flag (parse:read-bits 1)
              %type (parse:read-bits 7)
              %length (parse:read-uint-be 3)
              %data (parse-metadata (metadata-type)))))
    *metadata-block*))
