(in-package #:net.mfiano.lisp.flac-metadata)

(defvar *dump-spec* '(:streaminfo :application :seektable :vorbis-comment
                      :cuesheet :picture))

(defparameter *dump-padding* 40)

(defun print-line (level label &optional value)
  (format t "~a~v@<~a~>~@[~a~]~%"
          (make-string level :initial-element #\Space)
          (- *dump-padding* level) label value))

(defmethod dump :around (type node level)
  (if (eq (class-name (class-of node)) 'metadata-block)
      (let ((*metadata-block* node))
        (when (member (metadata-type) *dump-spec*)
          (call-next-method)))
      (call-next-method)))

(defun dump-file (file &rest spec)
  "Print out the FLAC metadata of FILE. SPEC may be one or more of the following
to control which types of metadata are dumped, or NIL for all types:

:STREAMINFO, :APPLICATION, :SEEKTABLE, :VORBIS-COMMENT, :CUESHEET, :PICTURE."
  (let ((*dump-spec* (or spec *dump-spec*)))
    (dump :file (load-file file) 0)))

(defmethod dump ((type (eql :file)) node level)
  (print-line level "File" (file-path node))
  (dump :datastream (parse-tree node) (+ level 2)))

(defmethod dump ((type (eql :datastream)) node level)
  (print-line level "Marker" (marker node))
  (print-line level "Metadata Blocks")
  (dump :streaminfo (stream-info node) (+ level 2))
  (dolist (item (metadata-blocks node))
    (let ((*metadata-block* item))
      (dump (metadata-type) item (+ level 2))))
  (print-line level "Frames" "<not implemented>"))

(defmethod dump ((type (eql :streaminfo)) node level)
  (with-slots (minimum-block-size maximum-block-size minimum-frame-size
               maximum-frame-size sample-rate channel-count bits-per-sample
               sample-count md5)
      (data node)
    (print-line level "Stream Information"
                (format nil "~:d bytes" (%metadata-size (header node))))
    (print-line (+ level 2) "Minimum block size"
                (format nil "~:d bytes" minimum-block-size))
    (print-line (+ level 2) "Maximum block size"
                (format nil "~:d bytes" maximum-block-size))
    (print-line (+ level 2) "Minimum frame size"
                (format nil "~:d bytes" minimum-frame-size))
    (print-line (+ level 2) "Maximum frame size"
                (format nil "~:d bytes" maximum-frame-size))
    (print-line (+ level 2) "Sample rate" (format nil "~:dHz" sample-rate))
    (print-line (+ level 2) "Channels" channel-count)
    (print-line (+ level 2) "Bit depth"
                (format nil "~a bits per sample" (1+ bits-per-sample)))
    (print-line (+ level 2) "Total samples" (format nil "~:d" sample-count))
    (print-line (+ level 2) "MD5 checksum"
                (format nil "~{~2,'0x~}" (coerce md5 'list)))))

(defmethod dump ((type (eql :padding)) node level)
  (print-line level "Padding"
              (format nil "~:d bytes" (%metadata-size (header node)))))

(defmethod dump ((type (eql :application)) node level)
  (print-line level "Application"
              (format nil "~:d bytes" (%metadata-size (header node))))
  (with-slots (id) (data node)
    (print-line (+ level 2) "ID" id)))

(defmethod dump ((type (eql :seektable)) node level)
  (print-line level "Seek Table"
              (format nil "~:d bytes" (%metadata-size (header node))))
  (with-slots (seek-points) (data node)
    (dolist (seek-point seek-points)
      (with-slots (%sample %offset %sample-count) seek-point
        (print-line (+ level 2) "Seek Point")
        (print-line (+ level 4) "Target sample"
                    (format nil "~:d" %sample))
        (print-line (+ level 4) "Offset"
                    (format nil "~:d" %offset))
        (print-line (+ level 4) "Sample count"
                    (format nil "~:d" %sample-count))))))

(defmethod dump ((type (eql :vorbis-comment)) node level)
  (print-line level "Vorbis Comment"
              (format nil "~:d bytes" (%metadata-size (header node))))
  (with-slots (vendor user-comments-count user-comments) (data node)
    (print-line (+ level 2) "Vendor" vendor)
    (print-line (+ level 2) "Comments count"
                (format nil "~:d" user-comments-count))
    (dolist (comment (sort (u:hash->alist user-comments)
                           #'string< :key #'car))
      (destructuring-bind (key . value) comment
        (print-line (+ level 4) key (if (zerop (length value))
                                        "<empty>"
                                        value))))))

(defmethod dump ((type (eql :cuesheet)) node level)
  (print-line level "Cue Sheet"
              (format nil "~:d bytes" (%metadata-size (header node))))
  (with-slots (media-catalog-id lead-in-sample-count cd-flag track-count tracks)
      (data node)
    (print-line (+ level 2) "Media catalog ID" (remove #\Nul media-catalog-id))
    (print-line (+ level 2) "Lead-in samples"
                (format nil "~:d" lead-in-sample-count))
    (print-line (+ level 2) "Compact disc?" (if (zerop cd-flag) "No" "Yes"))
    (print-line (+ level 2) "Tracks" track-count)
    (dolist (track tracks)
      (with-slots (%offset %number %isrc %type %pre-emphasis-flag %index-count
                   %indices)
          track
        (print-line (+ level 2) "Track" %number)
        (print-line (+ level 4) "Sample offset" (format nil "~:d" %offset))
        (print-line (+ level 4) "ISRC code" (remove #\Nul %isrc))
        (print-line (+ level 4) "Pre-emphasis" (if (zerop %pre-emphasis-flag)
                                                   "No"
                                                   "Yes"))
        (print-line (+ level 4) "Indices" %index-count)
        (dolist (index %indices)
          (with-slots (%offset %number) index
            (print-line (+ level 6) "Offset"
                        (format nil "~:d" %offset))
            (print-line (+ level 6) "Number" %number)))))))

(defmethod dump ((type (eql :picture)) node level)
  (print-line level "Picture" (%metadata-size (header node)))
  (with-slots (type mime-type description width height bits-per-pixel
               indexed-color-count size)
      (data node)
    (print-line (+ level 4) "Type" type)
    (print-line (+ level 4) "MIME type" mime-type)
    (print-line (+ level 4) "Description" description)
    (print-line (+ level 4) "Width" (format nil "~:dpx" width))
    (print-line (+ level 4) "Height" (format nil "~:dpx" height))
    (print-line (+ level 4) "Color depth" (format nil "~dbpp" bits-per-pixel))
    (print-line (+ level 4) "Indexed colors" indexed-color-count)
    (print-line (+ level 4) "Size" (format nil "~:d" size))))
