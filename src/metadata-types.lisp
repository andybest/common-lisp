(in-package #:mfiano.file-formats.flac)

(defclass seek-point ()
  ((%sample :initarg :sample)
   (%offset :initarg :offset)
   (%sample-count :initarg :sample-count)))

(defclass cuesheet-track ()
  ((%offset :initarg :offset)
   (%number :initarg :number)
   (%isrc :initarg :isrc)
   (%type :initarg :type)
   (%pre-emphasis-flag :initarg :pre-emphasis-flag)
   (%index-count :initarg :index-count)
   (%indices :initarg :indices)))

(defclass cuesheet-track-index ()
  ((%offset :initarg :offset)
   (%number :initarg :number)))

(defmacro define-metadata ((type) (&rest slots) &body body)
  (u:with-gensyms (type-symbol metadata)
    (let ((class-symbol (u:symbolicate 'metadata- type)))
      `(progn
         (defclass ,class-symbol () ,slots)
         (defmethod parse-metadata ((,type-symbol (eql ,(u:make-keyword type))))
           (let ((,metadata (make-instance ',class-symbol)))
             (with-slots ,slots ,metadata
               (parse:with-buffer-read (:sequence (parse:read-bytes (metadata-size)))
                 ,@body))
             ,metadata))))))

(define-metadata (streaminfo)
    (minimum-block-size
     maximum-block-size
     minimum-frame-size maximum-frame-size
     sample-rate channel-count bits-per-sample
     sample-count md5)
  (setf minimum-block-size (parse:read-uint-be 2)
        maximum-block-size (parse:read-uint-be 2)
        minimum-frame-size (parse:read-uint-be 3)
        maximum-frame-size (parse:read-uint-be 3)
        sample-rate (parse:read-bits 20)
        channel-count (parse:read-bits 3)
        bits-per-sample (parse:read-bits 5)
        sample-count (parse:read-bits 36)
        md5 (parse:read-bytes 16)))

(define-metadata (padding) ())

(define-metadata (application) (id data)
  (setf id (parse:read-string :bytes 4)
        data (parse:read-bytes (- (metadata-size) 4))))

(define-metadata (seektable) (seek-points)
  (setf seek-points
        (loop :repeat (/ (metadata-size) 18)
              :collect (make-instance 'seek-point
                                      :sample (parse:read-uint-be 8)
                                      :offset (parse:read-uint-be 8)
                                      :sample-count (parse:read-uint-be 2)))))

(define-metadata (vorbis-comment) (vendor-length vendor user-comments-count user-comments)
  (setf vendor-length (parse:read-uint-le 4)
        vendor (parse:read-string :bytes vendor-length :encoding :utf-8)
        user-comments-count (parse:read-uint-le 4)
        user-comments (u:dict #'equal))
  (loop :repeat user-comments-count
        :for length = (parse:read-uint-le 4)
        :for comment = (parse:read-string :bytes length :encoding :utf-8)
        :for (key value) = (parse:split-string comment #\=)
        :do (setf (u:href user-comments key) value)))

(define-metadata (cuesheet) (media-catalog-id lead-in-sample-count cd-flag track-count tracks)
  (labels ((parse-track ()
             (let ((track (make-instance
                           'cuesheet-track
                           :offset (parse:read-uint-be 8)
                           :number (parse:read-uint-be 1)
                           :isrc (parse:read-string :bytes 12)
                           :type (parse:read-bits 1)
                           :pre-emphasis-flag (parse:read-bits 1)
                           :index-count (progn (parse:read-bits (+ 6 (* 13 8)))
                                               (parse:read-uint-be 1)))))
               (with-slots (%index-count %indices) track
                 (setf %indices (loop :repeat %index-count :collect (parse-track-index))))
               track))
           (parse-track-index ()
             (prog1 (make-instance 'cuesheet-track-index
                                   :offset (parse:read-uint-be 8)
                                   :number (parse:read-uint-be 1))
               (parse:read-bytes 3))))
    (setf media-catalog-id (parse:read-string :bytes 128)
          lead-in-sample-count (parse:read-uint-be 8)
          cd-flag (parse:read-bits 1))
    (parse:read-bits (+ 7 (* 258 8)))
    (setf track-count (parse:read-uint-be 1)
          tracks (loop :repeat track-count :collect (parse-track)))))

(define-metadata (picture)
    (type
     mime-type-length
     mime-type
     description-length
     description
     width
     height
     bits-per-pixel
     indexed-color-count
     size
     data)
  (setf type (parse:read-uint-be 4)
        mime-type-length (parse:read-uint-be 4)
        mime-type (parse:read-string :bytes mime-type-length)
        description-length (parse:read-uint-be 4)
        description (parse:read-string :bytes description-length :encoding :utf-8)
        width (parse:read-uint-be 4)
        height (parse:read-uint-be 4)
        bits-per-pixel (parse:read-uint-be 4)
        indexed-color-count (parse:read-uint-be 4)
        size (parse:read-uint-be 4)
        data (parse:read-bytes size)))

(define-metadata (reserved) ()
  (warn "Skipping a reserved metadata block type."))

(define-metadata (invalid) ()
  (error "Invalid metadata block detected."))
