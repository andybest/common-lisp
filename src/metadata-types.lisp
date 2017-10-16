(in-package :flac-metadata)

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
  (with-gensyms (type-symbol metadata)
    (let ((class-symbol (symbolicate 'metadata- type)))
      `(progn
         (defclass ,class-symbol () ,slots)
         (defmethod parse-metadata ((,type-symbol (eql ,(make-keyword type))))
           (let ((,metadata (make-instance ',class-symbol)))
             (with-slots ,slots ,metadata
               (with-buffer-read (:sequence (read-bytes (metadata-size)))
                 ,@body))
             ,metadata))))))

(define-metadata (streaminfo) (minimum-block-size maximum-block-size
                               minimum-frame-size maximum-frame-size
                               sample-rate channel-count bits-per-sample
                               sample-count md5)
  (setf minimum-block-size (read-uint-be 2)
        maximum-block-size (read-uint-be 2)
        minimum-frame-size (read-uint-be 3)
        maximum-frame-size (read-uint-be 3)
        sample-rate (read-bits 20)
        channel-count (read-bits 3)
        bits-per-sample (read-bits 5)
        sample-count (read-bits 36)
        md5 (read-bytes 16)))

(define-metadata (padding) ())

(define-metadata (application) (id data)
  (setf id (read-string :bytes 4)
        data (read-bytes (- (metadata-size) 4))))

(define-metadata (seektable) (seek-points)
  (setf seek-points
        (loop :repeat (/ (metadata-size) 18)
              :collect (make-instance 'seek-point
                                      :sample (read-uint-be 8)
                                      :offset (read-uint-be 8)
                                      :sample-count (read-uint-be 2)))))

(define-metadata (vorbis-comment) (vendor-length vendor user-comments-count
                                   user-comments)
  (setf vendor-length (read-uint-le 4)
        vendor (read-string :bytes vendor-length :encoding :utf-8)
        user-comments-count (read-uint-le 4)
        user-comments (make-hash-table :test #'equal))
  (loop :for i :below user-comments-count
        :for length = (read-uint-le 4)
        :for comment = (read-string :bytes length :encoding :utf-8)
        :for (key value) = (split-string comment #\=)
        :do (setf (gethash key user-comments) value)))

(define-metadata (cuesheet) (media-catalog-id lead-in-sample-count cd-flag
                             track-count tracks)
  (labels ((parse-track ()
             (let ((track (make-instance
                           'cuesheet-track
                           :offset (read-uint-be 8)
                           :number (read-uint-be 1)
                           :isrc (read-string :bytes 12)
                           :type (read-bits 1)
                           :pre-emphasis-flag (read-bits 1)
                           :index-count (progn (read-bits (+ 6 (* 13 8)))
                                               (read-uint-be 1)))))
               (with-slots (%index-count %indices) track
                 (setf %indices (loop :repeat %index-count
                                      :collect (parse-track-index))))
               track))
           (parse-track-index ()
             (prog1 (make-instance 'cuesheet-track-index
                                   :offset (read-uint-be 8)
                                   :number (read-uint-be 1))
               (read-bytes 3))))
    (setf media-catalog-id (read-string :bytes 128)
          lead-in-sample-count (read-uint-be 8)
          cd-flag (read-bits 1))
    (read-bits (+ 7 (* 258 8)))
    (setf track-count (read-uint-be 1)
          tracks (loop :repeat track-count
                       :collect (parse-track)))))

(define-metadata (picture) (type mime-type-length mime-type description-length
                            description width height bits-per-pixel
                            indexed-color-count size data)
  (setf type (read-uint-be 4)
        mime-type-length (read-uint-be 4)
        mime-type (read-string :bytes mime-type-length)
        description-length (read-uint-be 4)
        description (read-string :bytes description-length :encoding :utf-8)
        width (read-uint-be 4)
        height (read-uint-be 4)
        bits-per-pixel (read-uint-be 4)
        indexed-color-count (read-uint-be 4)
        size (read-uint-be 4)
        data (read-bytes size)))

(define-metadata (reserved) ()
  (warn "Skipping a reserved metadata block type."))

(define-metadata (invalid) ()
  (error "Invalid metadata block detected."))
