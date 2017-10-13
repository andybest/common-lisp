(in-package :parsley)

(defvar *buffer*)

(defclass buffer ()
  ((bytes :initarg :bytes)
   (bits :initarg :bits)
   (sequence :initarg :sequence)
   (stream :initarg :stream)))

(defun buffer-bytes ()
  (slot-value *buffer* 'bytes))

(defun buffer-bits ()
  (slot-value *buffer* 'bits))

(defun buffer-sequence ()
  (slot-value *buffer* 'sequence))

(defun buffer-stream ()
  (slot-value *buffer* 'stream))

(defun buffer-position ()
  (fast-io:buffer-position (buffer-bytes)))

(defmacro with-buffer-read ((&key sequence stream) &body body)
  `(let* ((bytes (fast-io:make-input-buffer :vector ,sequence
                                            :stream ,stream))
          (bits (bitio:make-bitio bytes #'fast-io:fast-read-byte))
          (*buffer* (make-instance 'buffer
                                   :bytes bytes
                                   :bits bits
                                   :sequence (fast-io:input-buffer-vector bytes)
                                   :stream (fast-io:input-buffer-stream bytes))))
     ,@body))
