(in-package :parsley)

(defvar *byte-buffer*)

(defun octets= (octet-vector octet-list)
  (equalp octet-vector (fast-io:octets-from octet-list)))

(defun read-bytes (count &key (processor #'identity))
  (let ((octet-vector (fast-io:make-octet-vector count)))
    (fast-io:fast-read-sequence octet-vector *byte-buffer*)
    (funcall processor octet-vector)))

(defun read-uint-be (byte-count)
  (let ((value 0))
    (loop :for i :from (* (1- byte-count) 8) :downto 0 :by 8
          :for byte = (fast-io:fast-read-byte *byte-buffer*)
          :do (setf (ldb (byte 8 i) value) byte))
    value))

(defun read-uint-le (byte-count)
  (let ((value 0))
    (loop :for i :below (* byte-count 8) :by 8
          :for byte = (fast-io:fast-read-byte *byte-buffer*)
          :do (setf (ldb (byte 8 i) value) byte))
    value))

(defun read-int-be (byte-count)
  (%sign-extend (read-uint-be byte-count) byte-count))

(defun read-int-le (byte-count)
  (%sign-extend (read-uint-le byte-count) byte-count))

(defun read-string (&key bytes (encoding :ascii) (processor #'identity)
                      null-terminated)
  (let ((octet-vector (fast-io:make-octet-vector
                       (%string-length bytes null-terminated))))
    (fast-io:fast-read-sequence octet-vector *byte-buffer*)
    (when null-terminated
      (fast-io:fast-read-byte *byte-buffer*))
    (babel:octets-to-string
     (funcall processor octet-vector)
     :encoding encoding)))
