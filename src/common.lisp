(in-package :parsley)

(deftype u1 () '(unsigned-byte 8))

(defun %uncompress-octets (octet-vector compression-scheme)
  (chipz:decompress nil compression-scheme octet-vector
                    :buffer-size (* (length octet-vector) 2)))

(defun %sign-extend (value byte-count)
  (let ((size (* byte-count 8)))
    (logior (* (ldb (byte 1 (1- size)) value)
               (- (expt 2 size)))
            value)))

(defun %string-length (bytes null-terminated)
  (let* ((octet-vector (fast-io::input-buffer-vector *byte-buffer*))
         (max-length (or bytes (length octet-vector)))
         (start (fast-io:buffer-position *byte-buffer*))
         (end (min (length octet-vector) (+ start max-length)))
         (index (if null-terminated
                    (position 0 octet-vector :start start :end end)
                    end)))
    (- index start)))
