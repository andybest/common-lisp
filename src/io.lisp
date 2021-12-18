(in-package #:cl-freebsd)

(defun write-all (file-descriptor array)
  "Writes the data stored in ARRAY to the file designated by FILE-DESCRIPTOR. ARRAY should be a
  vector with ELEMENT-TYPE (UNSIGNED-BYTE (CFFI:FOREIGN-TYPE-SIZE :CHAR))"
  ;; It's almost always going to be (unsigned-byte 8), but that's not strictly guaranteed
  (let ((left-to-write (length array)))
    (c:with-foreign-array (buffer array (list :array :char (length array)))
      (loop until (= left-to-write 0) do
        (let ((written-this-time (write file-descriptor buffer left-to-write)))
          (decf left-to-write written-this-time)))))
  t)
