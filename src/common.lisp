(in-package #:%syntex.internal)

(defun check-seed (seed)
  (unless (typep seed '(or string null))
    (error 'invalid-seed :seed seed)))

(defun check-file-exists (file-path)
  (unless (uiop:file-exists-p file-path)
    (error 'file-not-found :file-path file-path)))

(defun check-image-dimension (dimension value)
  (unless (typep value '(integer 8 65535))
    (error 'invalid-dimension :dimension dimension :value value)))

(defun check-output-path (output-path)
  (unless (typep output-path '(or pathname string))
    (error 'invalid-output-path :value output-path)))
