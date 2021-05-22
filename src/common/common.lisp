(in-package #:%syntex.common)

(defun check-seed (seed)
  (unless (typep seed '(or string null))
    (error 'cond:invalid-seed :seed seed)))

(defun check-file-exists (file-path)
  (unless (uiop:file-exists-p file-path)
    (error 'cond:file-not-found :file-path file-path)))

(defun check-image-dimension (dimension value)
  (unless (typep value '(integer 8 65535))
    (error 'cond:invalid-dimension :dimension dimension :value value)))

(defun check-output-path (output-path)
  (unless (typep output-path '(or pathname string))
    (error 'cond:invalid-output-path :value output-path)))
