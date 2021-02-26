(in-package #:cricket.test)

(defun make-image (sampler)
  (let* ((width 128)
         (height 128)
         (data (make-array (* width height) :element-type 'u:ub8))
         (png (make-instance 'zpng:png
                             :color-type :grayscale
                             :width width
                             :height height
                             :image-data data)))
    (dotimes (y height)
      (dotimes (x width)
        (let* ((x-coord (float x 1d0))
               (y-coord (float y 1d0))
               (z-coord 100.5d0)
               (sample (+ (* (cn:sample sampler x-coord y-coord z-coord) 0.5) 0.5)))
          (setf (aref data (+ x (* y width))) (u:clamp (floor (* sample 255)) 0 255)))))
    png))

(defun find-file (name)
  (let ((path (make-pathname :defaults (string-downcase (symbol-name name))
                             :directory '(:relative "test" "data")
                             :type "png")))
    (asdf:system-relative-pathname :cricket.test path)))

(defun read-file (file)
  (when (uiop:file-exists-p file)
    (pngload:data (pngload:load-file file :flatten t))))

(defun write-file (sampler name)
  (let ((file (find-file name))
        (png (make-image sampler)))
    (zpng:write-png png file)))

(defun compare (name sampler)
  (u:when-let ((file (read-file (find-file name)))
               (test-data (zpng:image-data (make-image sampler))))
    (values (equalp file test-data)
            t)))
