(in-package #:%syntex.wfc)

;;; Render output

(u:fn-> render (core:core &key (:path (or pathname string))) (values))
(defun render (core &key path)
  (declare (optimize speed))
  (let* ((grid (tm:grid (core:tile-map core)))
         (width (grid:width grid))
         (height (grid:height grid))
         (data (u:make-ub32-array (* width height))))
    (grid:do-cells (grid cell)
      (let ((color (grid:value cell)))
        (setf (aref data (+ (* (grid:y cell) width) (grid:x cell))) color)))
    (img:write-image data width height path)
    (format t "~&Image written to: ~s~%" (namestring path))
    (values)))

;;; Main entry point

(defun wfc (sample-path
            &key
              seed
              (pattern-size 2)
              (periodic-input-p t)
              periodic-output-p
              (output-width 128)
              (output-height 128)
              output-path)
  (let* ((sample (sample:load sample-path))
         (tile-map (tm:make-tile-map :width output-width :height output-height))
         (core (core:make-core :seed seed :sample sample :tile-map tile-map)))
    (pat:extract core :size pattern-size :periodic-p periodic-input-p)
    (adj:generate core :pattern-size pattern-size)
    (tm:prepare core)
    (solver:solve core :periodic-p periodic-output-p)
    (render core :path output-path)
    (values)))
