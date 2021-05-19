(in-package #:%syntex.wfc)

;;; Analyze sample

(defun analyze-patterns (core &key pattern-size periodic-p)
  (pat:extract (core:patterns core)
               (core:frequencies core)
               (core:sample core)
               :size pattern-size
               :periodic-p periodic-p))

(defun analyze (core &key pattern-size periodic-p)
  (analyze-patterns core :pattern-size pattern-size :periodic-p periodic-p)
  (adj:generate core :pattern-size pattern-size))

;;; Prepare tile map

(defun prepare-tile-map (core)
  (tm:prepare core))

;;; Main algorithm

(defun propagate (core)
  (let* ((adjacencies (core:adjacencies core))
         (tile-map (core:tile-map core))
         (grid (tm:grid tile-map))
         (entropy-queue (tm:entropy-queue tile-map)))
    (u:while (tm:pattern-removal-stack tile-map)
      (let ((removal (pop (tm:pattern-removal-stack tile-map))))
        (destructuring-bind (removed-tile . removed-pattern-id) removal
          (let ((pattern-adjacencies (aref adjacencies removed-pattern-id)))
            (dolist (direction '(:left :right :up :down))
              (u:mvlet* ((opposite-direction (adj:invert-edge direction))
                         (ox oy (core:direction->offset direction))
                         (neighbor (grid:get-cell grid
                                                  (+ (grid:x removed-tile) ox)
                                                  (+ (grid:y removed-tile) oy)
                                                  :periodic-p t)))
                (dolist (pattern-id (u:href pattern-adjacencies direction))
                  (when (= (tm:enabler-count neighbor pattern-id opposite-direction) 1)
                    (when (and (plusp (tm:enabler-count neighbor pattern-id :left))
                               (plusp (tm:enabler-count neighbor pattern-id :right))
                               (plusp (tm:enabler-count neighbor pattern-id :up))
                               (plusp (tm:enabler-count neighbor pattern-id :down)))
                      (when (= (sbit (tm::possible-patterns neighbor) pattern-id) 1)
                        (tm:remove-possible-pattern core neighbor pattern-id)
                        (when (every #'zerop (tm::possible-patterns neighbor))
                          (error 'core:contradiction))
                        (pq:enqueue entropy-queue neighbor (tm:compute-entropy neighbor))
                        (push (cons neighbor pattern-id) (tm:pattern-removal-stack tile-map)))))
                  (decf (tm:enabler-count neighbor pattern-id opposite-direction)))))))))))

(defun solve (core)
  (let* ((tile-map (core:tile-map core))
         (grid (tm:grid tile-map))
         (entropy-queue (tm:entropy-queue tile-map))
         (tile (grid:get-cell grid 2 2)))
    (pq:enqueue entropy-queue tile (tm:compute-entropy tile))
    (u:while (plusp (tm:uncollapsed-count tile-map))
      (let ((tile (tm:choose-tile core)))
        (tm:collapse-tile core tile)
        (propagate core)
        (decf (tm:uncollapsed-count tile-map))))))

;;; Main entry point

(defun wfc (sample-path
            &key
              (pattern-size 2)
              (periodic-p t)
              (output-width 10)
              (output-height 10)
              seed)
  (let* ((sample (sample:load sample-path))
         (tile-map (tm:make-tile-map :width output-width :height output-height))
         (core (core:make-core :seed seed :sample sample :tile-map tile-map)))
    (analyze core :pattern-size pattern-size :periodic-p periodic-p)
    (prepare-tile-map core)
    (solve core)
    (render core)
    core))

(defun render (core)
  (labels ((from-argb (color)
             (values (ldb (byte 8 16) color)
                     (ldb (byte 8 8) color)
                     (ldb (byte 8 0) color)
                     (ldb (byte 8 24) color)))
           (unpack-argb (data width height)
             (let ((unpacked-data (u:make-ub8-array (* width height 4))))
               (dotimes (i (length data))
                 (u:mvlet ((r g b a (from-argb (aref data i)))
                           (offset (* i 4)))
                   (setf (aref unpacked-data offset) r
                         (aref unpacked-data (+ offset 1)) g
                         (aref unpacked-data (+ offset 2)) b
                         (aref unpacked-data (+ offset 3)) a)))
               unpacked-data))
           (write-image (data width height file-path)
             (let ((png (make-instance 'zpng:png
                                       :color-type :truecolor-alpha
                                       :width width
                                       :height height
                                       :image-data (unpack-argb data width height))))
               (zpng:write-png png file-path))))
    (let* ((grid (tm:grid (core:tile-map core)))
           (width (grid:width grid))
           (height (grid:height grid))
           (data (u:make-ub32-array (* width height))))
      (grid:do-cells (grid cell)
        (let ((color (grid:value cell)))
          (setf (aref data (+ (* (grid:y cell) width) (grid:x cell))) color)))
      (write-image data width height "~/Temp/foo.png"))))
