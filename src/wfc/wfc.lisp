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

(defun propagate (core &key periodic-p)
  (let* ((adjacencies (core:adjacencies core))
         (tile-map (core:tile-map core))
         (entropy-queue (tm:entropy-queue tile-map)))
    (u:while (tm:pattern-removal-stack tile-map)
      (let ((removal (pop (tm:pattern-removal-stack tile-map))))
        (destructuring-bind (tile . removed-pattern-id) removal
          (let ((pattern-adjacencies (aref adjacencies removed-pattern-id)))
            (dolist (direction '(:left :right :up :down))
              (u:when-let ((opposite-direction (adj:invert-edge direction))
                           (neighbor (tm:get-neighbor tile-map tile direction :periodic-p periodic-p)))
                (dolist (pattern-id (u:href pattern-adjacencies direction))
                  (when (and (= (tm:enabler-count neighbor pattern-id opposite-direction) 1)
                             (tm:possible-pattern-p neighbor pattern-id)
                             (tm:positive-enabler-counts-p neighbor pattern-id))
                    (tm:remove-possible-pattern core neighbor pattern-id)
                    (pq:enqueue entropy-queue neighbor (tm:compute-entropy neighbor))
                    (push (cons neighbor pattern-id) (tm:pattern-removal-stack tile-map)))
                  (decf (tm:enabler-count neighbor pattern-id opposite-direction)))))))))))

(defun solve (core &key periodic-p)
  (let* ((tile-map (core:tile-map core))
         (grid (tm:grid tile-map))
         (entropy-queue (tm:entropy-queue tile-map))
         (tile (grid:get-cell grid 2 2)))
    (pq:enqueue entropy-queue tile (tm:compute-entropy tile))
    (u:while (plusp (tm:uncollapsed-count tile-map))
      (let ((tile (tm:choose-tile core)))
        (tm:collapse-tile core tile)
        (propagate core :periodic-p periodic-p)
        (decf (tm:uncollapsed-count tile-map))))))

;;; Render output

(defun render (core)
  (let* ((grid (tm:grid (core:tile-map core)))
         (width (grid:width grid))
         (height (grid:height grid))
         (data (u:make-ub32-array (* width height))))
    (grid:do-cells (grid cell)
      (let ((color (grid:value cell)))
        (setf (aref data (+ (* (grid:y cell) width) (grid:x cell))) color)))
    (img:write-image data width height "~/Temp/foo.png")))

;;; Main entry point

(defun wfc (sample-path
            &key
              (pattern-size 2)
              (periodic-input-p t)
              periodic-output-p
              (output-width 10)
              (output-height 10)
              seed)
  (let* ((sample (sample:load sample-path))
         (tile-map (tm:make-tile-map :width output-width :height output-height))
         (core (core:make-core :seed seed :sample sample :tile-map tile-map)))
    (analyze core :pattern-size pattern-size :periodic-p periodic-input-p)
    (prepare-tile-map core)
    (solve core :periodic-p periodic-output-p)
    (render core)
    core))
