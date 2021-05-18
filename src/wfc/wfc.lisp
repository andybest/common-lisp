(in-package #:%syntex.wfc)

;;; Analyze sample

(defun analyze-patterns (core &key pattern-size periodic-p)
  (pat:extract (core:patterns core)
               (core:frequencies core)
               (core:sample core)
               :size pattern-size
               :periodic-p periodic-p))

(defun analyze-origin-colors (core)
  (let* ((patterns (core:patterns core))
         (pattern-count (pat:get-count patterns))
         (origin-colors (u:make-ub32-array pattern-count)))
    (dotimes (id pattern-count)
      (let* ((pattern (pat:get-pattern patterns id))
             (color (pat:get-origin-color pattern)))
        (setf (aref origin-colors id) color)))
    (setf (core:origin-colors core) origin-colors)))

(defun analyze (core &key pattern-size periodic-p)
  (analyze-patterns core :pattern-size pattern-size :periodic-p periodic-p)
  (analyze-origin-colors core)
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
              (u:mvlet* ((ox oy (core:direction->offset direction))
                         (neighbor (grid:get-cell grid
                                                  (+ (grid:x removed-tile) ox)
                                                  (+ (grid:y removed-tile) oy)
                                                  :periodic-p t)))
                (dolist (pattern-id (u:href pattern-adjacencies direction))
                  (when (= (tm:enabler-count neighbor pattern-id direction) 1)
                    (when (and (plusp (tm:enabler-count neighbor pattern-id :left))
                               (plusp (tm:enabler-count neighbor pattern-id :right))
                               (plusp (tm:enabler-count neighbor pattern-id :up))
                               (plusp (tm:enabler-count neighbor pattern-id :down)))
                      (tm:remove-possible-pattern core neighbor pattern-id)
                      (when (every #'zerop (tm::possible-patterns neighbor))
                        (error 'core:contradiction))
                      (pq:enqueue entropy-queue neighbor (tm:compute-entropy neighbor))
                      (push (cons neighbor pattern-id) (tm:pattern-removal-stack tile-map))))
                  (decf (tm:enabler-count neighbor pattern-id direction)))))))))))

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
    (tagbody
     start
       (prepare-tile-map core)
       (handler-case (solve core)
         (core:contradiction () (go start))))
    core))
