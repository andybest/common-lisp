(in-package #:%syntex.wfc)

(defun wfc (sample-path
            &key
              seed
              width
              height
              (pattern-size 2)
              (periodic-input-p t)
              (periodic-output-p nil)
              (strategy :backtrack)
              (backtrack-distance 1)
              (backtrack-retries 10)
              (render-p t)
              (show-progress-p t)
              output-path)
  (com:check-file-exists sample-path)
  (com:check-image-dimension :width width)
  (com:check-image-dimension :height height)
  (com:check-seed seed)
  (com:check-output-path output-path)
  (unless (typep pattern-size '(integer 2 255))
    (error 'cond:invalid-wfc-pattern-size :value pattern-size))
  (unless (typep strategy 'strategy)
    (error 'cond:invalid-wfc-strategy :value strategy :allowed '(:none :backtrack)))
  (unless (typep backtrack-distance 'u:positive-fixnum)
    (error 'cond:invalid-wfc-backtrack-distance :value backtrack-distance))
  (unless (typep backtrack-retries 'u:positive-fixnum)
    (error 'cond:invalid-wfc-backtrack-retry-count :value backtrack-retries))
  (let* ((*rng* (rng:make-generator seed))
         (backtracker (make-backtracker :distance backtrack-distance :retries backtrack-retries))
         (core (make-core :seed seed
                          :sample (load-sample sample-path)
                          :backtracker backtracker
                          :strategy strategy)))
    (analyze-patterns core :size pattern-size :periodic-p periodic-input-p)
    (make-tile-map core :width width :height height)
    (solve core :periodic-p periodic-output-p :show-progress-p show-progress-p)
    (if render-p
        (render core :path output-path)
        (make-output core))))
