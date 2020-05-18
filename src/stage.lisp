(in-package #:net.mfiano.lisp.dungen)

(defstruct (stage (:constructor %make-stage)
                  (:copier nil)
                  (:predicate nil))
  (width 49)
  (height 49)
  (seed (make-seed))
  (density 0.5)
  (room-extent 11)
  (wild-factor 0.25)
  (door-rate 0.5)
  (cycle-factor 0.5)
  grid)

(defun make-grid (stage)
  (let* ((width (stage-width stage))
         (height (stage-height stage))
         (grid (make-array (list width height))))
    (setf (stage-grid stage) grid)
    (dotimes (x width)
      (dotimes (y height)
        (make-cell stage x y)))))

(defun verify-stage (stage)
  (let* ((width (stage-width stage))
         (height (stage-height stage))
         (room-extent (stage-room-extent stage))
         (max-extent (- (ceiling (min (/ width 2) (/ height 2))) 2)))
    (unless (and (oddp width)
                 (plusp width))
      (error "Width must be an odd positive integer."))
    (unless (and (oddp height)
                 (plusp height))
      (error "Height must be an odd positive integer."))
    (unless (plusp (stage-seed stage))
      (error "Seed must be a positive integer."))
    (unless (<= 0.1 (stage-density stage) 1.0)
      (error "Density must be between 0.1 and 1.0."))
    (unless (and (integerp room-extent)
                 (oddp room-extent)
                 (<= 3 room-extent max-extent))
      (error "Room extent must be an odd integer between 3 and ~d" max-extent))
    (unless (<= 0.0 (stage-wild-factor stage) 1.0)
      (error "Wild factor must be between 0.0 and 1.0."))
    (unless (<= 0.0 (stage-door-rate stage) 1.0)
      (error "Door rate must be between 0.0 and 1.0."))
    (unless (<= 0.0 (stage-cycle-factor stage) 1.0)
      (error "Cycle factor must be between 0.0 and 1.0."))))

(defun make-stage (&rest args)
  (let* ((stage (apply #'%make-stage args))
         (*state* (make-state (stage-seed stage))))
    (verify-stage stage)
    (make-grid stage)
    (carve-rooms stage)
    (carve-corridors stage)
    (connect-regions stage)
    (carve-junctions stage)
    (erode-dead-ends stage)
    stage))
