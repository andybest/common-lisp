(in-package :dungen)

(defvar *state*)

(defstruct (state (:constructor %make-state))
  rng
  (current-region 0)
  (regions (au:dict #'eql))
  (connections (au:dict #'equal))
  dead-ends)

(defun make-state (seed)
  (%make-state :rng (pcg:make-pcg :seed seed)))
