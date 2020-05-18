(in-package #:net.mfiano.lisp.dungen)

(defvar *state*)

(defstruct (state (:constructor %make-state)
                  (:copier nil)
                  (:predicate nil))
  rng
  (current-region 0)
  (regions (u:dict #'eql))
  (connections (u:dict #'equal))
  dead-ends)

(defun make-state (seed)
  (%make-state :rng (pcg:make-pcg :seed seed)))
