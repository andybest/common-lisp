(in-package #:%syntex.synthesizers.wfc.topology)

(defstruct (periodicity
            (:constructor make-periodicity (&optional x y z))
            (:conc-name periodic-)
            (:predicate nil)
            (:copier nil))
  (x nil :type boolean)
  (y nil :type boolean)
  (z nil :type boolean))
