(in-package #:%syntex.synthesizers.wfc.periodicity)

(defstruct (periodicity
            (:constructor periodicity (&optional x y z))
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (x nil :type boolean)
  (y nil :type boolean)
  (z nil :type boolean))
