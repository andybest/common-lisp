(in-package #:%syntex.synthesizers.wfc.tile-propagator)

(defclass priority/weight ()
  ((%priority :accessor priority
              :initform 0)
   (%weight :accessor weight
            :initform 0d0)))

(defclass options ()
  ((%backtrack-depth :accessor backtrack-depth
                     :initform 0)
   (%constraints :accessor constraints)
   (%weights :accessor weights
             :initform (u:dict #'eq))
   (%random-func :accessor random-func)
   (%pick-heuristic-type :accessor pick-heuristic-type)
   (%model-constraint-algorithm :accessor model-constraint-algorithm)))

(defclass propagator ()
  ((%wave-propagator :reader wave-propagator)
   (%topology :reader topology)
   (%tile-model :reader tile-model)
   (%tile-model-mapping :reader tile-model-mapping)))
