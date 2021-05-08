(in-package #:cl-user)

(defpackage #:%syntex.internal
  (:use #:cl)
  ;; Conditions
  (:export
   #:file-not-found
   #:invalid-dimension
   #:invalid-harrison-candidate-count
   #:invalid-harrison-rounds
   #:invalid-kernel-size
   #:invalid-seed
   #:syntex-error))

(defpackage #:%syntex.image
  (:local-nicknames
   (#:png #:pngload)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export))

(defpackage #:%syntex.synthesizers.harrison
  (:local-nicknames
   (#:int #:%syntex.internal)
   (#:lp #:lparallel)
   (#:img #:%syntex.image)
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:harrison))

(defpackage #:%syntex.synthesizers.wfc.point
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:copy
   #:point
   #:x
   #:y
   #:z))

(defpackage #:%syntex.synthesizers.wfc.tile
  (:use #:cl)
  (:export
   #:tile
   #:value))

(defpackage #:%syntex.synthesizers.wfc.periodicity
  (:use #:cl)
  (:export
   #:periodicity
   #:x
   #:y
   #:z))

(defpackage #:%syntex.synthesizers.wfc.direction
  (:local-nicknames
   (#:point #:%syntex.synthesizers.wfc.point)
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow
   #:count
   #:get
   #:type)
  (:export
   #:+cartesian-2d+
   #:+cartesian-3d+
   #:+hexagonal-2d+
   #:+hexagonal-3d+
   #:count
   #:type
   #:x
   #:y
   #:z))

(defpackage #:%syntex.synthesizers.wfc.transform
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow
   #:*)
  (:export
   #:*
   #:angle
   #:check
   #:identity-p
   #:invert
   #:make-group
   #:make-transform
   #:reflect-x
   #:reflect-p
   #:rotation
   #:transform
   #:transforms
   #:group))

(defpackage #:%syntex.synthesizers.wfc.topology
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow
   #:values)
  (:export
   #:contains-index-p
   #:data
   #:depth
   #:directions-count
   #:get-coords
   #:get-index
   #:get-indices
   #:get-value
   #:height
   #:index-count
   #:mask
   #:topology
   #:try-move
   #:values
   #:width))

(defpackage #:%syntex.synthesizers.wfc.topology-grid
  (:local-nicknames
   (#:dir #:%syntex.synthesizers.wfc.direction)
   (#:per #:%syntex.synthesizers.wfc.periodicity)
   (#:point #:%syntex.synthesizers.wfc.point)
   (#:top #:%syntex.synthesizers.wfc.topology)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:make-grid-2d
   #:make-grid-3d))

(defpackage #:%syntex.synthesizers.wfc.topology-data
  (:local-nicknames
   (#:grid #:%syntex.synthesizers.wfc.topology-grid)
   (#:point #:%syntex.synthesizers.wfc.point)
   (#:top #:%syntex.synthesizers.wfc.topology)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export))

(defpackage #:%syntex.synthesizers.wfc.transformed-tile
  (:local-nicknames
   (#:tfm #:%syntex.synthesizers.wfc.transform)
   (#:tile #:%syntex.synthesizers.wfc.tile)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:make-tile
   #:tile
   #:transform
   #:transforms))

(defpackage #:%syntex.synthesizers.wfc.transform-subgroup
  (:local-nicknames
   (#:tfm #:%syntex.synthesizers.wfc.transform)
   (#:tile #:%syntex.synthesizers.wfc.tile)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:copy
   #:entries
   #:entry
   #:expand
   #:get-transforms
   #:permute
   #:set-tile
   #:subgroup
   #:tiles
   #:treatment
   #:treatment-set-by))

(defpackage #:%syntex.synthesizers.wfc.transform-builder
  (:local-nicknames
   (#:sg #:%syntex.synthesizers.wfc.transform-subgroup)
   (#:tfm #:%syntex.synthesizers.wfc.transform)
   (#:tfm.tile #:%syntex.synthesizers.wfc.transformed-tile)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export))

(defpackage #:%syntex.synthesizers.wfc.deque
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow
   #:length)
  (:export
   #:deque
   #:do-elements
   #:empty-p
   #:full-p
   #:length
   #:make-deque
   #:peak-head
   #:peak-tail
   #:pop-head
   #:pop-tail
   #:push-head
   #:push-tail
   #:to-list))

(defpackage #:%syntex.synthesizers.wfc.wave
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow
   #:get)
  (:export
   #:get
   #:get-pattern-count
   #:indices
   #:make-wave
   #:wave))

(defpackage #:%syntex.synthesizers.wfc.tracker
  (:use #:cl)
  (:export
   #:ban
   #:reset
   #:tracker
   #:unban))

(defpackage #:%syntex.synthesizers.wfc.random-picker
  (:use #:cl)
  (:export
   #:get-index
   #:get-pattern
   #:picker))

(defpackage #:%syntex.synthesizers.wfc.frequency-set
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:frequencies
   #:groups
   #:pattern-count
   #:patterns
   #:plogp
   #:priority-indices))

(defpackage #:%syntex.synthesizers.wfc.ordered-random-picker
  (:local-nicknames
   (#:rp #:%syntex.synthesizers.wfc.random-picker)
   (#:u #:golden-utils)
   (#:wave #:%syntex.synthesizers.wfc.wave))
  (:use #:cl)
  (:export
   #:make-picker
   #:picker))

(defpackage #:%syntex.synthesizers.wfc.entropy-tracker
  (:local-nicknames
   (#:rp #:%syntex.synthesizers.wfc.random-picker)
   (#:tr #:%syntex.synthesizers.wfc.tracker)
   (#:wave #:%syntex.synthesizers.wfc.wave)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:entropy
   #:entropy-values
   #:plogp-sum
   #:recompute-entropy
   #:sum
   #:tracker))

(defpackage #:%syntex.synthesizers.wfc.array-priority-entropy-tracker
  (:local-nicknames
   (#:etr #:%syntex.synthesizers.wfc.entropy-tracker)
   (#:fset #:%syntex.synthesizers.wfc.frequency-set)
   (#:rp #:%syntex.synthesizers.wfc.random-picker)
   (#:tr #:%syntex.synthesizers.wfc.tracker)
   (#:wave #:%syntex.synthesizers.wfc.wave)
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow
   #:count)
  (:export
   #:tracker))

(uiop:define-package #:syntex
  (:use #:cl)
  (:reexport #:%syntex.synthesizers.harrison))
