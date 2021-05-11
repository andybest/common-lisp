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

(defpackage #:%syntex.synthesizers.wfc.base
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl)
  ;; point
  (:export
   #:copy-point
   #:make-point
   #:point
   #:point-x
   #:point-y
   #:point-z)
  ;; tile
  (:export
   #:make-tile
   #:tile
   #:value)
  ;; tile-propagator-tile-set
  (:export
   #:make-tile-propagator-tile-set
   #:offset->patterns
   #:tile-propagator-tile-set
   #:tiles))

(defpackage #:%syntex.synthesizers.wfc.transform
  (:local-nicknames
   (#:base #:%syntex.synthesizers.wfc.base)
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow
   #:*)
  (:export
   #:group
   #:identity-p
   #:invert
   #:make-tile-transform
   #:reflect-x
   #:rotation
   #:tile-transform
   #:transform
   #:transform-all
   #:transform-tile
   #:transform-tiles))

(defpackage #:%syntex.synthesizers.wfc.topology
  (:local-nicknames
   (#:base #:%syntex.synthesizers.wfc.base)
   (#:tfm #:%syntex.synthesizers.wfc.transform)
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow
   #:get
   #:map)
  ;; periodicity
  (:export
   #:periodic-x
   #:periodic-y
   #:periodic-z)
  ;; direction
  (:export
   #:direction-count
   #:direction-set
   #:direction-type
   #:direction-x
   #:direction-y
   #:direction-z
   #:get-direction
   #:invert-direction)
  ;; topology
  (:export
   #:contains-index-p
   #:data
   #:data/tiles
   #:depth
   #:get-coords
   #:get
   #:get-index
   #:get-indices
   #:height
   #:mask
   #:topology
   #:try-move
   #:width)
  ;; grid
  (:export
   #:directions
   #:grid
   #:make-masked-copy
   #:make-resized-copy
   #:periodicity)
  ;; data
  (:export
   #:make-data-1d
   #:make-data-2d
   #:make-data-3d
   #:make-data-by-coords
   #:to-tiles
   #:transform
   #:transform-vector))

(defpackage #:%syntex.synthesizers.wfc.model
  (:local-nicknames
   (#:base #:%syntex.synthesizers.wfc.base)
   (#:pm #:%syntex.synthesizers.wfc.pattern-model)
   (#:tfm #:%syntex.synthesizers.wfc.transform)
   (#:top #:%syntex.synthesizers.wfc.topology)
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

(defpackage #:%syntex.synthesizers.wfc.pattern-model
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:get-pattern-count
   #:pattern-model))

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

(defpackage #:%syntex.synthesizers.wfc.constraint-tile
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:tile-constraint))

(defpackage #:%syntex.synthesizers.wfc.constraint-border
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl)
  (:export))

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

(defpackage #:%syntex.synthesizers.wfc.tile-propagator
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl)
  (:export))

(uiop:define-package #:syntex
  (:use #:cl)
  (:reexport #:%syntex.synthesizers.harrison))
