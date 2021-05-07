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

(defpackage #:%syntex.synthesizers.wfc.subgroup
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

(defpackage #:%syntex.synthesizers.wfc.topology
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:contains-index-p
   #:depth
   #:directions-count
   #:get-coords
   #:get-index
   #:get-indices
   #:height
   #:index-count
   #:mask
   #:topology
   #:try-move
   #:width))

(defpackage #:%syntex.synthesizers.wfc.frequency-set
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl)
  (:export))

(defpackage #:%syntex.synthesizers.wfc
  (:local-nicknames
   (#:dir #:%syntex.synthesizers.wfc.direction)
   (#:fs #:%syntex.synthesizers.wfc.frequency-set)
   (#:per #:%syntex.synthesizers.wfc.periodicity)
   (#:point #:%syntex.synthesizers.wfc.point)
   (#:sg #:%syntex.synthesizers.wfc.subgroup)
   (#:tfm #:%syntex.synthesizers.wfc.transform)
   (#:tile #:%syntex.synthesizers.wfc.tile)
   (#:top #:%syntex.synthesizers.wfc.topology)
   (#:u #:golden-utils))
  (:use #:cl))

(uiop:define-package #:syntex
  (:use #:cl)
  (:reexport #:%syntex.synthesizers.harrison))
