(in-package #:cl-user)

(defpackage #:%syntex.wfc.grid
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:cell
   #:cells
   #:do-cells
   #:get-cell
   #:grid
   #:height
   #:make-grid
   #:value
   #:width
   #:x
   #:y))

(defpackage #:%syntex.wfc.kernel
  (:local-nicknames
   (#:grid #:%syntex.wfc.grid)
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow
   #:count
   #:map)
  (:export
   #:align
   #:convolve
   #:count
   #:kernel
   #:make-kernel
   #:map
   #:rotation
   #:x
   #:y))

(defpackage #:%syntex.wfc.sample
  (:local-nicknames
   (#:grid #:%syntex.wfc.grid)
   (#:img #:%syntex.image)
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow
   #:load)
  (:export
   #:load))

(defpackage #:%syntex.wfc.pattern
  (:local-nicknames
   (#:grid #:%syntex.wfc.grid)
   (#:kernel #:%syntex.wfc.kernel)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:collection
   #:extract
   #:get-count
   #:get-origin-color
   #:get-pattern
   #:grid
   #:make-collection
   #:pattern
   #:size))

(defpackage #:%syntex.wfc.core
  (:local-nicknames
   (#:grid #:%syntex.wfc.grid)
   (#:pat #:%syntex.wfc.pattern)
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:adjacencies
   #:core
   #:direction
   #:direction->index
   #:direction->offset
   #:frequencies
   #:make-core
   #:patterns
   #:rng
   #:sample
   #:tile-map))

(defpackage #:%syntex.wfc.adjacency
  (:local-nicknames
   (#:core #:%syntex.wfc.core)
   (#:grid #:%syntex.wfc.grid)
   (#:kernel #:%syntex.wfc.kernel)
   (#:pat #:%syntex.wfc.pattern)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:generate
   #:invert-edge))

(defpackage #:%syntex.wfc.tile-map
  (:local-nicknames
   (#:core #:%syntex.wfc.core)
   (#:grid #:%syntex.wfc.grid)
   (#:int #:%syntex.internal)
   (#:pat #:%syntex.wfc.pattern)
   (#:pq #:damn-fast-priority-queue)
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:choose-tile
   #:collapse-tile
   #:compute-entropy
   #:enabler-count
   #:entropy-queue
   #:get-neighbor
   #:grid
   #:make-tile-map
   #:pattern-removal-stack
   #:possible-pattern-p
   #:positive-enabler-counts-p
   #:prepare
   #:remove-possible-pattern
   #:uncollapsed-count))

(defpackage #:%syntex.wfc.solver
  (:local-nicknames
   (#:adj #:%syntex.wfc.adjacency)
   (#:core #:%syntex.wfc.core)
   (#:grid #:%syntex.wfc.grid)
   (#:pq #:damn-fast-priority-queue)
   (#:tm #:%syntex.wfc.tile-map)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:solve))

(defpackage #:%syntex.wfc
  (:local-nicknames
   (#:adj #:%syntex.wfc.adjacency)
   (#:core #:%syntex.wfc.core)
   (#:grid #:%syntex.wfc.grid)
   (#:img #:%syntex.image)
   (#:pat #:%syntex.wfc.pattern)
   (#:sample #:%syntex.wfc.sample)
   (#:solver #:%syntex.wfc.solver)
   (#:tm #:%syntex.wfc.tile-map)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:wfc))
