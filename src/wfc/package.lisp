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

(defpackage #:%syntex.wfc.core
  (:local-nicknames
   (#:grid #:%syntex.wfc.grid)
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:adjacencies
   #:core
   #:data->pattern
   #:direction
   #:direction->index
   #:direction->offset
   #:history
   #:id->pattern
   #:invert-direction
   #:make-core
   #:progress
   #:rng
   #:sample
   #:tile-map
   #:uncollapsed-count))

(defpackage #:%syntex.wfc.history
  (:local-nicknames
   (#:cond #:%syntex.conditions)
   (#:core #:%syntex.wfc.core)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:advance-time
   #:backtrack
   #:history
   #:make-history
   #:record))

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
   (#:core #:%syntex.wfc.core)
   (#:grid #:%syntex.wfc.grid)
   (#:kernel #:%syntex.wfc.kernel)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:analyze
   #:get-count
   #:get-frequency
   #:get-origin-color))

(defpackage #:%syntex.wfc.tile-map
  (:local-nicknames
   (#:cond #:%syntex.conditions)
   (#:core #:%syntex.wfc.core)
   (#:grid #:%syntex.wfc.grid)
   (#:hist #:%syntex.wfc.history)
   (#:pat #:%syntex.wfc.pattern)
   (#:pq #:%syntex.priority-queue)
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
   #:remove-possible-pattern))

(defpackage #:%syntex.wfc.solver
  (:local-nicknames
   (#:core #:%syntex.wfc.core)
   (#:grid #:%syntex.wfc.grid)
   (#:hist #:%syntex.wfc.history)
   (#:pq #:%syntex.priority-queue)
   (#:tm #:%syntex.wfc.tile-map)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:solve))

(defpackage #:%syntex.wfc
  (:local-nicknames
   (#:core #:%syntex.wfc.core)
   (#:cond #:%syntex.conditions)
   (#:grid #:%syntex.wfc.grid)
   (#:hist #:%syntex.wfc.history)
   (#:img #:%syntex.image)
   (#:com #:%syntex.common)
   (#:pat #:%syntex.wfc.pattern)
   (#:sample #:%syntex.wfc.sample)
   (#:solver #:%syntex.wfc.solver)
   (#:tm #:%syntex.wfc.tile-map)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:wfc))
