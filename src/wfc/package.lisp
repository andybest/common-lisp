(in-package #:cl-user)

(defpackage #:%syntex.wfc.image
  (:local-nicknames
   (#:png #:pngload)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:data
   #:image
   #:height
   #:make-image
   #:unpack
   #:width))

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
   #:x
   #:y))

(defpackage #:%syntex.wfc.sample
  (:local-nicknames
   (#:grid #:%syntex.wfc.grid)
   (#:img #:%syntex.wfc.image)
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
   #:extract
   #:get-count
   #:get-origin-color
   #:get-pattern
   #:grid
   #:id
   #:make-pattern
   #:make-pattern-collection
   #:pattern-collection
   #:reflect-p
   #:rotation
   #:size
   #:x
   #:y))

(defpackage #:%syntex.wfc.core
  (:local-nicknames
   (#:grid #:%syntex.wfc.grid)
   (#:pat #:%syntex.wfc.pattern)
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:adjacencies
   #:contradiction
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
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:generate
   #:invert-edge))

(defpackage #:%syntex.wfc.tile-map
  (:local-nicknames
   (#:core #:%syntex.wfc.core)
   (#:grid #:%syntex.wfc.grid)
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
   #:prepare
   #:remove-possible-pattern
   #:uncollapsed-count))

(defpackage #:%syntex.wfc
  (:local-nicknames
   (#:adj #:%syntex.wfc.adjacency)
   (#:core #:%syntex.wfc.core)
   (#:grid #:%syntex.wfc.grid)
   (#:pat #:%syntex.wfc.pattern)
   (#:pq #:damn-fast-priority-queue)
   (#:sample #:%syntex.wfc.sample)
   (#:tm #:%syntex.wfc.tile-map)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:wfc))
