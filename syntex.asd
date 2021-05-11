(asdf:defsystem #:syntex
  :description ""
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "MIT"
  :homepage "https://git.mfiano.net/mfiano/syntex"
  :encoding :utf-8
  :depends-on (#:cl-cpus
               #:golden-utils
               #:lparallel
               #:pngload
               #:seedable-rng
               #:uiop
               #:zpng)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "conditions")
   (:file "common")
   (:file "image")
   (:module "synthesizers"
    :components
    ((:file "harrison")
     (:module "wfc"
      :components
      (
       ;; base
       (:file "point")
       (:file "tile")
       (:file "tile-propagator-tile-set")

       ;; rot
       (:file "transform")
       (:file "transform-tile")
       (:file "transform-subgroup")
       (:file "transform-builder")

       ;; topo
       (:file "periodicity")
       (:file "direction")
       (:file "topology")
       (:file "topology-grid")
       (:file "topology-data")
       (:file "topology-data-1d")
       (:file "topology-data-2d")
       (:file "topology-data-3d")

       ;; wfc
       (:file "deque")
       (:file "pattern-model")
       (:file "wave")

       ;; models
       (:file "model-pattern-array")
       (:file "model-overlapping-analysis")
       (:file "model-tile-mapping")
       (:file "model-tile")
       (:file "model-adjacent")
       (:file "model-graph-adjacent")
       (:file "model-overlapping")

       ;; trackers
       (:file "tracker")
       (:file "tracker-random-picker")
       (:file "tracker-ordered-random-picker")
       (:file "tracker-frequency-set")
       (:file "tracker-entropy")
       (:file "tracker-array-priority-entropy")
       ))))))
