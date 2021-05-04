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
      ((:file "point") ;; debroglie
       (:file "periodicity") ;; mine
       (:file "direction") ;; topo
       (:file "topology") ;; topo
       (:file "topology-data") ;; topo
       (:file "topology-data-1d")
       (:file "topology-data-2d")
       (:file "topology-data-3d")
       (:file "grid") ;; topo
       (:file "orientation") ;; rot
       (:file "deque") ;; wfc
       (:file "wave") ;; wfc
       ))))))
