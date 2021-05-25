(in-package #:cl-user)

(defpackage #:%syntex.conditions
  (:use #:cl)
  (:export
   #:file-not-found
   #:invalid-dimension
   #:invalid-harrison-candidate-count
   #:invalid-harrison-rounds
   #:invalid-harrison-kernel-size
   #:invalid-output-path
   #:invalid-seed
   #:invalid-wfc-backtrack-distance
   #:invalid-wfc-backtrack-retry-count
   #:invalid-wfc-pattern-size
   #:invalid-wfc-strategy
   #:syntex-error
   #:wfc-contradiction-error
   #:wfc-contradiction-warning
   #:wfc-max-backtrack-retries-exceeded))

(defpackage #:%syntex.image
  (:local-nicknames
   (#:png #:pngload)
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow
   #:write)
  (:export
   #:data
   #:from-argb
   #:height
   #:make-image
   #:unpack
   #:width
   #:write))

(defpackage #:%syntex.priority-queue
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:copy
   #:dequeue
   #:enqueue
   #:make-queue
   #:peek
   #:queue))

(defpackage #:%syntex.common
  (:local-nicknames
   (#:cond #:%syntex.conditions))
  (:use #:cl)
  (:export
   #:check-file-exists
   #:check-image-dimension
   #:check-output-path
   #:check-seed))

(defpackage #:%syntex.harrison
  (:local-nicknames
   (#:com #:%syntex.common)
   (#:cond #:%syntex.conditions)
   (#:lp #:lparallel)
   (#:img #:%syntex.image)
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:harrison))

(defpackage #:%syntex.wfc
  (:local-nicknames
   (#:com #:%syntex.common)
   (#:cond #:%syntex.conditions)
   (#:img #:%syntex.image)
   (#:pq #:%syntex.priority-queue)
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:wfc))

(uiop:define-package #:syntex
  (:use #:cl)
  (:mix-reexport
   #:%syntex.conditions
   #:%syntex.harrison
   #:%syntex.wfc))
