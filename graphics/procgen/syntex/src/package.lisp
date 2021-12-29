(in-package #:cl-user)

(defpackage #:%mfiano.graphics.procgen.syntex.conditions
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

(defpackage #:%mfiano.graphics.procgen.syntex.image
  (:local-nicknames
   (#:png #:mfiano.file-formats.png)
   (#:u #:mfiano.misc.utils))
  (:use #:cl)
  (:shadow
   #:write)
  (:export
   #:data
   #:from-rgb
   #:height
   #:make-image
   #:unpack
   #:width
   #:write))

(defpackage #:%mfiano.graphics.procgen.syntex.priority-queue
  (:local-nicknames
   (#:u #:mfiano.misc.utils))
  (:use #:cl)
  (:export
   #:copy
   #:dequeue
   #:enqueue
   #:make-queue
   #:peek
   #:queue))

(defpackage #:%mfiano.graphics.procgen.syntex.common
  (:local-nicknames
   (#:cond #:%mfiano.graphics.procgen.syntex.conditions))
  (:use #:cl)
  (:export
   #:check-file-exists
   #:check-image-dimension
   #:check-output-path
   #:check-seed))

(defpackage #:%mfiano.graphics.procgen.syntex.harrison
  (:local-nicknames
   (#:com #:%mfiano.graphics.procgen.syntex.common)
   (#:cond #:%mfiano.graphics.procgen.syntex.conditions)
   (#:lp #:lparallel)
   (#:img #:%mfiano.graphics.procgen.syntex.image)
   (#:rng #:mfiano.misc.rng)
   (#:u #:mfiano.misc.utils))
  (:use #:cl)
  (:export
   #:harrison))

(defpackage #:%mfiano.graphics.procgen.syntex.wfc
  (:local-nicknames
   (#:com #:%mfiano.graphics.procgen.syntex.common)
   (#:cond #:%mfiano.graphics.procgen.syntex.conditions)
   (#:img #:%mfiano.graphics.procgen.syntex.image)
   (#:pq #:%mfiano.graphics.procgen.syntex.priority-queue)
   (#:rng #:mfiano.misc.rng)
   (#:u #:mfiano.misc.utils))
  (:use #:cl)
  (:export
   #:wfc))

(uiop:define-package #:mfiano.graphics.procgen.syntex
  (:use #:cl)
  (:mix-reexport
   #:%mfiano.graphics.procgen.syntex.conditions
   #:%mfiano.graphics.procgen.syntex.harrison
   #:%mfiano.graphics.procgen.syntex.wfc))
