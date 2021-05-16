(in-package #:cl-user)

(defpackage #:%syntex.wfc.core
  (:local-nicknames
   (#:adj #:%syntex.wfc.adjacency)
   (#:grid #:%syntex.wfc.grid)
   (#:pat #:%syntex.wfc.pattern)
   (#:sample #:%syntex.wfc.sample)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:wfc))

(in-package #:%syntex.wfc.core)

(defstruct (core
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (sample nil :type grid:grid)
  (patterns (pat:make-pattern-collection) :type pat:pattern-collection)
  (frequencies (make-array 0 :fill-pointer 0 :adjustable t) :type vector)
  (adjacencies (u:dict #'eql) :type hash-table)
  (origin-colors (u:make-ub32-array 0) :type u:ub32a))

(defun get-frequency (core id)
  (aref (frequencies core) id))

(defun generate-patterns (core &key size periodic-p)
  (pat:extract (patterns core)
               (frequencies core)
               (sample core)
               :size size
               :periodic-p periodic-p))

(defun generate-origin-colors (core)
  (let* ((patterns (patterns core))
         (pattern-count (pat:get-count patterns))
         (origin-colors (u:make-ub32-array pattern-count)))
    (dotimes (id pattern-count)
      (let* ((pattern (pat:get-pattern patterns id))
             (color (pat:get-origin-color pattern)))
        (setf (aref origin-colors id) color)))
    (setf (origin-colors core) origin-colors)))

(defun generate-adjacencies (core &key pattern-size)
  (let* ((patterns (patterns core))
         (adjacencies (adjacencies core)))
    (adj:generate adjacencies patterns pattern-size)))

(defun wfc (sample-path &key (pattern-size 2) (periodic-p t))
  (let* ((sample (sample:load sample-path))
         (core (make-core :sample sample)))
    (generate-patterns core :size pattern-size :periodic-p periodic-p)
    (generate-origin-colors core)
    (generate-adjacencies core :pattern-size pattern-size)
    core))
