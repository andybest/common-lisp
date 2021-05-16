(in-package #:cl-user)

(defpackage #:%syntex.wfc.core
  (:local-nicknames
   (#:grid #:%syntex.wfc.grid)
   (#:kernel #:%syntex.wfc.kernel)
   (#:pat #:%syntex.wfc.pattern)
   (#:sample #:%syntex.wfc.sample)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export))

(in-package #:%syntex.wfc.core)

(defstruct (core
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (sample nil :type grid:grid)
  (patterns (pat:make-pattern-collection) :type pat:pattern-collection)
  (frequencies (make-array 0 :fill-pointer 0 :adjustable t) :type vector)
  (origin-colors (u:dict #'eql) :type hash-table))

(defun get-frequency (core id)
  (aref (frequencies core) id))

(defun generate-patterns (core &key size periodic-p)
  (pat:extract (patterns core)
               (frequencies core)
               (sample core)
               :size size
               :periodic-p periodic-p))

(defun generate-origin-colors (core)
  (let ((origin-colors (origin-colors core)))
    (map nil
         (lambda (x)
           (let ((id (pat:id x))
                 (color (pat:get-origin-color x)))
             (setf (u:href origin-colors id) color)))
         (pat:id->pattern (patterns core)))))

(defun wfc (sample-path &key (pattern-size 2) (periodic-p t))
  (let* ((sample (sample:load sample-path))
         (core (make-core :sample sample)))
    (generate-patterns core :size pattern-size :periodic-p periodic-p)
    (generate-origin-colors core)
    core))
