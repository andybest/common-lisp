;; Test an asdf system
#+asdf
(defun :test (system)
  (asdf:test-system system))

;; Convenience function to force a garbage collection
#+sbcl
(defun :gc ()
  (sb-ext:gc :full t))

;; Convenience macro to benchmark some code
#+sbcl
(defmacro :bench ((count) &body body)
  `(progn
     (:gc)
     (time (loop :repeat ,count :do (progn ,@body)))))

;; Pretty-print hash tables
(labels ((pretty-print-hash-table (stream table)
           (let ((*print-pretty* t)
                 (i 0))
             (pprint-logical-block (stream nil)
               (princ "#{" stream)
               (pprint-indent :block 2 stream)
               (unless (zerop (hash-table-count table))
                 (block nil
                   (let ((j 0))
                     (maphash (lambda (k v)
                                (princ " " stream)
                                (when (and *print-length* (> (incf i) *print-length*))
                                  (princ "..." stream)
                                  (return))
                                (when (and k (listp k)) (princ #\' stream))
                                (if (typep k 'hash-table)
                                    (pretty-print-hash-table stream k)
                                    (prin1 k stream))
                                (princ " " stream)
                                (when (and v (listp v))
                                  (princ #\' stream))
                                (if (typep v 'hash-table)
                                    (pretty-print-hash-table stream v)
                                    (progn (prin1 v stream)
                                           (princ " " stream)))
                                (incf j)
                                (unless (= (hash-table-count table) j)
                                  (pprint-newline :mandatory stream))
                                )
                              table))))
               (princ "}" stream))
             table)))
  (set-pprint-dispatch 'hash-table #'pretty-print-hash-table))
