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
(defmacro :bench (count &body body)
  `(progn
     (:gc)
     (time (loop :repeat ,count :do ,@body))))

