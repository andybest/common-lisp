(in-package #:freebsd-tools.cpu)

(defun get-cpu-count ()
  (bsd:with-sysctl-by-name (ptr "hw.ncpu")
    (c:mem-ref ptr :int)))

(defun get-cpu-ticks/total ()
  (bsd:with-sysctl-by-name (ptr "kern.cp_time")
    (map 'list #'identity (c:foreign-array-to-lisp ptr '(:array :long 5) :element-type 'u:b64))))

(defun get-cpu-ticks/per-cpu ()
  (bsd:with-sysctl-by-name (ptr "kern.cp_times")
    (let* ((cpu-count (get-cpu-count))
           (foreign-type `(:array :long ,(* cpu-count 5)))
           (times (c:foreign-array-to-lisp ptr foreign-type :element-type 'u:b64)))
      (u:batches (map 'list #'identity times) 5))))
