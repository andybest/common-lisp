(in-package #:freebsd-tools.clmem)

(defun get-page-size ()
  (bsd:with-sysctl-by-name (ptr "hw.pagesize")
    (c:mem-ref ptr :int)))

(defun get-wired-count ()
  (bsd:with-sysctl-by-name (ptr "vm.stats.vm.v_wire_count")
    (c:mem-ref ptr :uint)))

(defun get-active-count ()
  (bsd:with-sysctl-by-name (ptr "vm.stats.vm.v_active_count")
    (c:mem-ref ptr :uint)))

(defun get-zfs-arc-size ()
  (bsd:with-sysctl-by-name (ptr "kstat.zfs.misc.arcstats.size")
    (c:mem-ref ptr :uint64)))

(defun get-total-memory ()
  (bsd:with-sysctl-by-name (ptr "hw.physmem")
    (c:mem-ref ptr :ulong)))

(defun bytes-to-gb (bytes)
  (* bytes #.(expt 1024.0 -3)))

(defun print-percentage (percentage)
  (let* ((precision (lib:get-option 'precision))
         (string (string-right-trim '(#\.) (format nil "~,vf" precision percentage))))
    (write-string string)
    (when (lib:get-option 'suffixes)
      (write-char #\%))))

(defun print-usage (used total)
  (let* ((precision (lib:get-option 'precision))
         (suffixes (lib:get-option 'suffixes))
         (used-string (format nil "~,vf" precision used))
         (total-string (format nil "~,vf" precision total)))
    (format t " (~a" used-string)
    (when suffixes
      (write-char #\G))
    (format t " / ~a" total-string)
    (when suffixes
      (write-char #\G))
    (format t ")")))

(defun print-report ()
  (let* ((page-size (get-page-size))
         (wired (* (get-wired-count) page-size))
         (active (* (get-active-count) page-size))
         (arc (get-zfs-arc-size))
         (total (get-total-memory))
         (used (+ (- wired arc) active)))
    (print-percentage (* (/ used total) 100.0))
    (when (lib:get-option 'verbose)
      (print-usage (bytes-to-gb used) (bytes-to-gb total)))))
