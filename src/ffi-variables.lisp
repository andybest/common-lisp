(in-package #:mfiano.ffi.freebsd)

(c:defcvar ("__stderrp" +stderr+ :read-only t) :pointer)
(c:defcvar ("__stdinp" +stdin+ :read-only t) :pointer)
(c:defcvar ("__stdoutp" +stdout+ :read-only t) :pointer)
