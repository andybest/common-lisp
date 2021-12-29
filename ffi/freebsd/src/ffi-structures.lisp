(in-package #:mfiano.ffi.freebsd)

(c:defcstruct clock-info
  (:hz :int)
  (:tick :int)
  (:stat-hz :int :offset 12)
  (:prof-hz :int))

(c:defcstruct window-size
  (:rows :ushort)
  (:columns :ushort)
  (:pixels-x :ushort)
  (:pixels-y :ushort))
