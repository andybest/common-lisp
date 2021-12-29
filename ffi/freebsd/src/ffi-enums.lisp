(in-package #:mfiano.ffi.freebsd)

;; Flags that are passed to OPEN() and OPENAT().
;; We also define some additional Lispy synonyms for the user.
(c:defbitfield open-flags
  (:rdonly #x0) (:read-only #x0)
  :wronly (:write-only #x1)
  :rdwr (:read-write #x2)
  :nonblock (:non-block #x4)
  :append
  :shlock (:shared-lock #x10)
  :exlock (:exclusive-lock #x20)
  :async
  :sync (:fsync #x80)
  :nofollow (:no-follow #x100)
  :creat (:create #x200)
  :trunc (:truncate #x400)
  :excl (:exclusive #x800)
  (:noctty #x8000) ; ignored
  :direct
  :directory
  :exec (:search #x40000)
  :tty-init ;ignored
  :cloexec (:close-on-exec #x100000)
  :verify
  :path
  :resolve-beneath
  :dsync
  :empty-path)

(c:defcenum (ioctl-request :ulong)
  ;; generic
  (:fionread 1074030207)
  (:fionwrite 1074030199)
  (:fionspace 1074030198)
  ;; terminal
  (:tiocsetd 2147775515)
  (:tiocgetd 1074033690)
  (:tiocsbrk 536900731)
  (:tioccbrk 536900730)
  (:tiocsdtr 536900729)
  (:tioccdtr 536900728)
  (:tiocgpgrp 1074033783)
  (:tiocspgrp 2147775606)
  (:tiocgeta 1076655123)
  (:tiocseta 2150396948)
  (:tiocsetaw 2150396949)
  (:tiocsetaf 2150396950)
  (:tiocoutq 1074033779)
  (:tiocsti 2147578994)
  (:tiocnotty 536900721)
  (:tiocstop 536900719)
  (:tiocstart 536900718)
  (:tiocsctty 536900705)
  (:tiocdrain 536900702)
  (:tiocgdrainwait 1074033750)
  (:tiocsdrainwait 2147775575)
  (:tiocexcl 536900621)
  (:tiocnxcl 536900622)
  (:tiocflush 2147775504)
  (:tiocgwinsz 1074295912)
  (:tiocswinsz 2148037735)
  (:tioccons 2147775586)
  (:tiocmset 2147775597)
  (:tiocmget 1074033770)
  (:tiocmbis 2147775596)
  (:tiocmbic 2147775595))
