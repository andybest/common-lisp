(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.origin
  (:local-nicknames
   (#:com #:net.mfiano.lisp.origin.common)
   (#:m4 #:net.mfiano.lisp.origin.mat4)
   (#:q #:net.mfiano.lisp.origin.quat)
   (#:u #:net.mfiano.lisp.golden-utils)
   (#:v2 #:net.mfiano.lisp.origin.vec2)
   (#:v3 #:net.mfiano.lisp.origin.vec3)
   (#:v4 #:net.mfiano.lisp.origin.vec4))
  (:use #:cl)
  (:shadowing-import-from
   #:net.mfiano.lisp.origin.common
   #:+rad+
   #:+rad/double+
   #:+deg+
   #:+deg/double+
   #:2pi
   #:2pi/3
   #:2pi/12
   #:3pi/2
   #:3pi/4
   #:3pi/12
   #:4pi/3
   #:4pi/12
   #:5pi/3
   #:5pi/4
   #:5pi/6
   #:5pi/12
   #:6pi/12
   #:7pi/4
   #:7pi/6
   #:7pi/12
   #:8pi/12
   #:9pi/12
   #:10pi/12
   #:11pi/6
   #:11pi/12
   #:12pi/12
   #:13pi/12
   #:14pi/12
   #:15pi/12
   #:16pi/12
   #:17pi/12
   #:18pi/12
   #:19pi/12
   #:20pi/12
   #:21pi/12
   #:22pi/12
   #:23pi/12
   #:24pi/12
   #:pi
   #:pi/2
   #:pi/3
   #:pi/4
   #:pi/6
   #:pi/12)
  ;; constants
  (:export
   #:+rad+
   #:+deg+
   #:2pi
   #:2pi/3
   #:2pi/12
   #:3pi/2
   #:3pi/4
   #:3pi/12
   #:4pi/3
   #:4pi/12
   #:5pi/3
   #:5pi/4
   #:5pi/6
   #:5pi/12
   #:6pi/12
   #:7pi/4
   #:7pi/6
   #:7pi/12
   #:8pi/12
   #:9pi/12
   #:10pi/12
   #:11pi/6
   #:11pi/12
   #:12pi/12
   #:13pi/12
   #:14pi/12
   #:15pi/12
   #:16pi/12
   #:17pi/12
   #:18pi/12
   #:19pi/12
   #:20pi/12
   #:21pi/12
   #:22pi/12
   #:23pi/12
   #:24pi/12
   #:pi
   #:pi/2
   #:pi/3
   #:pi/4
   #:pi/6
   #:pi/12)
  ;; shaping
  (:export
   #:linear
   #:sine-out
   #:sine-in
   #:sine-in-out
   #:quadratic-out
   #:quadratic-in
   #:quadratic-in-out
   #:cubic-out
   #:cubic-in
   #:cubic-in-out
   #:quartic-out
   #:quartic-in
   #:quartic-in-out
   #:quintic-out
   #:quintic-in
   #:quintic-in-out
   #:exponential-out
   #:exponential-in
   #:exponential-in-out
   #:circular-out
   #:circular-in
   #:circular-in-out
   #:back-out
   #:back-in
   #:back-in-out
   #:elastic-out
   #:elastic-in
   #:elastic-in-out
   #:bounce-out
   #:bounce-in
   #:bounce-in-out
   #:hermite-curve
   #:quintic-curve))
