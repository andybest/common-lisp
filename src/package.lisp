(in-package :defpackage+-user-1)

(defpackage+ #:box.math.base
  (:use #:cl)
  (:export #:+epsilon+
           #:%make-accessor-symbol
           #:%~))
