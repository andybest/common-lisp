(in-package :defpackage+-user-1)

(defpackage+ #:box.math.base
  (:inherit #:cl)
  (:export #:+epsilon+
           #:%make-accessor-symbol
           #:%~))
