(in-package #:net.mfiano.lisp.origin)

;;; The functions in this file are still in flux -- please do not rely on them.

;; (u:fn-> line-plane-intersect (v3:vec v3:vec v3:vec v3:vec) v3:vec)
;; (u:defun-inline line-plane-intersect (line-point1
;;                                       line-point2
;;                                       plane-point
;;                                       plane-normal)
;;   (declare (optimize speed))
;;   (let* ((dir (line-direction line-point1 line-point2))
;;          (dir-dot-plane (v3:dot dir plane-normal))
;;          (plane-line (v3:- line-point1 plane-point)))
;;     (if (zerop dir-dot-plane)
;;         (v3:zero)
;;         (let ((dist (/ (- (v3:dot plane-normal plane-line)) dir-dot-plane)))
;;           (translate-point line-point1 dir dist)))))

;; (u:fn-> line-point-distance (v3:vec v3:vec v3:vec) u:f32)
;; (u:defun-inline line-point-distance (line-point1 line-point2 point)
;;   (declare (optimize speed))
;;   (let* ((dir (line-direction line-point1 line-point2))
;;          (intersect (line-plane-intersect line-point1 line-point2 point dir)))
;;     (v3:distance point intersect)))
