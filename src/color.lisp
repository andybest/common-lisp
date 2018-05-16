(in-package :umbra.color)

;;;; Color space conversion
;;;; Credits:
;;;; Ian Taylor http://www.chilliant.com/rgb2hsv.html
;;;; Jason Summers http://entropymine.com/imageworsener/srgbformula
;;;; https://en.wikipedia.org/wiki/SRGB
;;;; https://en.wikipedia.org/wiki/CIE_1931_color_space

(defconstant +epsilon+ 1e-20)

;;; Grayscale
;;; Uses the ITU-R Recommendation BT.709 standard for its luma coefficients.

(defun-gpu rgb->grayscale ((color :vec4))
  (let ((intensity (v3:make (+ (* (.r color) 0.2126)
                               (* (.g color) 0.7152)
                               (* (.b color) 0.0722)))))
    (v4:make intensity (.a color))))

;;; Hue

(defun-gpu hue->rgb ((hue :float))
  (let ((v (* hue 6)))
    (umbra.math:saturate
     (v4:make (1- (abs (- v 3)))
              (- 2 (abs (- v 2)))
              (- 2 (abs (- v 4)))
              1.0))))

;;; HCV (hue/chroma/value)

(defun-gpu rgb->hcv ((color :vec4))
  (let* ((k (v4:make 0 (/ -1 3.0) (/ 2 3.0) -1))
         (p (if (< (.g color) (.b color))
                (v4:make (.bg color) (.wz k))
                (v4:make (.gb color) (.xy k))))
         (q (if (< (.r color) (.x p))
                (v4:make (.xyw p) (.r color))
                (v4:make (.r color) (.yzx p))))
         (d (- (.x q) (min (.w q) (.y q))))
         (h (abs (+ (/ (- (.w q) (.y q))
                       (+ (* 6 d) +epsilon+))
                    (.z q)))))
    (v4:make h d (.x q) (.a color))))

;;; HSV (hue/saturation/value)

(defun-gpu rgb->hsv ((color :vec4))
  (let* ((hcv (.xyz (rgb->hcv color)))
         (s (/ (.y hcv) (+ (.z hcv) +epsilon+))))
    (v4:make (.x hcv) s (.z hcv) (.a color))))

(defun-gpu hsv->rgb ((color :vec4))
  (let ((rgb (.rgb (hue->rgb (.x color)))))
    (v4:make (* (1+ (* (1- rgb) (.y color))) (.z color))
             (.a color))))

;;; HCY (hue/saturation/luminance)

(defun-gpu rgb->hcy ((color :vec4))
  (let* ((hcy-weights (v3:make 0.299 0.587 0.114))
         (hcv (.xyz (rgb->hcv color)))
         (y (dot (.rgb color) hcy-weights))
         (z (dot (.rgb (hue->rgb (.x hcv))) hcy-weights)))
    (if (< y z)
        (multf (.y hcv) (/ z (+ +epsilon+ y)))
        (multf (.y hcv) (/ (- 1 z) (- (1+ +epsilon+) y))))
    (v4:make (.xy hcv) y (.a color))))

(defun-gpu hcy->rgb ((color :vec4))
  (let* ((hcy-weights (v3:make 0.299 0.587 0.114))
         (rgb (.rgb (hue->rgb (.x color))))
         (z (dot rgb hcy-weights)))
    (cond
      ((< (.z color) z)
       (multf (.y color) (/ (.z color) z)))
      ((< z 1)
       (multf (.y color) (/ (- 1 (.z color)) (- 1 z)))))
    (v4:make (+ (* (- rgb z) (.y color)) (.z color)) (.a color))))

;;; HSL (hue/saturation/lightness)

(defun-gpu rgb->hsl ((color :vec4))
  (let* ((hcv (.xyz (rgb->hcv color)))
         (l (- (.z hcv) (* (.y hcv) 0.5)))
         (s (/ (.y hcv) (- 1 (+ (abs (1- (* l 2)))) +epsilon+))))
    (v4:make (.x hcv) s l (.a color))))

(defun-gpu hsl->rgb ((color :vec4))
  (let* ((rgb (.rgb (hue->rgb (.x color))))
         (c (* (- 1 (abs (1- (* 2 (.z color))))) (.y color))))
    (v4:make (+ (* (- rgb 0.5) c) (.z color)) (.a color))))

;;; SRGB

(defconstant +srgb-gamma+ (/ 2.2))
(defconstant +srgb-gamma-inverse+ 2.2)

(defun-gpu rgb->srgb-approx ((color :vec4))
  (v4:make (expt (.rgb color) (v3:make +srgb-gamma+)) (.a color)))

(defun-gpu rgb->srgb ((color :vec4))
  (let ((rgb (.rgb color)))
    (v4:make
     (mix (* 12.92 rgb)
          (- (* 1.055 (expt rgb (v3:make (/ 2.4)))) 0.055)
          (step (v3:make 0.0031308) rgb))
     (.a color))))

(defun-gpu srgb->rgb-approx ((color :vec4))
  (v4:make (expt (.rgb color) (v3:make +srgb-gamma-inverse+)) (.a color)))

(defun-gpu srgb->rgb ((color :vec4))
  (let ((rgb (.rgb color)))
    (v4:make
     (mix (/ rgb 12.92)
          (expt (/ (+ rgb 0.055) 1.055) (v3:make 2.4))
          (step (v3:make 0.04045) rgb))
     (.a color))))

;;; CIE XYZ 1931

(defun-gpu rgb->xyz ((color :vec4))
  (let ((transform (m3:make 0.4124564 0.3575761 0.1804375
                            0.2126729 0.7151522 0.0721750
                            0.0193339 0.1191920 0.9503041)))
    (v4:make (* transform (.rgb color)) (.a color))))

(defun-gpu xyz->rgb ((color :vec4))
  (let ((transform (m3:make 3.2404542 -1.5371385 -0.4985314
                            -0.9692660 1.8760108 0.0415560
                            0.0556434 -0.2040259 1.0572252)))
    (v4:make (* transform (.xyz color)) (.a color))))

;;; CIE xyY

(defun-gpu xyy->xyz ((color :vec4))
  (let* ((xyy (.xyz color))
         (y (.z xyy))
         (x (/ (* y (.x xyy)) (.y xyy)))
         (z (/ (* y (- 1 (.x xyy) (.y xyy))) (.y xyy))))
    (v4:make x y z (.a color))))

(defun-gpu xyz->xyy ((color :vec4))
  (let* ((xyz (.xyz color))
         (v (+ (.x xyz) (.y xyz) (.z xyz)))
         (x (/ (.x xyz) v))
         (y (/ (.y xyz) v)))
    (v4:make x y (.y xyz) (.a color))))

(defun-gpu rgb->xyy ((color :vec4))
  (xyz->xyy (rgb->xyz color)))

(defun-gpu xyy->rgb ((color :vec4))
  (xyz->rgb (xyy->xyz color)))
