(in-package :umbra.color)

;;; Exposure

(defun-gpu set-exposure ((color :vec3)
                         (exposure :int))
  (* color (expt 2 exposure)))

(defun-gpu set-exposure ((color :vec4)
                         (exposure :int))
  (v4:make (set-exposure (.rgb color) exposure) (.a color)))

;;; Saturation

(defun-gpu set-saturation ((color :vec3)
                           (saturation :float))
  (mix (rgb->grayscale color) color saturation))

(defun-gpu set-saturation ((color :vec4)
                           (saturation :float))
  (v4:make (set-saturation (.rgb color) saturation) (.a color)))

;;; Contrast

(defun-gpu set-contrast ((color :vec3)
                         (contrast :float))
  (+ (* (- (.rgb color) 0.5) contrast) 0.5))

(defun-gpu set-contrast ((color :vec4)
                         (contrast :float))
  (v4:make (set-contrast (.rgb color) contrast) (.a color)))

;;; Brightness

(defun-gpu set-brightness ((color :vec3)
                           (brightness :float))
  (+ color brightness))

(defun-gpu set-brightness ((color :vec4)
                           (brightness :float))
  (v4:make (set-brightness (.rgb color) brightness) (.a color)))

;;; Gamma

(defun-gpu set-gamma ((color :vec3)
                      (gamma :float))
  (v3:make (expt (abs (.r color)) (/ gamma))
           (expt (abs (.g color)) (/ gamma))
           (expt (abs (.b color)) (/ gamma))))

(defun-gpu set-gamma ((color :vec4)
                      (gamma :float))
  (v4:make (set-gamma (.rgb color) gamma) (.a color)))

;;; Color filter

(defun-gpu color-filter ((color :vec3)
                         (filter :vec3)
                         (exposure :int))
  (let ((exposure (set-exposure color exposure)))
    (* color filter exposure)))

(defun-gpu color-filter ((color :vec4)
                         (filter :vec3)
                         (exposure :int))
  (v4:make (color-filter (.rgb color) filter exposure) (.a color)))

;;; Tone mapping
;;; http://filmicworlds.com/blog/filmic-tonemapping-operators/

(defun-gpu tone-map-linear ((color :vec3)
                            (exposure :int))
  (set-gamma (set-exposure color exposure) +gamma+))

(defun-gpu tone-map-linear ((color :vec4)
                            (exposure :int))
  (v4:make (tone-map-linear (.rgb color) exposure) (.a color)))

(defun-gpu tone-map-reinhard ((color :vec3)
                              (exposure :int))
  (let ((color (set-exposure color exposure)))
    (set-gamma (/ color (1+ color)) +gamma+)))

(defun-gpu tone-map-reinhard ((color :vec4)
                              (exposure :int))
  (v4:make (tone-map-reinhard (.rgb color) exposure) (.a color)))

(defun-gpu tone-map-haarm-peter-duiker ((color :vec3)
                                        (exposure :int)
                                        (film-lut :sampler-2d))
  (let* ((color (set-exposure color exposure))
         (log-color (saturate (/ (+ (* (/ (log10 (* 0.4 color)) 0.002) 0.45) 444) 1023.0)))
         (padding (/ 0.5 256))
         (r (v2:make (mix padding (- 1 padding) (.r color)) 0.5))
         (g (v2:make (mix padding (- 1 padding) (.g color)) 0.5))
         (b (v2:make (mix padding (- 1 padding) (.b color)) 0.5)))
    (v3:make (.r (texture film-lut r))
             (.r (texture film-lut g))
             (.r (texture film-lut b)))))

(defun-gpu tone-map-haarm-peter-duiker ((color :vec4)
                                        (exposure :int)
                                        (film-lut :sampler-2d))
  (v4:make (tone-map-haarm-peter-duiker (.rgb color) exposure film-lut) (.a color)))

(defun-gpu tone-map-hejl-burgess-dawson ((color :vec3)
                                         (exposure :int))
  (let* ((color (set-exposure color exposure))
         (x (max (v3:make 0) (- color 0.004)))
         (y (* 6.2 x)))
    (/ (* x (+ y 0.5))
       (+ (* x (+ y 1.7)) 0.06))))

(defun-gpu tone-map-hejl-burgess-dawson ((color :vec4)
                                         (exposure :int))
  (v4:make (tone-map-hejl-burgess-dawson (.rgb color) exposure) (.a color)))

(defun-gpu tone-map-uncharted2 ((color :vec3)
                                (exposure :int))
  (flet ((tone-map ((x :vec3))
           (- (/ (+ (* x (+ (* 0.15 x) 0.05)) 0.004)
                 (+ (* x (+ (* 0.15 x) 0.5)) 0.06))
              0.006)))
    (expt (* (tone-map (* 2 (set-exposure color exposure)))
             (/ (tone-map (v3:make 11.2))))
          (v3:make +gamma+))))

(defun-gpu tone-map-uncharted2 ((color :vec4)
                                (exposure :int))
  (v4:make (tone-map-uncharted2 (.rgb color) exposure) (.a color)))
