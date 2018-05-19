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
