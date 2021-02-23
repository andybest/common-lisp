(in-package #:%coherent-noise.map)

(deftype color () '(simple-array u:ub8 (4)))

(declaim (inline rgba))
(defstruct (color
            (:type (vector u:ub8))
            (:constructor rgba (r g b a))
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (r 0 :type u:ub8)
  (g 0 :type u:ub8)
  (b 0 :type u:ub8)
  (a 0 :type u:ub8))

(u:fn-> get-color-channels (color) (values u:ub8 u:ub8 u:ub8 u:ub8))
(declaim (inline get-color-channels))
(defun get-color-channels (color)
  (values (r color) (g color) (b color) (a color)))

(declaim (inline blend-colors))
(defun blend-colors (color1 color2 alpha)
  (flet ((lerp (channel1 channel2)
           (let ((channel1 (* channel1 #.(/ 255.0)))
                 (channel2 (* channel2 #.(/ 255.0))))
             (values (truncate (* (u:lerp alpha channel1 channel2) 255.0))))))
    (declare (inline lerp))
    (let ((r (lerp (r color1) (r color2)))
          (g (lerp (g color1) (g color2)))
          (b (lerp (b color1) (b color2)))
          (a (lerp (a color1) (a color2))))
      (values r g b a))))
