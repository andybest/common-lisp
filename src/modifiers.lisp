(in-package #:coherent-noise/modifiers)

(defun + (sampler1 sampler2)
  "Sum the outputs of samplers `sampler1` and `sampler2`."
  (cn::check-modifier-input '+ 'sampler1 sampler1)
  (cn::check-modifier-input '+ 'sampler2 sampler2)
  (lambda (&rest args)
    (cl:+ (apply sampler1 args) (apply sampler2 args))))

(defun - (sampler1 sampler2)
  "Subtract the output of `sampler2` from the output of `sampler1`."
  (cn::check-modifier-input '- 'sampler1 sampler1)
  (cn::check-modifier-input '- 'sampler2 sampler2)
  (lambda (&rest args)
    (cl:- (apply sampler1 args) (apply sampler2 args))))

(defun * (sampler1 sampler2)
  "Multiply the output of `sampler1` by the output of `sampler2`."
  (cn::check-modifier-input '* 'sampler1 sampler1)
  (cn::check-modifier-input '* 'sampler2 sampler2)
  (lambda (&rest args)
    (cl:* (apply sampler1 args) (apply sampler2 args))))

(defun min (sampler1 sampler2)
  "Return the smallest value of the outputs of `sampler1` and `sampler2`."
  (cn::check-modifier-input 'min 'sampler1 sampler1)
  (cn::check-modifier-input 'min 'sampler2 sampler2)
  (lambda (&rest args)
    (cl:min (apply sampler1 args) (apply sampler2 args))))

(defun max (sampler1 sampler2)
  "Return the largest value of the outputs of `sampler1` and `sampler2`."
  (cn::check-modifier-input 'max 'sampler1 sampler1)
  (cn::check-modifier-input 'max 'sampler2 sampler2)
  (lambda (&rest args)
    (cl:max (apply sampler1 args) (apply sampler2 args))))

(defun blend (sampler1 sampler2 control-sampler)
  "Blend the outputs of `sampler1` and `sampler2` together, using the output of `control-sampler`.
This performs a linear interpolation."
  (cn::check-modifier-input 'blend 'sampler1 sampler1)
  (cn::check-modifier-input 'blend 'sampler2 sampler2)
  (cn::check-modifier-input 'blend 'control-sampler control-sampler)
  (lambda (&rest args)
    (u:lerp (cl:* (1+ (apply control-sampler args)) 0.5)
            (apply sampler1 args)
            (apply sampler2 args))))

(defun select (sampler1 sampler2 control-sampler &key (min -1.0) (max 1.0) (falloff 0.0))
  "Blend the outputs of `sampler1` and `sampler2` together, using the output of `control-sampler`.
Any output value of `control-sampler` that falls within the selection range defined by `min` and
`max` will select samples from `sampler1`. Values that fall outside of the selection range will
select samples from `sampler2`. By default, there is a hard transition between adjacent samples
selected from `sampler1` and `sampler2`. To smooth the transitions, increase the value of `falloff`,
which should be a value between 0 and 1."
  (cn::check-modifier-input 'select 'sampler1 sampler1)
  (cn::check-modifier-input 'select 'sampler2 sampler2)
  (cn::check-modifier-input 'select 'control-sampler control-sampler)
  (lambda (&rest args)
    (let* ((half-size (cl:* (cl:- max min) 0.5))
           (falloff (if (> falloff half-size) half-size falloff))
           (sample1 (apply sampler1 args))
           (sample2 (apply sampler2 args))
           (control (apply control-sampler args)))
      (if (plusp falloff)
          (cond
            ((< control (cl:- min falloff))
             sample1)
            ((< control (cl:+ min falloff))
             (let* ((low (cl:- min falloff))
                    (high (cl:+ min falloff))
                    (alpha (cn::interpolate/cubic (/ (cl:- control low) (cl:- high low)))))
               (u:lerp alpha sample1 sample2)))
            ((< control (cl:- max falloff))
             sample2)
            ((< control (cl:+ max falloff))
             (let* ((low (cl:- max falloff))
                    (high (cl:+ max falloff))
                    (alpha (cn::interpolate/cubic (/ (cl:- control low) (cl:- high low)))))
               (u:lerp alpha sample2 sample1)))
            (t
             sample1))
          (if (or (< control min) (> control max))
              sample1
              sample2)))))

(defun abs (sampler)
  "Take the absolute value of the output of `sampler`."
  (cn::check-modifier-input 'abs 'sampler sampler)
  (lambda (&rest args)
    (cl:abs (apply sampler args))))

(defun expt (sampler &optional (power 1.0))
  "Raise the output of `sampler` to the power `power`."
  (cn::check-modifier-input 'expt 'sampler sampler)
  (lambda (&rest args)
    (1- (cl:* (cl:expt (cl:abs (cl:* (1+ (apply sampler args)) 0.5)) power) 2))))

(defun power (sampler1 sampler2)
  "Raise the output of `sampler1` to the power of the output of `sampler2`."
  (cn::check-modifier-input 'power 'sampler1 sampler1)
  (cn::check-modifier-input 'power 'sampler2 sampler2)
  (lambda (&rest args)
    (let ((sample1 (cl:abs (cl:* (1+ (apply sampler1 args)) 0.5)))
          (sample2 (cl:abs (cl:* (1+ (apply sampler2 args)) 0.5))))
      (1- (cl:* (cl:expt sample1 sample2) 2)))))

(defun clamp (sampler &key (min -1f0) (max 1f0))
  "Clamp the output of `sampler` to be within the range denoted by `min` and `max`."
  (cn::check-modifier-input 'clamp 'sampler sampler)
  (lambda (&rest args)
    (u:clamp (apply sampler args) min max)))

(defun invert (sampler)
  "Invert the output of `sampler`."
  (cn::check-modifier-input 'invert 'sampler sampler)
  (lambda (&rest args)
    (cl:- (apply sampler args))))

(defun scale (sampler &rest scalars)
  "Scale the inputs of `sampler` by the given `scalars`. `scalars` is a list of floating point
values that must match the dimensionality of the sampler, and correspond to the amount of scaling to
apply to each of its axes."
  (cn::check-modifier-input 'scale 'sampler sampler)
  (lambda (&rest args)
    (loop :for arg :in args
          :for scalar :in scalars
          :collect (/ arg scalar) :into scale
          :finally (return (apply sampler scale)))))

(defun uniform-scale (sampler scalar)
  "Uniformly scale the inputs of `sampler` by `scalar`. NOTE: This is equivalent to `#'scale` called
with identical scalars for each dimension."
  (cn::check-modifier-input 'uniform-scale 'sampler sampler)
  (lambda (&rest args)
    (apply sampler (mapcar (lambda (x) (/ x scalar)) args))))

(defun rotate (sampler &key (x 0.0) (y 0.0) (z 0.0))
  "Rotate the inputs of `sampler` around the origin by the supplied angles in radians supplied for
the axes `x`, `y`, and `z`. This assumes a left-handed coordinate system."
  (cn::check-modifier-input 'rotate 'sampler sampler)
  (let* ((cx (cos x))
         (cy (cos y))
         (cz (cos z))
         (sx (sin x))
         (sy (sin y))
         (sz (sin z))
         (x1 (cl:+ (cl:* sy sx sz) (cl:* cy cz)))
         (y1 (cl:* cx sz))
         (z1 (cl:- (cl:* sy cz) (cl:* cy sy sz)))
         (x2 (cl:- (cl:* sy sx cz) (cl:* cy sz)))
         (y2 (cl:* cx cz))
         (z2 (cl:- (cl:* (cl:- cy) sx cz) (cl:* sy sz)))
         (x3 (cl:* (cl:- sy) cx))
         (y3 sx)
         (z3 (cl:* cy cx)))
    (lambda (x &optional (y 0d0) (z 0d0))
      (let ((x (cl:+ (cl:* x x1) (cl:* y y1) (cl:* z z1)))
            (y (cl:+ (cl:* x x2) (cl:* y y2) (cl:* z z2)))
            (z (cl:+ (cl:* x x3) (cl:* y y3) (cl:* z z3))))
        (funcall sampler x y z)))))

(defun translate (sampler &rest offsets)
  "Translate the inputs of `sampler` by the given `offsets`. `offsets` is a list of floating point
values that must match the dimensionality of the sampler, and correspond to the amount of
translation to apply to each of its axes."
  (cn::check-modifier-input 'translate 'sampler sampler)
  (lambda (&rest args)
    (loop :for arg :in args
          :for offset :in offsets
          :collect (cl:+ arg offset) :into translation
          :finally (return (apply sampler translation)))))

(defun strengthen (sampler strength &optional (bias 0.0))
  "Multiply the output of `sampler` by `strength`, then add `bias` to that."
  (cn::check-modifier-input 'strengthen 'sampler sampler)
  (lambda (&rest args)
    (cl:+ (cl:* (apply sampler args) strength) bias)))

(defun displace (sampler &rest displacement-samplers)
  "Displace the inputs of `sampler` according to the samplers given in `displacement-samplers`.
`displacement-samplers` is a list of samplers whose length matches the dimensionality of the source
sampler, `sampler`. For each displacement sampler, the corresponding coordinate axis of the input
coordinates of `sampler` is displaced by an amount equal to the result of sampling from the
displacement sampler."
  (cn::check-modifier-input 'displace 'sampler sampler)
  (lambda (&rest args)
    (loop :for arg :in args
          :for displace-sampler :in displacement-samplers
          :collect (cl:+ (apply displace-sampler args) arg) :into displacements
          :finally (return (apply sampler displacements)))))

(defun fractal (sampler &key (octaves 4) (frequency 1.0) (gain 0.5) (lacunarity 2.0))
  "Sum the output of `sampler` `octaves` times, with each succesive octave decreasing in amplitude
according to `gain`, and increasing in frequency according to `lacunarity`.

`octaves`: The number of successive noises to apply. Increasing this increases the feature detail of
the resulting noise, at the expense of more calculation time.

`frequency`: The frequency of the first octave of noise. Each successive octave's frequency is
increased by multiplying it by `lacunarity`.

`gain`: Controls the roughness of the generated noise. This should be a value between 0.0 and 1.0.

`lacunarity`: A multiplier for the frequency of each successive octave. This should be a value of
1.0 or larger."
  (cn::check-modifier-input 'fractal 'sampler sampler)
  (lambda (&rest args)
    (loop :with amplitude = 1.0
          :with args = (mapcar (lambda (x) (cl:* x frequency)) args)
          :repeat octaves
          :for sample = (apply sampler args)
          :sum (cl:* sample amplitude) :into value
          :do (setf args (mapcar (lambda (x) (cl:* x lacunarity)) args)
                    amplitude (cl:* amplitude gain))
          :finally (return value))))

(defun ridges (sampler &key (octaves 4) (frequency 1.0) (gain 2.0) (lacunarity 2.0) (exponent 1.0)
                         (offset 1.0))
  "A fractal-based sampler like `#'fractal`, except uses the absolute value for successive octaves
to give a ridged look."
  (cn::check-modifier-input 'ridges 'sampler sampler)
  (let ((weights (make-array octaves :element-type 'u:f32)))
    (loop :for i :below octaves
          :for frequency = 1.0 :then (cl:* frequency lacunarity)
          :do (setf (aref weights i) (cl:expt frequency (cl:- exponent))))
    (lambda (&rest args)
      (let ((args (mapcar (lambda (x) (cl:* x frequency)) args))
            (weight 1.0)
            (value 0.0))
        (dotimes (i octaves)
          (let ((sample (cl:* (cl:expt (cl:- offset (cl:abs (apply sampler args))) 2) weight)))
            (incf value (cl:* sample (aref weights i)))
            (setf weight (u:clamp (cl:* sample gain) 0.0 1.0)
                  args (mapcar (lambda (x) (cl:* x lacunarity)) args))))
        (1- (cl:* value 1.25))))))

(defun ridges2 (sampler &key (octaves 4) (frequency 1.0) (gain 0.5) (lacunarity 2.0))
  "A alternate version of the `#'ridges` fractal sampler, giving a different effect."
  (cn::check-modifier-input 'ridges2 'sampler sampler)
  (lambda (&rest args)
    (let ((args (mapcar (lambda (x) (cl:* x frequency)) args))
          (amplitude 1.0)
          (value 0.0))
      (dotimes (i octaves)
        (let ((sample (cl:abs (apply sampler args))))
          (incf value (cl:* (1+ (cl:* sample -2)) amplitude))
          (setf amplitude (cl:* amplitude gain )
                args (mapcar (lambda (x) (cl:* x lacunarity)) args))))
      (1- (cl:* value 1.25)))))

(defun billow (sampler &key (octaves 4) (frequency 1.0) (gain 0.5) (lacunarity 2.0))
  "A fractal-based sampler like `#'fractal`, except generates a 'billowy' effect suitable for
simulating clouds and rocks."
  (cn::check-modifier-input 'billow 'sampler sampler)
  (lambda (&rest args)
    (loop :with amplitude = 1.0
          :with args = (mapcar (lambda (x) (cl:* x frequency)) args)
          :repeat octaves
          :for sample = (1- (cl:* (cl:abs (apply sampler args)) 2))
          :sum (cl:* sample amplitude) :into value
          :do (setf args (mapcar (lambda (x) (cl:* x lacunarity)) args)
                    amplitude (cl:* amplitude gain))
          :finally (return (cl:+ value 0.5)))))

(defun turbulence (sampler &key (frequency 1.0) (power 1.0) (roughness 3))
  "A sampler that randomly displaces the inputs of `sampler`."
  (cn::check-modifier-input 'turbulence 'sampler sampler)
  (let ((d (fractal (cn:perlin-3d) :octaves roughness :frequency frequency)))
    (lambda (&rest args)
      (destructuring-bind (x &optional (y 0d0) (z 0d0) (w 0d0)) args
        (let* ((x0 (cl:+ x 0.1894226))
               (y0 (cl:+ y 0.9937134))
               (z0 (cl:+ z 0.47816467))
               (w0 (cl:+ w 0.78841865))
               (x1 (cl:+ x 0.40464783))
               (y1 (cl:+ y 0.27661133))
               (z1 (cl:+ z 0.9230499))
               (w1 (cl:+ w 0.5960642))
               (x2 (cl:+ x 0.821228))
               (y2 (cl:+ y 0.1710968))
               (z2 (cl:+ z 0.6842804))
               (w2 (cl:+ w 0.04850936))
               (x3 (cl:+ x 0.80564606))
               (y3 (cl:+ y 0.7283617))
               (z3 (cl:+ z 0.69029343))
               (w3 (cl:+ w 0.17203021)))
          (funcall sampler
                   (cl:+ x (cl:* (cn:sample d x0 y0 z0 w0) power))
                   (cl:+ y (cl:* (cn:sample d x1 y1 z1 w1) power))
                   (cl:+ z (cl:* (cn:sample d x2 y2 z2 w2) power))
                   (cl:+ w (cl:* (cn:sample d x3 y3 z3 w3) power))))))))
