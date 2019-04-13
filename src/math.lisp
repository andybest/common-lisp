(in-package :au)

(defun-inline degrees->radians (degrees)
  "Convert `DEGREES` to radians."
  (* degrees (/ pi 180)))

(defun-inline radians->degrees (radians)
  "Convert `RADIANS` to degrees."
  (* radians (/ 180 pi)))

(defun-inline map-domain (source-min source-max dest-min dest-max value)
  "Map `VALUE` from the domain denoted by `SOURCE-MIN` and `SOURCE-MAX` to the domain denoted by
`DEST-MIN` and `DEST-MAX`."
  (lerp (/ (- value source-min)
           (- source-max source-min))
        dest-min
        dest-max))

(defun-inline average (numbers)
  "Calculate the mean average of `NUMBERS`, a list of numbers."
  (/ (apply #'+ numbers) (length numbers)))
