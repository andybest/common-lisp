(in-package #:mfiano.gamedev.umbra.noise)

;;;; Noise utility functions

;;; fBm (Fractional Brownian motion)
;;; We have separate Perlin and Simplex variants, because varjo sometimes generates wrong code with
;;; higher order functions.

(defun fbm/perlin ((point :vec2)
                   (octaves :uint)
                   (frequency :float)
                   (gain :float)
                   (lacunarity :float))
  (let ((value 0.0)
        (amplitude 0.5))
    (dotimes (i octaves)
      (incf value (* amplitude (perlin (* point frequency))))
      (setf point (* point lacunarity)
            amplitude (* amplitude gain)))
    value))

(defun fbm/perlin ((point :vec3)
                   (octaves :uint)
                   (frequency :float)
                   (gain :float)
                   (lacunarity :float))
  (let ((value 0.0)
        (amplitude 0.5))
    (dotimes (i octaves)
      (incf value (* amplitude (perlin (* point frequency))))
      (setf point (* point lacunarity)
            amplitude (* amplitude gain)))
    value))

(defun fbm/simplex ((point :vec2)
                    (octaves :uint)
                    (frequency :float)
                    (gain :float)
                    (lacunarity :float))
  (let ((value 0.0)
        (amplitude 0.5))
    (dotimes (i octaves)
      (incf value (* amplitude (simplex-perlin (* point frequency))))
      (setf point (* point lacunarity)
            amplitude (* amplitude gain)))
    value))

(defun fbm/simplex ((point :vec3)
                    (octaves :uint)
                    (frequency :float)
                    (gain :float)
                    (lacunarity :float))
  (let ((value 0.0)
        (amplitude 0.5))
    (dotimes (i octaves)
      (incf value (* amplitude (simplex-perlin (* point frequency))))
      (setf point (* point lacunarity)
            amplitude (* amplitude gain)))
    value))

(defun fbm/cellular ((point :vec2)
                     (octaves :uint)
                     (frequency :float)
                     (gain :float)
                     (lacunarity :float))
  (let ((value 0.0)
        (amplitude 0.5))
    (dotimes (i octaves)
      (incf value (* amplitude (cellular-fast (* point frequency))))
      (setf point (* point lacunarity)
            amplitude (* amplitude gain)))
    value))

(defun fbm/cellular ((point :vec3)
                     (octaves :uint)
                     (frequency :float)
                     (gain :float)
                     (lacunarity :float))
  (let ((value 0.0)
        (amplitude 0.5))
    (dotimes (i octaves)
      (incf value (* amplitude (cellular-fast (* point frequency))))
      (setf point (* point lacunarity)
            amplitude (* amplitude gain)))
    value))

;;; Ridges
;;; We have separate Perlin and Simplex variants, because varjo sometimes generates wrong code with
;;; higher order functions.

(defun ridges/perlin ((point :vec2)
                      (octaves :uint)
                      (frequency :float)
                      (gain :float)
                      (lacunarity :float)
                      (exponent :float)
                      (offset :float))
  (let ((value 0.0)
        (amplitude 0.5)
        (previous 1.0))
    (dotimes (i octaves)
      (let* ((n (perlin (* point frequency)))
             (n (- offset (abs n)))
             (n (expt n exponent)))
        (incf value (* n amplitude previous))
        (setf previous n
              frequency (* frequency lacunarity)
              amplitude (* amplitude gain))))
    value))

(defun ridges/perlin ((point :vec3)
                      (octaves :uint)
                      (frequency :float)
                      (gain :float)
                      (lacunarity :float)
                      (exponent :float)
                      (offset :float))
  (let ((value 0.0)
        (amplitude 0.5)
        (previous 1.0))
    (dotimes (i octaves)
      (let* ((n (perlin (* point frequency)))
             (n (- offset (abs n)))
             (n (expt n exponent)))
        (incf value (* n amplitude previous))
        (setf previous n
              frequency (* frequency lacunarity)
              amplitude (* amplitude gain))))
    value))

(defun ridges/simplex ((point :vec2)
                       (octaves :uint)
                       (frequency :float)
                       (gain :float)
                       (lacunarity :float)
                       (exponent :float)
                       (offset :float))
  (let ((value 0.0)
        (amplitude 0.5)
        (previous 1.0))
    (dotimes (i octaves)
      (let* ((n (simplex-perlin (* point frequency)))
             (n (- offset (abs n)))
             (n (expt n exponent)))
        (incf value (* n amplitude previous))
        (setf previous n
              frequency (* frequency lacunarity)
              amplitude (* amplitude gain))))
    value))

(defun ridges/simplex ((point :vec3)
                       (octaves :uint)
                       (frequency :float)
                       (gain :float)
                       (lacunarity :float)
                       (exponent :float)
                       (offset :float))
  (let ((value 0.0)
        (amplitude 0.5)
        (previous 1.0))
    (dotimes (i octaves)
      (let* ((n (simplex-perlin (* point frequency)))
             (n (- offset (abs n)))
             (n (expt n exponent)))
        (incf value (* n amplitude previous))
        (setf previous n
              frequency (* frequency lacunarity)
              amplitude (* amplitude gain))))
    value))

(defun ridges/cellular ((point :vec2)
                        (octaves :uint)
                        (frequency :float)
                        (gain :float)
                        (lacunarity :float)
                        (exponent :float)
                        (offset :float))
  (let ((value 0.0)
        (amplitude 0.5)
        (previous 1.0))
    (dotimes (i octaves)
      (let* ((n (cellular-fast (* point frequency)))
             (n (- offset (abs n)))
             (n (expt n exponent)))
        (incf value (* n amplitude previous))
        (setf previous n
              frequency (* frequency lacunarity)
              amplitude (* amplitude gain))))
    value))

(defun ridges/cellular ((point :vec3)
                        (octaves :uint)
                        (frequency :float)
                        (gain :float)
                        (lacunarity :float)
                        (exponent :float)
                        (offset :float))
  (let ((value 0.0)
        (amplitude 0.5)
        (previous 1.0))
    (dotimes (i octaves)
      (let* ((n (cellular-fast (* point frequency)))
             (n (- offset (abs n)))
             (n (expt n exponent)))
        (incf value (* n amplitude previous))
        (setf previous n
              frequency (* frequency lacunarity)
              amplitude (* amplitude gain))))
    value))

;;; Billow
;;; We have separate Perlin and Simplex variants, because varjo sometimes generates wrong code with
;;; higher order functions.

(defun billow/perlin ((point :vec2)
                      (octaves :uint)
                      (frequency :float)
                      (gain :float)
                      (lacunarity :float))
  (let ((value 0.0)
        (amplitude 0.5))
    (dotimes (i octaves)
      (incf value (* (1- (* (abs (perlin (* point frequency))) 2))
                     amplitude))
      (setf point (* point lacunarity)
            amplitude (* amplitude gain)))
    value))

(defun billow/perlin ((point :vec3)
                      (octaves :uint)
                      (frequency :float)
                      (gain :float)
                      (lacunarity :float))
  (let ((value 0.0)
        (amplitude 0.5))
    (dotimes (i octaves)
      (incf value (* (1- (* (abs (perlin (* point frequency))) 2))
                     amplitude))
      (setf point (* point lacunarity)
            amplitude (* amplitude gain)))
    value))

(defun billow/simplex ((point :vec2)
                       (octaves :uint)
                       (frequency :float)
                       (gain :float)
                       (lacunarity :float))
  (let ((value 0.0)
        (amplitude 0.5))
    (dotimes (i octaves)
      (incf value (* (1- (* (abs (simplex-perlin (* point frequency))) 2))
                     amplitude))
      (setf point (* point lacunarity)
            amplitude (* amplitude gain)))
    value))

(defun billow/simplex ((point :vec3)
                       (octaves :uint)
                       (frequency :float)
                       (gain :float)
                       (lacunarity :float))
  (let ((value 0.0)
        (amplitude 0.5))
    (dotimes (i octaves)
      (incf value (* (1- (* (abs (simplex-perlin (* point frequency))) 2))
                     amplitude))
      (setf point (* point lacunarity)
            amplitude (* amplitude gain)))
    value))

(defun billow/cellular ((point :vec2)
                        (octaves :uint)
                        (frequency :float)
                        (gain :float)
                        (lacunarity :float))
  (let ((value 0.0)
        (amplitude 0.5))
    (dotimes (i octaves)
      (incf value (* (1- (* (abs (cellular (* point frequency))) 2))
                     amplitude))
      (setf point (* point lacunarity)
            amplitude (* amplitude gain)))
    value))

(defun billow/cellular ((point :vec3)
                        (octaves :uint)
                        (frequency :float)
                        (gain :float)
                        (lacunarity :float))
  (let ((value 0.0)
        (amplitude 0.5))
    (dotimes (i octaves)
      (incf value (* (1- (* (abs (cellular (* point frequency))) 2))
                     amplitude))
      (setf point (* point lacunarity)
            amplitude (* amplitude gain)))
    value))
