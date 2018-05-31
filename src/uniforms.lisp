(in-package :shadow)

(defun store-uniforms (program uniforms)
  (loop :for (symbol type) :in uniforms
        :for name = (varjo.internals:safe-glsl-name-string symbol)
        :do (setf (au:href (uniforms program) (au:make-keyword symbol))
                  (au:dict #'eq :name name :type type))))

(defun store-uniform-locations (program)
  (let ((id (id program)))
    (gl:use-program id)
    (au:do-hash-values (v (uniforms program))
      (setf (au:href v :location) (gl:get-uniform-location id (au:href v :name))))
    (gl:use-program 0)))

(defun get-uniform-location (uniform)
  (au:href (uniforms *active-shader-program*) uniform :location))

(defmacro %uniform-array (location func component-count element-type sequence)
  (au:with-unique-names (count sv)
    `(let ((,count (length ,sequence)))
       (static-vectors:with-static-vector
           (,sv (* ,count ,component-count)
                :element-type ',element-type)
         ,(if (= component-count 1)
              `(replace ,sv ,sequence)
              `(let ((i 0))
                 (map nil
                      (lambda (x)
                        (replace ,sv x :start1 i)
                        (incf i ,component-count))
                      ,sequence)))
         (,func ,location ,count (static-vectors:static-vector-pointer ,sv))))))

(defun uniform-int (uniform value)
  "Specify an integer as the VALUE for the uniform variable, UNIFORM."
  (let ((location (get-uniform-location uniform)))
    (%gl:uniform-1i location value)))

(defun uniform-int-array (uniform value)
  "Specify an array of integers as the VALUE for the uniform variable, UNIFORM."
  (let ((location (get-uniform-location uniform)))
    (%uniform-array location %gl:uniform-1iv 1 (unsigned-byte 32) value)))

(defun uniform-float (uniform value)
  "Specify a float as the VALUE for the uniform variable, UNIFORM."
  (let ((location (get-uniform-location uniform)))
    (%gl:uniform-1f location value)))

(defun uniform-float-array (uniform value)
  "Specify an array of floats as the VALUE for the uniform variable, UNIFORM."
  (let ((location (get-uniform-location uniform)))
    (%uniform-array location %gl:uniform-1fv 1 single-float value)))

(defun uniform-vec2 (uniform value)
  "Specify a vec2 as the VALUE for the uniform variable, UNIFORM."
  (let ((location (get-uniform-location uniform)))
    (%gl:uniform-2f location (aref value 0) (aref value 1))))

(defun uniform-vec2-array (uniform value)
  "Specify an array of vec2's as the VALUE for the uniform variable, UNIFORM."
  (let ((location (get-uniform-location uniform)))
    (%uniform-array location %gl:uniform-2fv 2 single-float value)))

(defun uniform-vec3 (uniform value)
  "Specify a vec3 as the VALUE for the uniform variable, UNIFORM."
  (let ((location (get-uniform-location uniform)))
    (%gl:uniform-3f location (aref value 0) (aref value 1) (aref value 2))))

(defun uniform-vec3-array (uniform value)
  "Specify an array of vec3's as the VALUE for the uniform variable, UNIFORM."
  (let ((location (get-uniform-location uniform)))
    (%uniform-array location %gl:uniform-3fv 3 single-float value)))

(defun uniform-vec4 (uniform value)
  "Specify a vec4 as the VALUE for the uniform variable, UNIFORM."
  (let ((location (get-uniform-location uniform)))
    (%gl:uniform-4f location (aref value 0) (aref value 1) (aref value 2) (aref value 3))))

(defun uniform-vec4-array (uniform value)
  "Specify an array of vec4's as the VALUE for the uniform variable, UNIFORM."
  (let ((location (get-uniform-location uniform)))
    (%uniform-array location %gl:uniform-4fv 4 single-float value)))

(defun uniform-mat2 (uniform value)
  "Specify a mat2 as the VALUE for the uniform variable, UNIFORM."
  (let ((location (get-uniform-location uniform)))
    (gl:uniform-matrix-2fv location value nil)))

(defun uniform-mat2-array (uniform value)
  "Specify an array of mat2's as the VALUE for the uniform variable, UNIFORM."
  (let ((location (get-uniform-location uniform)))
    (gl:uniform-matrix location 2 value nil)))

(defun uniform-mat3 (uniform value)
  "Specify a mat3 as the VALUE for the uniform variable, UNIFORM."
  (let ((location (get-uniform-location uniform)))
    (gl:uniform-matrix-3fv location value nil)))

(defun uniform-mat3-array (uniform value)
  "Specify an array of mat3's as the VALUE for the uniform variable, UNIFORM."
  (let ((location (get-uniform-location uniform)))
    (gl:uniform-matrix location 3 value nil)))

(defun uniform-mat4 (uniform value)
  "Specify a mat4 as the VALUE for the uniform variable, UNIFORM."
  (let ((location (get-uniform-location uniform)))
    (gl:uniform-matrix-4fv location value nil)))

(defun uniform-mat4-array (uniform value)
  "Specify an array of mat4's as the VALUE for the uniform variable, UNIFORM."
  (let ((location (get-uniform-location uniform)))
    (gl:uniform-matrix location 4 value nil)))
