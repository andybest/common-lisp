(in-package #:umbra.sprite)

(defstruct sprite-data
  (sampler :sampler-2d :accessor sampler)
  (index :int :accessor index))

(defstruct spritesheet-data
  (pos (:vec2 2048) :accessor pos)
  (size (:vec2 2048) :accessor size))

(defstruct vertex-data
  (positions (:vec4 4) :accessor positions)
  (uvs (:vec2 4) :accessor uvs))

(defun sprite/v (&uniforms
                 (model :mat4)
                 (view :mat4)
                 (proj :mat4)
                 (vertices vertex-data)
                 (sprite sprite-data)
                 (spritesheet spritesheet-data :ssbo :std-430))
  (with-slots (positions uvs) vertices
    (let* ((size (.xyxy (texture-size (sampler sprite) 0)))
           (extents (vec4 (aref (pos spritesheet) (index sprite))
                          (aref (size spritesheet) (index sprite))))
           (offsets (* size
                       (vec4 (* 0.5 (.zw extents))
                             (* -0.5 (.zw extents))))))
      (setf (.zw extents) (+ (.xy extents) (.zw extents))
            (aref positions 0) (vec4 (.xy offsets) 0 1)
            (aref uvs 0) (.zw extents)
            (aref positions 1) (vec4 (.zy offsets) 0 1)
            (aref uvs 1) (.xw extents)
            (aref positions 2) (vec4 (.xw offsets) 0 1)
            (aref uvs 2) (.zy extents)
            (aref positions 3) (vec4 (.zw offsets) 0 1)
            (aref uvs 3) (.xy extents))
      (values (* proj view model (aref positions gl-vertex-id))
              (aref uvs gl-vertex-id)))))

(defun sprite/f ((uv :vec2)
                 &uniforms
                 (opacity :float)
                 (sprite sprite-data))
  (let ((color (texture (sampler sprite) uv)))
    (vec4 (.rgb color) (* (.a color) opacity))))

(define-shader sprite (:primitive :triangle-strip)
  (:vertex (sprite/v))
  (:fragment (sprite/f :vec2)))
