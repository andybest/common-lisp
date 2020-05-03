(in-package #:umbra.sprite)

(defstruct sprite-data
  (sampler :sampler-2d :accessor sampler)
  (index :int :accessor index))

(defstruct spritesheet-data
  (pos (:vec2 2048) :accessor pos)
  (size (:vec2 2048) :accessor size))

(defun make-vertex-data ((sprite sprite-data)
                         (spritesheet spritesheet-data))
  (let* ((size (.xyxy (texture-size (sampler sprite) 0)))
         (extents (vec4 (aref (pos spritesheet) (index sprite))
                        (aref (size spritesheet) (index sprite))))
         (offsets (* size (vec4 (* (.zw extents) 0.5)
                                (* (.zw extents) -0.5)))))
    (incf (.zw extents) (.xy extents))
    (case gl-vertex-id
      (0 (values (.xy offsets) (.zw extents)))
      (1 (values (.zy offsets) (.xw extents)))
      (2 (values (.xw offsets) (.zy extents)))
      (otherwise (values (.zw offsets) (.xy extents))))))

(defun sprite/v (&uniforms
                 (model :mat4)
                 (view :mat4)
                 (proj :mat4)
                 (sprite sprite-data)
                 (spritesheet spritesheet-data :ssbo :std-430))
  (mvlet* ((pos uv (make-vertex-data sprite spritesheet)))
    (values (* proj view model (vec4 pos 0 1))
            uv)))

(defun sprite/f ((uv :vec2)
                 &uniforms
                 (sprite sprite-data)
                 (opacity :float))
  (let ((color (texture (sampler sprite) uv)))
    (vec4 (.rgb color) (* (.a color) opacity))))

(define-shader sprite (:primitive :triangle-strip)
  (:vertex (sprite/v))
  (:fragment (sprite/f :vec2)))
