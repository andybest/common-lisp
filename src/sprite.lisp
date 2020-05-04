(in-package #:umbra.sprite)

(defstruct sprite-data
  (sampler :sampler-2d :accessor sampler)
  (index :int :accessor index))

(defstruct spritesheet-data
  (pos (:ivec2 2048) :accessor pos)
  (size (:ivec2 2048) :accessor size))

(defun make-vertex-data ((sprite sprite-data)
                         (spritesheet spritesheet-data))
  (let* ((tsize (vec4 (.xyxy (texture-size (sampler sprite) 0))))
         (spos (aref (pos spritesheet) (index sprite)))
         (ssize (+ (vec2 1 1) (aref (size spritesheet) (index sprite))))
         (zpos (+ spos ssize))
         (vertpos (vec4 (* ssize 0.5) (* ssize -0.5)))
         (uv (/ (+ (vec4 -0.5 -0.5 0.5 0.5) (vec4 spos zpos)) tsize)))
    (case gl-vertex-id
      (0 (values (.xy vertpos) (.zw uv)))
      (1 (values (.zy vertpos) (.xw uv)))
      (2 (values (.xw vertpos) (.zy uv)))
      (otherwise (values (.zw vertpos) (.xy uv))))))

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
