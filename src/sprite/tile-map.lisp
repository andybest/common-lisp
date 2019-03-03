(in-package :umbra.sprite)

(define-function tile-map/v ((pos :vec3)
                             (uv :vec2)
                             &uniform
                             (model :mat4)
                             (view :mat4)
                             (proj :mat4))
  (values (* proj view model (vec4 pos 1))
          uv))

(define-shader tile-map (:primitive :points)
  (:vertex (tile-map/v :vec3 :vec2))
  (:fragment (sprite/f :vec2)))
