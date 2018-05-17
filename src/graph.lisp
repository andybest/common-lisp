(in-package :umbra.graph)

;;;; Graphing
;;;; Credit: Chris Bagley

(defun-gpu axis ((uv :vec2)
                 (xy-range :vec4)
                 (style :vec4))
  (let* ((color (v4:make (.xyz style) 1))
         (diff (/ (.xz xy-range) (- (.yw xy-range) (.xz xy-range))))
         (uv (+ uv diff)))
    (+ (* color (smoothstep (.w style) 0 (abs (.x uv))))
       (* color (smoothstep (.w style) 0 (abs (.y uv)))))))

(defun-gpu graph ((func (function (:float) :float))
                  (uv :vec2)
                  (xy-range :vec4)
                  (line-style :vec4)
                  (axis-style :vec4)
                  (samples :int))
  (let* ((axis (axis uv xy-range axis-style))
         (diff (- (.yw xy-range) (.xz xy-range)))
         (uv (+ (* uv diff) (.xz xy-range)))
         (samples (float samples))
         (max-dist (* (.w line-style) diff))
         (step (/ max-dist samples))
         (count 0.0)
         (my-samples 0.0))
    (for (i 0.0) (< i samples) (++ i)
         (let ((fx (funcall func (+ (.x uv) (* i (.x step))))))
           (for (j 0.0) (< j samples) (++ j)
                (when (> (+ (* i i) (* j j)) (* samples samples))
                  (continue))
                (incf my-samples 1.0)
                (let ((diff (- fx (+ (.y uv) (* j (.y step))))))
                  (incf count (- (* (step 0.0 diff) 2.0) 1))))))
    (values (+ (* (if (/= (abs count) my-samples)
                      (- 1.0 (/ (abs (float count)) (float my-samples)))
                      0.0)
                  (v4:make (.xyz line-style) 1))
               axis)
            (funcall func (.x uv)))))

(defun-gpu graph ((func (function (:float) :float))
                  (uv :vec2))
  (graph func
         uv
         (v4:make 0 1 0 1)
         (v4:make 1 1 1 0.004)
         (v4:make 0.1 0.1 0.1 0.004)
         10))

(defun-gpu graph ((func (function (:float) :float))
                  (uv :vec2)
                  (xy-range :vec4))
  (graph func
         uv
         xy-range
         (v4:make 1 1 1 0.004)
         (v4:make 0.1 0.1 0.1 0.004)
         10))

(defun-gpu graph ((func (function (:float) :float))
                  (uv :vec2)
                  (xy-range :vec4)
                  (line-style :float))
  (graph func
         uv
         xy-range
         (v4:make 1 1 1 line-style)
         (v4:make 0.1 0.1 0.1 0.4)
         10))

(defun-gpu graph ((func (function (:float) :float))
                  (uv :vec2)
                  (xy-range :vec4)
                  (line-style :vec4))
  (graph func
         uv
         xy-range
         line-style
         (v4:make 0.1 0.1 0.1 0.4)
         10))

(defun-gpu graph ((func (function (:float) :float))
                  (uv :vec2)
                  (xy-range :vec4)
                  (line-style :float)
                  (axis-style :float))
  (graph func
         uv
         xy-range
         (v4:make 1 1 1 line-style)
         (v4:make 0.1 0.1 0.1 axis-style)
         10))

(defun-gpu graph ((func (function (:float) :float))
                  (uv :vec2)
                  (xy-range :vec4)
                  (line-style :vec4)
                  (axis-style :vec4))
  (graph func uv xy-range line-style axis-style 10))
