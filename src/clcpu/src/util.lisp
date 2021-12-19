(in-package #:freebsd-tools.clcpu)

(defun fg-color->ansi (color)
  (cond
    ((<= 0 color 7)
     (+ color 30))
    ((<= 8 color 15)
     (+ color 82))
    (t
     (lib:user-error "Invalid foreground color: ~d" color))))

(defun bg-color->ansi (color)
  (cond
    ((<= 0 color 7)
     (+ color 40))
    ((<= 8 color 15)
     (+ color 92))
    (t
     (lib:user-error "Invalid background color: ~d" color))))

(defun parse-color-pair (color)
  (destructuring-bind (fg &optional bg) (u:split-sequence #\, color :count 2)
    (list (if (u:emptyp fg)
              37
              (fg-color->ansi (lib:parse-integer fg)))
          (if (u:emptyp bg)
              40
              (bg-color->ansi (lib:parse-integer bg))))))
