(in-package #:freebsd-tools.lib)

(defvar *program-name*)

(defvar *version* "0.1.0")

(defvar *authors* nil)

(defvar *initial-year*
  (cl:parse-integer
   (lt:format-timestring nil (lt:now) :format '((:year 4)))))

(defvar *interactive* t)
