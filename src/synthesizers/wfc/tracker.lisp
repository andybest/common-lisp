(in-package #:%syntex.synthesizers.wfc.tracker)

(defclass tracker () ())

(defgeneric reset (tracker))

(defgeneric ban (tracker index pattern))

(defgeneric unban (tracker index pattern))
