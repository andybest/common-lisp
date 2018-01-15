(in-package :shadow)

(defclass uniform-buffer ()
  ((%id :reader id)
   (%name :reader name)
   (%accessors :reader accessors)))

(defun collect-blocks (stages block-type)
  (mapcan
   (lambda (x)
     (loop :for uniform :in (varjo:uniform-variables x)
           :when (member block-type (varjo:qualifiers uniform))
             :collect uniform))
   stages))

(defun collect-block-structs (blocks)
  (let ((structs))
    (labels ((process (type)
               (typecase type
                 (varjo:v-array (process-array type))
                 (varjo:v-user-struct (process-struct type))))
             (process-array (type)
               (process (varjo:v-element-type type)))
             (process-struct (type)
               (unless (find type structs :test #'varjo:v-type-eq)
                 (map nil (lambda (x) (process (second x))) (varjo.internals:v-slots type))
                 (push type structs)))
             (find-structs (types)
               (map nil #'process types)
               (reverse structs)))
      (find-structs
       (mapcan
        (lambda (x)
          (loop :with struct = (varjo:v-type-of x)
                :for (nil type) :in (varjo.internals:v-slots struct)
                :when (typep type 'varjo:v-user-struct)
                  :collect type))
        blocks)))))

(defun cache-accessors (program packing)
  (loop :for ((object-name) . (data)) :in packing
        :do (destructuring-bind (&key members &allow-other-keys) data
              (dolist (part members)
                (destructuring-bind (&key slot-name offset size &allow-other-keys) part
                  (let* ((part-names (alexandria:ensure-list slot-name))
                         (key (ensure-keyword (format nil "~a~{.~a~}" object-name part-names))))
                    (setf (gethash key (buffers program))
                          (lambda (x)
                            offset size))))))))

(defun store-buffer-data (program stages block-type)
  (let* ((blocks (collect-blocks stages block-type))
         (structs (collect-block-structs blocks))
         (packing (pack-all structs blocks)))
    (format t "structs:~%~S~%~%" structs)
    (format t "blocks:~%~S~%~%" blocks)
    (format t "packing:~%~S~%~%" packing)
    (cache-accessors program packing)))
