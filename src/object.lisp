(in-package #:mfiano.webapi.stripe)

(defmacro define-object (name super-classes &body fields)
  (u:with-gensyms (stream)
    (let ((slots (mapcar
                  (lambda (x)
                    (let ((name (u:ensure-list x)))
                      (destructuring-bind (name &key (reader name) extra-initargs) name
                        `(,(u:symbolicate '#:% name)
                          :reader ,reader
                          :initarg ,(u:make-keyword name)
                          ,@(when extra-initargs
                              `(,@(mapcan
                                   (lambda (x)
                                     `(:initarg ,x))
                                   extra-initargs)))))))
                  fields)))
      `(progn
         (defclass ,name
             ,@(if super-classes
                   `(,super-classes)
                   `((stripe-object)))
           ,slots)
         (u:define-printer (,name ,stream :type nil)
           (if (and (slot-exists-p ,name '%id)
                    (slot-boundp ,name '%id))
               (format ,stream "~a ~a" ',name (id ,name))
               (format ,stream "~a" ',name)))))))

(defclass stripe-object () ())

(defmethod initialize-instance :after ((instance stripe-object) &key data &allow-other-keys)
  (apply #'reinitialize-instance instance :allow-other-keys t data))

(define-object address ()
  (line1 :extra-initargs (:address-line1))
  (line2 :extra-initargs (:address-line2))
  (city :extra-initargs (:address-city))
  (state :extra-initargs (:address-state))
  (postal-code :extra-initargs (:address-zip))
  (country :extra-initargs (:address-country)))

(define-object shipping ()
  address
  name
  phone)

(defmethod initialize-instance :after ((instance shipping) &key data &allow-other-keys)
  (destructuring-bind (&key address &allow-other-keys) data
    (reinitialize-instance instance :address (make-instance 'address :data address))))

(define-object billing-details (shipping)
  email)
