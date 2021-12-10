(in-package #:mfiano.scripts.base)

(defun generate-validity-error-message (body long)
  (let ((short-name (u:plist-get body :short))
        (long-name (format nil "-~a" (or (u:plist-get body :long) long)))
        (message (format nil (or (u:plist-get body :validity-error)
                                 "encountered an unknown error."))))
    (format nil "The argument supplied for ~{-~a~^/~} ~a"
            (remove nil (list short-name long-name))
            message)))

(defmacro define-option (name &body body)
  (let ((long (string-downcase (symbol-name name))))
    `(progn
       (defvar ,(u:symbolicate '#:*arg- name '#:*))
       (defparameter ,(u:symbolicate '#:*option- name '#:*)
         (ui:make-option
          ',name
          ,@(u:plist-remove body :validity-check :validity-error :help :manual)
          ,@(u:when-let ((help (u:plist-get body :help)))
              `(:help ,(format nil help)))
          ,@(u:when-let ((manual (u:plist-get body :manual)))
              `(:manual ,(format nil manual)))
          :long ,long
          :reduce (constantly t)))
       ,@(u:when-let ((check (u:plist-get body :validity-check)))
           `((defmethod validate-option ((key (eql ',name)) value)
               (unless (funcall ,check value)
                 (error 'user-error :message ,(generate-validity-error-message body long)))))))))

(defmacro define-boolean-options (name &body body)
  (let ((option-on (u:symbolicate '#:*option- name '#:*))
        (option-off (u:symbolicate '#:*option-no- name '#:*)))
    `(progn
       (defvar ,option-on)
       (defvar ,option-off)
       (defvar ,(u:symbolicate '#:*arg- name '#:*))
       (setf (values ,option-on ,option-off)
             (ui:make-boolean-options
              ',name
              ,@(u:plist-remove body :help :help-no :manual :manual-no)
              ,@(u:when-let ((help (u:plist-get body :help)))
                  `(:help ,(format nil help)))
              ,@(u:when-let ((help-no (u:plist-get body :help-no)))
                  `(:help-no ,(format nil help-no)))
              ,@(u:when-let ((manual (u:plist-get body :manual)))
                  `(:manual ,(format nil manual)))
              ,@(u:when-let ((manual-no (u:plist-get body :manual-no)))
                  `(:manual-no ,(format nil manual-no)))
              :long ,(string-downcase (symbol-name name)))))))

(define-option help
  :short #\h
  :help "Display help and exit.")

(define-option version
  :short #\V
  :help "Display version information and exit.")
