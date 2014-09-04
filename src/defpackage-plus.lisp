(in-package :defpackage-plus-1)

(defgeneric defpackage+-dispatch (option parameters package-name)
  (:documentation "This function is called for every form in the body
of `DEFPACKAGE+`.  The `CAR` of each form is the `OPTION` parameter,
which methods should specialize on.  The `CDR` of the list is
`PARAMETERS`.

All keywords and symbols from `COMMON-LISP` are reserved for use by
defpackage-plus; user methods should **not** specialize on these."))

(defmacro defpackage+ (package-name &body options)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (ensure-package ',package-name)
     ,@(loop for option in options
             collect `(defpackage+-dispatch ',(car option) ',(cdr option) ',package-name))))

(defmethod defpackage+-dispatch ((option (eql :use)) params package)
  (use-package params package))

(defmethod defpackage+-dispatch ((option (eql :use-only)) params package)
  (ensure-use-only package params))

(defmethod defpackage+-dispatch ((option (eql :export)) params package)
  (ensure-export params package))

(defmethod defpackage+-dispatch ((option (eql :export-only)) params package)
  (ensure-export-only params package))

(defmethod defpackage+-dispatch ((option (eql :export-warning)) params package)
  (ensure-export-warning params package))

(defmethod defpackage+-dispatch ((option (eql :documentation)) params package)
  (setf (documentation (find-package package) t)
        (car params)))

(defmethod defpackage+-dispatch ((option (eql :inherit-from)) params package)
  (let ((from-package (car params))
        (symbol-list (cdr params)))
    (inherit-from-package from-package symbol-list package)))

(defmethod defpackage+-dispatch ((option (eql :inherit)) params package)
  (loop for i in params
        do (inherit-package i package)))

(defmethod defpackage+-dispatch ((option (eql :inherit-except)) params package)
  (let ((from-package (car params))
        (symbol-list (cdr params)))
    (inherit-package-except from-package symbol-list package)))

(defmethod defpackage+-dispatch ((option (eql :import-from)) params package)
  (let ((from-package (car params))
        (symbol-list (cdr params)))
    (import-from symbol-list from-package package)))

(defmethod defpackage+-dispatch ((option (eql :shadow)) params package)
  (shadow params package))

(defmethod defpackage+-dispatch ((option (eql :shadowing-import-from)) params package)
  (let ((from-package (car params))
        (symbol-list (cdr params)))
    (shadowing-import-from from-package symbol-list package)))

(defmethod defpackage+-dispatch ((option (eql :nicknames)) params package)
  (ensure-nicknames params package))

(defmethod defpackage+-dispatch ((option (eql :intern)) params package)
  (loop for i in params
        do (ensure-symbol i package)))
