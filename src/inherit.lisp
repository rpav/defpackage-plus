(in-package :defpackage-plus-1.0)

(defun import-from (from-package symbol-name-list &optional (package *package*))
  "Import symbols in `SYMBOL-NAME-LIST` from `FROM-PACKAGE` into `PACKAGE`.
It is an error if any symbol named in `SYMBOL-NAME-LIST` is not accessible in
`FROM-PACKAGE`."
  (let* ((from-package (find-package from-package))
         (symbol-list (mapcar (lambda (x)
                                (multiple-value-bind (symbol status)
                                    (find-symbol (string x) from-package)
                                  (if status
                                      symbol
                                      (error "Symbol ~S not accessible in ~S in IMPORT-FROM."
                                             x from-package))))
                              symbol-name-list)))
    (import symbol-list package)))

(defun shadowing-import-from (from-package symbol-name-list &optional (package *package*))
  "Shadowing-import symbols in `SYMBOL-NAME-LIST` from `FROM-PACKAGE` into `PACKAGE`.
It is an error if any symbol named in `SYMBOL-NAME-LIST` is not accessible in
`FROM-PACKAGE`."
  (let* ((from-package (find-package from-package))
         (symbol-list (mapcar (lambda (x)
                                (multiple-value-bind (symbol status)
                                    (find-symbol (string x) from-package)
                                  (if status
                                      symbol
                                      (error "Symbol ~S not accessible in ~S in IMPORT-FROM."
                                             x from-package))))
                              symbol-name-list)))
    (shadowing-import symbol-list package)))

(defun inherit-from-package (from-package symbol-list &optional (package *package*))
  "Import/export some external symbols from `FROM-PACKAGE`.  This is like
`IMPORT-FROM`, except symbols in `SYMBOL-LIST` are *also exported*
from `PACKAGE`.  It is an error if any symbols in `SYMBOL-LIST` are
not external to `FROM-PACKAGE`."
  (let* ((from-package (find-package from-package))
         (symbol-list (mapcar (lambda (x)
                                (multiple-value-bind (symbol status)
                                    (find-symbol (string x) from-package)
                                  (if (eq status :external)
                                      symbol
                                      (error "Symbol ~S not external to ~S in INHERIT-PACKAGE."
                                             x from-package))))
                              symbol-list)))
    (import symbol-list package)
    (export symbol-list package)))

(defun inherit-package (from-package &optional (package *package*))
  "Import/export *all* external symbols from `FROM-PACKAGE`.  This is
like `USE-PACKAGE` and `INHERIT-FROM-PACKAGE` with all symbols
external to `FROM-PACKAGE` specified.

Note that this only applies to symbols exported *when called*.  Future
symbols exported from `FROM-PACKAGE` will not also be exported from
`PACKAGE` unless this function is called again."
  (let ((all-symbols (package-external-symbols from-package)))
    (import all-symbols package)
    (export all-symbols package)))

(defun inherit-package-except (from-package except-symbols
                               &optional (package *package*))
  "Import/export all symbols from `FROM-PACKAGE` *except* those specified by
`EXCEPT-SYMBOLS`."
  (let ((symbol-list (set-difference (package-external-symbols from-package)
                                     except-symbols)))
    (import symbol-list package)
    (export symbol-list package)))
