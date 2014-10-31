(in-package :defpackage-plus-1)

(defvar *from-package* nil)

(defun map-symbol-names (function symbol-name-list from-package)
  (let ((*from-package* (find-package from-package)))
    (mapcar (lambda (x)
              (multiple-value-bind (symbol status)
                  (find-symbol (string x) *from-package*)
                (funcall function (or symbol x) status)))
            symbol-name-list)))

(defun assert-symbol-p (symbol status function-name)
  (if status
      symbol
      (error "Symbol ~S not accessible in ~S in ~A."
             symbol *from-package* function-name)))

(defun external-symbol-p (symbol status function-name)
  (if (eq status :external)
      symbol
      (error "Symbol ~S not external to ~S in ~A."
             symbol *from-package* function-name)))

(defun symbol-check (check-function function-name)
  (lambda (symbol status)
    (funcall check-function symbol status function-name)))

(defun import-from (from-package symbol-name-list &optional (package *package*))
  "Import symbols in `SYMBOL-NAME-LIST` from `FROM-PACKAGE` into `PACKAGE`.
It is an error if any symbol named in `SYMBOL-NAME-LIST` is not accessible in
`FROM-PACKAGE`."
  (let ((symbol-list
          (map-symbol-names (symbol-check #'assert-symbol-p "IMPORT-FROM")
                            symbol-name-list from-package)))
    (import symbol-list package)))

(defun import-external-from (from-package symbol-name-list &optional (package *package*))
  "Import symbols in `SYMBOL-NAME-LIST` from `FROM-PACKAGE` into
`PACKAGE`, but only if they are external to `FROM-PACKAGE`.  It is an
error if any symbol named in `SYMBOL-NAME-LIST` is not accessible in
`FROM-PACKAGE`, or if the symbol is not external to `FROM-PACKAGE`."
  (let ((symbol-list
          (map-symbol-names (symbol-check #'external-symbol-p "IMPORT-EXTERNAL-FROM")
                            symbol-name-list from-package)))
    (import symbol-list package)))

(defun shadowing-import-from (from-package symbol-name-list &optional (package *package*))
  "Shadowing-import symbols in `SYMBOL-NAME-LIST` from `FROM-PACKAGE` into `PACKAGE`.
It is an error if any symbol named in `SYMBOL-NAME-LIST` is not accessible in
`FROM-PACKAGE`."
  (let ((symbol-list
          (map-symbol-names (symbol-check #'assert-symbol-p "SHADOWING-IMPORT-FROM")
                            symbol-name-list from-package)))
    (shadowing-import symbol-list package)))

(defun import-package-except (from-package except-symbols
                              &optional (package *package*))
  "Import all symbols from `FROM-PACKAGE` *except* those specified by
`EXCEPT-SYMBOLS`."
  (let ((symbol-list (set-difference (package-external-symbols from-package)
                                     except-symbols
                                     :key #'symbol-name
                                     :test #'equal)))
    (import symbol-list package)))

(defun inherit-from (from-package symbol-list &optional (package *package*))
  "Import/export some external symbols from `FROM-PACKAGE`.  This is like
`IMPORT-FROM`, except symbols in `SYMBOL-LIST` are *also exported*
from `PACKAGE`.  It is an error if any symbols in `SYMBOL-LIST` are
not external to `FROM-PACKAGE`."
  (let ((symbol-list
          (map-symbol-names (symbol-check #'external-symbol-p "INHERIT-FROM")
                            symbol-list from-package)))
    (import symbol-list package)
    (export symbol-list package)))

(defun inherit-package (from-package &optional (package *package*))
  "Import/export *all* external symbols from `FROM-PACKAGE`.  This is
like `USE-PACKAGE` and `INHERIT-FROM` with all symbols external to
`FROM-PACKAGE` specified.

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
                                     except-symbols
                                     :key #'symbol-name
                                     :test #'equal)))
    (import symbol-list package)
    (export symbol-list package)))
