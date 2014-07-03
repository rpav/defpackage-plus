(in-package :defpackage-plus-1.0)

(defun package-external-symbols (package)
  "=> LIST-OF-SYMBOLS

Return a new list of symbols external to `PACKAGE`"
  (let (list)
    (do-external-symbols (s package list)
      (push s list))))

(defun package-symbols (package)
  "=> LIST-OF-SYMBOLS

Return a new list of symbols accessible from `PACKAGE`.  Like
`DO-SYMBOLS`, this may include the same symbol more than once."
  (let (list)
    (do-symbols (s package list)
      (push s list))))
