(in-package :defpackage-plus-1)

(defun ensure-use-only (use-list &optional (package *package*))
  (let ((use-list (mapcar #'find-package use-list)))
    (unuse-package (set-difference (package-use-list package) use-list)
                   package)
    (use-package use-list package)))

(defun ensure-package (package-name)
  (unless (find-package package-name)
    (make-package package-name :use nil)))

(defun ensure-nicknames (nickname-list &optional (package *package*))
  (rename-package package (package-name package) nickname-list))

(defun ensure-export-only (export-list &optional (package *package*))
  (let ((package (find-package package)))
    (unexport (set-difference (package-external-symbols package)
                              export-list)
              package)
    (ensure-export export-list package)))

(defun ensure-export-warning (export-list &optional (package *package*))
  (when-let (diff (set-difference (package-external-symbols package)
                                  export-list))
    (warn "The following symbols not specified to ENSURE-EXPORT-WARNING are
also exported from ~S:~%  ~S~%"
          package diff))
  (ensure-export export-list))

(defun ensure-export (export-list &optional (package *package*))
  (let ((package (find-package package)))
    (when export-list
      (export
       (mapcar (lambda (x) (ensure-symbol x package))
               export-list)
       package))))
