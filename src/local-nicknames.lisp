(in-package :defpackage-plus-1)

(defun ensure-global-nickname (package nickname)
  (let ((package (find-package package)))
    (rename-package package (package-name package)
                    (adjoin nickname (package-nicknames package)
                            :key #'string :test #'equal))))

(defun add-local-nickname (package nickname local-to)
  (declare (ignorable package nickname local-to))
  #+sbcl
  (sb-ext:add-package-local-nickname nickname package local-to)
  #+(or abcl ecl)
  (ext:add-package-local-nickname nickname package local-to))

(defmethod defpackage+-dispatch ((option (eql :local-nicknames)) params package)
  (loop :for (nick pack) :in params :do
    (progn
      (ensure-package pack)
      #+package-local-nicknames
      (add-local-nickname pack nick package)
      #-package-local-nicknames
      (ensure-global-nickname pack nick))))

