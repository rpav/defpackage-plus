(in-package :common-lisp-user)

(unless (find-package :defpackage-plus-1)
  (defpackage :defpackage-plus-1
    (:use #:cl #:alexandria)
    (:export #:defpackage+))

  (defpackage :defpackage+-user-1
    (:use #:cl #:defpackage-plus-1)))
