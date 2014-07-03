(defpackage :defpackage-plus-1.0
  (:use #:cl #:alexandria)
  (:export #:defpackage+))

(defpackage :defpackage+-user-1.0
  (:use #:cl #:defpackage-plus-1.0))
