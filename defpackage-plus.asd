
(defpackage :defpackage-plus.asdf
  (:use #:cl #:asdf))

(in-package :defpackage-plus.asdf)

(defsystem :defpackage-plus
  :description "Extensible DEFPACKAGE with version support"
  :author "Ryan Pavlik"
  :license "BSD-2-Clause"
  :version "1.0"

  :depends-on (:alexandria)
  :pathname "src"
  :serial t

  :components
  ((:file "package")
   (:file "utility")
   (:file "ensure")
   (:file "inherit")
   (:file "local-nicknames")
   (:file "defpackage-plus")
   (:file "package-plus")))
