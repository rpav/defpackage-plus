(in-package :defpackage+-user-1)

(defpackage+ :defpackage-plus-1
  (:nicknames #:defpackage+-1)
  (:export-only

   ;; The Macro
   #:defpackage+
   #:defpackage+-dispatch

   ;; Ensuring package qualities
   #:ensure-use-only
   #:ensure-package
   #:ensure-nicknames
   #:ensure-export
   #:ensure-export-only
   #:ensure-export-warning

   ;; Importing/Inheriting
   #:import-from
   #:import-external-from
   #:shadowing-import-from
   #:inherit-from
   #:inherit-package
   #:inherit-package-except

   ;; Utility
   #:package-external-symbols
   #:package-symbols))
