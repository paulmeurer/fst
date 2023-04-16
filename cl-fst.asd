;;;   -*- Mode: LISP; Package: COMMON-LISP-USER; BASE: 10; Syntax: ANSI-Common-Lisp; -*-

;; Copyright (c) 2014, Paul Meurer, University of Bergen
;; https://clarino.uib.no
;; All rights reserved.
;; 

(in-package :cl-user)

(asdf:defsystem :cl-fst
  :depends-on (cffi acl-compat utilities)
  :serial t
  :components
  ((:file "package")
   (:file "load-fst")
   (:file "fst-api")
   (:file "cl-fst")))

:eof
