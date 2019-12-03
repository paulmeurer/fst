(in-package :cl-fst)


;; adapt to your environment

(define-foreign-library libfst
  (:darwin
   #+X86-64
   (:default "/usr/local/fst/cfsm_api/macosx64/lib/libcfsm")
   #+X86
   (:default "/usr/local/fst/cfsm_api/macosx32/lib/libcfsm"))
  (:unix
   #+(and X86-64 (or (not iness) gekko))
   (:default "/usr/local/fst/test/cfsm_api/linux64/lib/libcfsm")
   ;;(:default "/usr/local/fst/cfsm_api/linux64/lib/libcfsm")
   #+(and X86-64 iness (not gekko))
   (:default "/home/iness/local/fst/cfsm_api/linux64/lib/libcfsm")))

(use-foreign-library libfst)

:eof
