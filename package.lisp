(in-package :cl-user)

(defpackage #:cl-fst-internal
  (:use))
   
(defpackage #:cl-fst
  (:export #:fst-tokenize #:fst-map-tokens #:fst-net #:fst-tokenizer #:fst-pattern-matcher
	   #:token-boundary #:fst-lookup #:fst-apply-patterns #:set-pattern-match-type)
  (:use #:cl #:cffi #:cl-fst-internal #:acl-compat.mp))

:eof