(in-package :cl-fst)

;; the global fst context record
(defparameter *fst-cntxt* (initialize-cfsm)) ;; Lauri: try using one for each thread

(defclass fst-net ()
  ((fst-cntxt :initform *fst-cntxt* :initarg :fst-cntxt :reader fst-cntxt)
   (fst-file :initform nil :initarg :file :reader fst-file)
   (net-ptr :initform nil :reader net-ptr)
   (name :initform nil :initarg :name :reader name)
   #+ignore
   (Lock :initform (make-process-lock :name (symbol-name (gensym "net-lock-")))
	 :reader fst-net-lock)))

;;(print (symbol-name (gensym "lock-")))

(defclass fst-tokenizer (fst-net)
  ((token-boundary :initform "TB" :initarg :token-boundary :reader token-boundary)
   (fail-token :initform "FAILED_TOKEN" :initarg :fail-token :reader fail-token)
   (escape-p :initform nil :initarg :escape-p :reader escape-p)))

(defclass fst-pattern-matcher (fst-net)
  ())

(defparameter +fst-net-lock+ (make-process-lock :name "fst-net-lock"))

(defmethod initialize-instance :after ((net fst-net) &key &allow-other-keys)
  (with-slots (fst-cntxt fst-file net-ptr) net
    (with-process-lock (+fst-net-lock+) ;; the global lock
      (assert (probe-file fst-file))
      (setf net-ptr (load-net (namestring (translate-logical-pathname fst-file)) fst-cntxt))
      (assert (not (null-pointer-p net-ptr))))))

(defun context-page-buffer (fst-cntxt)
  (foreign-slot-value
   (foreign-slot-value fst-cntxt 'cfsm-context 'temp-bufs)
   'temporary-buffers 'page-buffer))

(defun side-to-id (side)
  (ecase side
    (:upper 0)
    (:lower 1)
    (:both-sides 2)))

;;(defparameter +lookup-lock+ (make-process-lock :name "lookup-lock"))

;; fst-lookup() is thread-safe since each thread gets its own applyer object
(defmethod fst-lookup ((net fst-net) (strings list) function &key (side :lower))
  (with-process-lock (+fst-net-lock+)
    (with-slots (fst-cntxt net-ptr) net
      (let ((applyer (init-apply net-ptr (side-to-id side) fst-cntxt)))
	(unwind-protect
	     (loop for string in strings
		do (funcall function string (apply-to-string string applyer) net))
	  (free-applyer applyer))))))

;; look up using the nets in list until a result is found (equal to priority union of nets)
(defmethod fst-lookup ((nets list) (string string) function
		       &key (side :lower) (mode :priority-union))
  (loop with found = nil
       and res = nil
     for net in nets
     until (with-slots (fst-cntxt net-ptr) net
	     (with-process-lock (+fst-net-lock+)
	       (let ((applyer (init-apply net-ptr (side-to-id side) fst-cntxt)))
		 (unwind-protect
		      (let ((result (apply-to-string string applyer)))
			(unless (zerop (length result))
			  (setf res (if res
					(u:concat res result)
					result))
			  ;; return
			  (eq mode :priority-union)))
		   (free-applyer applyer)))))
     finally (funcall function string (or res "") net)))

#+orig
(defmethod fst-lookup ((nets list) (string string) function
		       &key (side :lower) (mode :priority-union))
  (loop with found = nil
     for net in nets
     until (with-slots (fst-cntxt net-ptr) net
	     (with-process-lock (+fst-net-lock+)
	       (let ((applyer (init-apply net-ptr (side-to-id side) fst-cntxt)))
		 (unwind-protect
		      (let ((result (apply-to-string string applyer)))
			(unless (zerop (length result))
			  (funcall function string result net)
			  (and (setf found t)
			       (eq mode :priority-union))))
		   (free-applyer applyer)))))
     finally (unless found (funcall function string "" nil))))

(defmethod fst-lookup ((net fst-net) (string string) function &key (side :lower))
  (with-process-lock (+fst-net-lock+)
    (with-slots (fst-cntxt net-ptr) net
      (let ((applyer (init-apply net-ptr (side-to-id side) fst-cntxt)))
	(unwind-protect
	     (funcall function string (apply-to-string string applyer) net)
	  (free-applyer applyer))))))

(defparameter +tokenizer-lock+ (make-process-lock :name "tokenizer-lock"))

;; what does this one do??
;; have to use locking since tokenizing is not (yet) thread safe
(defmethod fst-tokenize ((net fst-tokenizer) (string string))
  (with-slots (fst-cntxt net-ptr) net
    (let ((page (new-page))
	  (tok nil)
	  (escape-p (escape-p net)))
      (unwind-protect
	   (with-foreign-string (cstr string)
	     (with-process-lock (+fst-net-lock+);;  (+tokenizer-lock+)
	       (reset-page page)
	       (setf tok (new-tokenizer
			  net-ptr
			  cstr
			  0
			  (single-to-id (token-boundary net))
			  (single-to-id (fail-token net))
			  fst-cntxt))
	       (unless (null-pointer-p tok)
		 (loop for tok-net = (next-token-net tok)
		    until (null-pointer-p tok-net)
		    do (words-to-page tok-net (side-to-id :upper) (if escape-p 1 0) page)
		    (free-network tok-net))
		 (prog1 (foreign-slot-value page 'page 'string)
		   (reset-page page)))))
	(unless (null-pointer-p tok)
	  (free-tokenizer tok))
	(unless (null-pointer-p page)
	  (free-page page)))))) 

(defmethod fst-map-tokens ((net fst-tokenizer) (string string) function
			   &key (split-at-nl t) split-char)
  (with-slots (fst-cntxt net-ptr) net
    (let ((page (new-page))
	  (tok nil)
	  (escape-p (escape-p net)))
      ;;(print (list :enter ccl::*current-process*))
      (with-process-lock (+fst-net-lock+) ;; (+tokenizer-lock+)
	(unwind-protect
	     (with-foreign-string (cstr string)
	       (reset-page page)
	       (setf tok (new-tokenizer ;; Lauri: use restart_tokenizer()
			  net-ptr
			  cstr
			  0
			  (single-to-id (token-boundary net))
			  (single-to-id (fail-token net))
			  fst-cntxt))
	       (unless (null-pointer-p tok)
		 (loop for tok-net = (next-token-net tok)
		    until (null-pointer-p tok-net)
		    do (words-to-page tok-net (side-to-id :upper) (if escape-p 1 0) page)
		    (if split-at-nl
			(mapc (lambda (token)
				(funcall function token))
			      (u:split (foreign-slot-value page 'page 'string)
				       (or split-char #\newline) nil nil t))
			(funcall function (string-trim #.(string #\newline)
						       (foreign-slot-value page 'page 'string))))
		    (reset-page page)
		    (free-network tok-net))))
	  ;;(print (list :free ccl::*current-process*))
	  (unless (null-pointer-p tok)
	    (free-tokenizer tok))
	  (unless (null-pointer-p page)
	    (free-page page))
	  ;;(print (list :exit ccl::*current-process*))
	  )))))

#+orig
(defmethod fst-map-tokens ((net fst-tokenizer) (string string) function &key (split-at-nl t))
  (with-slots (fst-cntxt net-ptr) net
    (let ((page (new-page))
	  (tok nil)
	  (escape-p (escape-p net)))
      (unwind-protect
	   (with-foreign-string (cstr string)
	     (with-process-lock (+tokenizer-lock+)
	       (reset-page page)
	       (setf tok (new-tokenizer ;; Lauri: use restart_tokenizer()
			  net-ptr
			  cstr
			  0
			  (single-to-id (token-boundary net))
			  (single-to-id (fail-token net))
			  fst-cntxt))
	       (unless (null-pointer-p tok)
		 (loop for tok-net = (next-token-net tok)
		    until (null-pointer-p tok-net)
		    do (words-to-page tok-net (side-to-id :upper) (if escape-p 1 0) page)
		      (if split-at-nl
			  (mapc (lambda (token)
				  (funcall function token))
				(u:split (foreign-slot-value page 'page 'string) #\newline nil nil t))
			  (funcall function (string-trim #.(string #\newline) (foreign-slot-value page 'page 'string))))
		    (reset-page page)
		    (free-network tok-net)))))
	(unless (null-pointer-p tok)
	  (free-tokenizer tok))
	(unless (null-pointer-p page)
	  (free-page page))))))

#+test
(defparameter *tokenizer1*
  (make-instance 'cl-fst:fst-tokenizer :token-boundary #.(string #\newline)
		 :file "~/lisp/projects/iness/morphology/regex/tokenize.fst"))

#+test
(cl-fst:fst-map-tokens *tokenizer1* "men også cand.mag. dr. art." #'print :split-at-nl nil)

#+test ;; send bug report!
(cl-fst:fst-map-tokens *tokenizer1* "Dette er bra." #'print :split-at-nl nil)

#+test ;; accumulate in page and funcall at the end
(defmethod fst-map-tokens ((net fst-tokenizer) (string string) function &key &allow-other-keys)
  (with-slots (fst-cntxt net-ptr) net
    (let ((page (new-page))
	  (tok nil)
	  (escape-p (escape-p net)))
      (unwind-protect
	   (with-foreign-string (cstr string)
	     (with-process-lock (+tokenizer-lock+)
	       (reset-page page)
	       (setf tok (new-tokenizer
			  net-ptr
			  cstr
			  0
			  (single-to-id (token-boundary net))
			  (single-to-id (fail-token net))
			  fst-cntxt))
	       (unless (null-pointer-p tok)
		 (loop for tok-net = (next-token-net tok)
		    until (null-pointer-p tok-net)
		    do (words-to-page tok-net (side-to-id :upper) (if escape-p 1 0) page)
		      ;;(funcall function (string-trim #.(string #\newline) (foreign-slot-value page 'page 'string)))
		    ;;(reset-page page)
		    (free-network tok-net)))))
	(unless (null-pointer-p tok)
	  (free-tokenizer tok))
	(funcall function (string-trim #.(string #\newline) (foreign-slot-value page 'page 'string)))
	(unless (null-pointer-p page)
	  (free-page page))))))

(defun set-pattern-match-type (type value)
  (let ((slot (ecase type
		(:count 'count-patterns)
		(:delete 'delete-patterns)
		(:extract 'extract-patterns)
		(:locate 'locate-patterns)
		(:mark 'mark-patterns))))
    (setf (foreign-slot-value
	   (foreign-slot-value (int-parameters) 'interface-parameters 'io)
	   'io slot)
	  (if value 1 0))))

(defmethod fst-apply-patterns ((net fst-pattern-matcher) (string string) &key from-file-p)
  (with-slots (fst-cntxt fst-file net-ptr) net
    (let ((applyer nil))
      (unwind-protect
	   (with-foreign-string (cstr string)
	     (with-process-lock (+fst-net-lock+) ;;  (+tokenizer-lock+)
	       (setf applyer
		     (if from-file-p
			 (make-pattern-applyer
			  fst-file
			  cstr
			  (null-pointer)
			  (null-pointer)
			  (side-to-id :lower)
			  fst-cntxt)
			 (new-pattern-applyer
			  net-ptr
			  cstr
			  0
			  0
			  (side-to-id :lower)
			  fst-cntxt))))
	     (let ((buffer (next-pattern-output applyer)))
	       (foreign-slot-value buffer 'string-buffer 'string)))
	(when applyer
	  (free-applyer applyer))))))

:eof
