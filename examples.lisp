(in-package :cl-fst)

;; testing

(print (libcfsm-version))

#+orig
(defparameter *net*
  (make-instance 'fst-net :file "/usr/local/fst/cfsm_api/src/apply/test/FrenchEnglish.fst"))

(defparameter *net*
  (make-instance 'fst-net :file "projects:iness;morphology;regex;nob-analyze.fst"))

(defparameter *net1*
  (make-instance 'fst-net :file "projects:iness;morphology;regex;nob-analyze.fst"))

(fst-lookup *net* "three" (lambda (str val) (format t "~a	~a~%" str val)))

(defparameter *tokenizer*
  (make-instance 'fst-tokenizer :file "/usr/local/fst/cfsm_api/src/tokenize/test/token.fst"))

(fst-map-tokens *tokenizer* "Wie man ein Huhn kämmt." #'print :split-at-nl nil)

(defparameter *patterns*
  (make-instance 'fst-pattern-matcher :file "/usr/local/fst/cfsm_api/src/pmatch/test/patterns.fst"))

(defparameter *text* "My social security number is 123-45-6789.  You can call me at (650)
812-4567. Grace Kelly and Gary Grant starred in \"To Catch a Thief\".
Charlie chaplin played Hitler in THE GREAT DICTATOR.")

(set-pattern-match-type :locate t)

(print (fst-apply-patterns *patterns* *text*))

(set-pattern-match-type :extract t)

(print (fst-apply-patterns *patterns* *text*))



;; crashes without locking
(progn
  (process-run-function
   "tok1"
   (lambda ()
     (dotimes (i 100)
       (fst-map-tokens *tokenizer* "Wie man ein Huhn kämmt." #'print))))
  (process-run-function
   "tok2"
   (lambda ()
     (dotimes (i 100)
       (fst-map-tokens *tokenizer* "Wie man ein Huhn kämmt." #'print)))))

(defun random-string (size)
  (let ((str (make-string size)))
    (loop for i below size
	 do (setf (char str i) (code-char (+ (char-code #\a) (random 26)))))
    str))

(dotimes (i 100)
  (print (random-string 10)))

;; seems to be OK; but not in recent CCL? (Seems to work now…)
(dotimes (i 100)
  (progn
    (process-run-function
     "tok1"
     (lambda ()
       (dotimes (i 100)
	 (fst-lookup *net* (random-string (random 11)) (lambda (str val) (declare (ignore str val)))))))
    (process-run-function
     "tok2"
     (lambda ()
       (dotimes (i 100)
	 (fst-lookup *net* (random-string (random 10)) (lambda (str val) (declare (ignore str val))))))))
  (print i)
  (sleep 1))

(progn
  (process-run-function
   "tok1"
   (lambda ()
     (dotimes (i 1000)
       (fst-lookup *net* (random-string (random 11)) (lambda (str val) (declare (ignore str val)))))))
  (process-run-function
   "tok2"
   (lambda ()
     (dotimes (i 1000)
       (fst-lookup *net1* (random-string (random 10)) (lambda (str val) (declare (ignore str val))))))))

(dotimes (i 10)
  (process-run-function
   "lookup"
   (lambda ()
     (let ((net (make-instance 'fst-net :file "projects:iness;morphology;regex;nob-analyze.fst")))
       (dotimes (i 1000)
	 (fst-lookup net (random-string (random 11)) (lambda (str val) (declare (ignore str val)))))))))


(defparameter *nob-ner*
  (make-instance 'fst-pattern-matcher :file "lisp:projects;iness;morphology;regex;nob-ner.fst"))

(defparameter *nob-ner*
  (make-instance 'fst-pattern-matcher :file "/usr/local/xledir/pargram/norwegian/bokmal/morphology/nob-ner.fst"))


(print (fst-apply-patterns
	*nob-ner*
	"Vi er på Den kongelige vitenskapsakademi ."
	))


:eof