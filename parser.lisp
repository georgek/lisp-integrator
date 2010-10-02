(in-package :gk-integrator)

;;; lexing stuff
(defun make-poly-node (var)
  (make-mono-poly (intern var) 1 1))

(deflexer scan-math ()
  ("0|[1-9][0-9]*" integer parse-integer)
  ("[a-zA-Z]+" variable make-poly-node)
  ("-" - intern)
  ("\\+" + intern)
  ("\\*" * intern)
  ("/" / intern)
  ("\\^" ^ intern)
  ("\\(" \( intern)
  ("\\)" \) intern)
  ("," \, intern)
  ("[ \t]+" space))

;; returns function to be passed to parser
(defun math-lexer (string)
  ;; get tokens from string using lexer
  (let ((tokens (remove-if (lambda (x) (eql (car x) 'space))
                           (lex-all 'scan-math string))))
    (lambda ()
      (let ((token (pop tokens)))
        (if (null token)
            (values nil nil)
            (values (car token) (cdr token)))))))

;;; parsing stuff

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun i2p (a b c)
    "Infix to prefix"
    (list b a c))

  (defun k-2-3 (a b c)
    "Second out of three"
    (declare (ignore a c))
    b))

(define-parser *math-parser*
  (:start-symbol expression)
  (:terminals (integer variable - + * / ^ \( \) \,))
  (:precedence ((:left ^) (:left * /) (:left + -)))

  (expression
   (expression + expression #'i2p)
   (expression - expression #'i2p)
   (expression * expression #'i2p)
   (expression / expression #'i2p)
   (expression ^ expression #'i2p)
   term)

  (term
   integer
   variable
   (- term)
   (\( expression \) #'k-2-3)))

(defun read-inputs ()
  (do ((end nil))
      (end)
    (format t "> ")
    (let ((input (read-line)))
      (if (string= input "quit")
          (setf end t)
          (let ((tree (parse-with-lexer (math-lexer input) *math-parser*)))
            ;; (print-prefix tree)
            ;; (print-infix tree)
            ;; (format t "~%~%")
            (format t "~a~%" (eval tree)))))))

(defun parse-infix (string)
  (eval (parse-with-lexer (math-lexer string) *math-parser*)))
