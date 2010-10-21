(in-package :gk-integrator)

;;; lexing stuff
(deflexer scan-math ()
  ("0|[1-9][0-9]*" integer parse-integer)
  ("[a-zA-Z]+" name)
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
    b)

  (defun exp-list-append (a b c)
    "Append expression to expression list."
    (declare (ignore b))
    (append a (list c)))
  
  (defun if2p (a b c d)
    (declare (ignore b d))
    (append (list (read-from-string a)) c))
  
  (defun make-poly-node (var)
    (make-mono-poly (add-var (intern var)) 1 1)))

(define-parser *math-parser*
  (:start-symbol expression-list)
  (:terminals (integer name - + * / ^ \( \) \,))
  (:precedence ((:left ^) (:left * /) (:left + -)))

  (expression-list
   (expression-list \, expression #'exp-list-append)
   (expression #'list))

  (expression
   (expression + expression #'i2p)
   (expression - expression #'i2p)
   (expression * expression #'i2p)
   (expression / expression #'i2p)
   (expression ^ expression #'i2p)
   term)

  (term
   integer
   (name #'make-poly-node)              ; variables
   (- term)                             ; unary minus
   (\( expression \) #'k-2-3)           ; brackets
   (name \( expression-list \) #'if2p))) ; functions

(defun read-inputs ()
  (do ((end nil))
      (end)
    (format t "> ")
    (let ((input (read-line)))
      (if (string= input "quit")
          (setf end t)
          (let* ((*variable-table* (make-hash-table))
                 (exp-list (parse-with-lexer (math-lexer input) *math-parser*)))
            ;; (print-prefix exp-list)
            ;; (print-infix exp-list)
            ;; (format t "~%~%")
            (loop for tree on exp-list do
                 (format t "~a" (eval (car tree)))
                 (unless (endp (cdr tree))
                   (format t ", ")))
            (format t "~%"))))))

(defun parse-infix (string)
  (eval (parse-with-lexer (math-lexer string) *math-parser*)))

(defun parse-infix-mac (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((string (read stream)))
    `(parse-infix ,string)))

(set-dispatch-macro-character #\# #\i #'parse-infix-mac)
