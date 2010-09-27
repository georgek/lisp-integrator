(in-package :gk-integrator)

(defun print-prefix (tree)
  (format t "~a~%" tree))

(defparameter *op-priorities*
  '((- . 1) (+ . 1)
    (* . 2) (/ . 2)
    (^ . 3)))

(defun higher-priority (op1 op2)
  (> (cdr (assoc op1 *op-priorities*))
     (cdr (assoc op2 *op-priorities*))))

(defun unary-op-p (op)
  (and (listp op) (= (length op) 2)))

(defun binary-op-p (op)
  (and (listp op) (= (length op) 3)))

(defun operator (op)
  (car op))

(defun operand1 (op)
  (cadr op))

(defun operand2 (op)
  (caddr op))

(defun print-infix (tree)
  (cond ((not (listp tree))
         (format t "~a" tree))
        ((unary-op-p tree)
         (if (eql (operator tree) '-)
             ;; unary minus
             (progn
               (format t "-")
               (if (binary-op-p (operand1 tree))
                   (progn
                     (format t "(")
                     (print-infix (operand1 tree))
                     (format t ")"))
                   (print-infix (operand1 tree))))))
        ((binary-op-p tree)
         ;; operand 1
         (if (and (binary-op-p (operand1 tree))
                  (higher-priority (operator tree) (operator (operand1 tree))))
             (progn
               (format t "(")
               (print-infix (operand1 tree))
               (format t ")"))
             (print-infix (operand1 tree)))
         ;; operator
         (format t "~a" (operator tree))
         ;; operand 2
         (if (and (binary-op-p (operand2 tree))
                  (higher-priority (operator tree) (operator (operand2 tree))))
             (progn
               (format t "(")
               (print-infix (operand2 tree))
               (format t ")"))
             (print-infix (operand2 tree))))))
