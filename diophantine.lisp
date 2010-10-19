(in-package :gk-integrator)

;;;; extended Euclidean algorithm for solving diophantine equations

(defun half-extended-euclidean (a b)
  (let ((a1 1)
        (b1 0)
        r1)
    (loop with q and r
       until (zerop b) do
         (multiple-value-bind (quo rem)
             (polynomial-division a b)
           (setf q quo)
           (setf r rem))
         (setf a b
               b r
               r1 (- a1 (* q b1))
               a1 b1
               b1 r1))
    (values a1 a)))

(defun extended-euclidean (a b)
  (multiple-value-bind (s g) (half-extended-euclidean a b)
    (values s
            (polynomial-exact-division (- g (* s a)) b)
            g)))

(defun half-solve-diophantine (a b c)
  (multiple-value-bind (s g) (half-extended-euclidean a b)
    (multiple-value-bind (q r) (polynomial-division c g)
      (unless (zerop r)
        (error "c is not in the ideal generated by a and b!"))
      (setf s (* q s))
      (let ((var (poly-highest-var s b)))
        (when (and
               (not (zerop s))
               (>= (deg s var) (deg b var)))
          (multiple-value-bind (quo rem) (polynomial-division s b)
            (setf q quo)
            (setf r rem))
          (setf s r))
        s))))

(defun solve-diophantine (a b c)
  (let ((s (half-solve-diophantine a b c)))
    (values s
            (polynomial-exact-division (- c (* s a)) b))))
