(in-package :gk-integrator)

;;;; greatest common divisors for polynomials

(defgeneric 2arg-gcd (poly1 poly2)
  (:documentation "Calculates the greatest common divisor."))

(defun gcd (&rest polynomials)
  (cond
    ((null polynomials)
     (cl:gcd))
    ((endp (cdr polynomials))
     (car polynomials))
    (t
     (reduce #'2arg-gcd polynomials))))

(defmethod 2arg-gcd ((poly1 rational) (poly2 rational))
  (if (or
        (not (= 1 (denominator poly1)))
        (not (= 1 (denominator poly2))))
      1
      (cl:gcd poly1 poly2)))

(defmethod 2arg-gcd ((poly1 polynomial) (poly2 rational))
  ;; this is simply the gcd of contents since gcd of pp's is 1
  (gcd (content poly1 (slot-value poly1 'variable-name)) poly2))

(defmethod 2arg-gcd ((poly1 rational) (poly2 polynomial))
  (2arg-gcd poly2 poly1))

(defmethod 2arg-gcd ((poly1 polynomial) (poly2 polynomial))
  (subresultantgcd poly1 poly2 (poly-highest-var poly1 poly2)))

;; WWGCD works before I can be bothered to implement a better one
(defun wwgcd (a b var)
  (let ((c (pp a var))
        (d (pp b var))
        (r 0)
        (gamma))
    (loop while (not (zerop d)) do
         (setf r (prem c d))
         (setf c d)
         (setf d (pp r var)))
    (setf gamma (gcd (content a var) (content b var)))
    (* gamma c)))

;; subresultant gcd algorithm
(defun subresultantgcd (a b var)
  (let ((c a)
        (d b)
        (r 0)
        (beta)
        (gamma)
        (delta (- (deg a var) (deg b var)))
        (psi -1))
    ;; (format t "R_0 = ~a~%R_1 = ~a~%" c d)
    ;; (format t "   psi_1 = ~a~%" psi)
    ;; (format t "   delta_1 = ~a~%" delta)
    (setf beta (^ -1 (1+ delta)))
    ;; (format t "   beta_1 = ~a~%" beta)
    (loop for i = 2 then (1+ i)
       while (not (zerop d)) do
         (setf r (prem c d))
         (setf c d)
         (setf d r)
         ; remove beta
         (when (not (zerop beta))
           (setf d (/ d beta)))
         ;; (format t "R_~a = ~a~%" i d)
         ; calculate new beta
         (setf psi (* (^ (- (lc c var)) delta)
                      (^ psi (- 1 delta))))
         ;; (format t "   psi_~a = ~a~%" i psi)
         (setf delta (- (deg c var) (deg d var)))
         ;; (format t "   delta_~a = ~a~%" i delta)
         (setf beta (* (- (lc c var))
                       (^ psi delta)))
         ;; (format t "   beta_~a = ~a~%" i beta)
         )
    (setf gamma (gcd (content a var) (content b var)))
    (* gamma (pp c var))))
