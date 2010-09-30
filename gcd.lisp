(in-package :gk-integrator)

;;;; greatest common divisors for polynomials

(defgeneric 2arg-gcd (poly1 poly2)
  (:documentation "Calculates the greatest common divisor."))

(defun gcd (&rest polynomials)
  (cond
    ((null polynomials)
     0)
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
  (gcd (content poly1) poly2))

(defmethod 2arg-gcd ((poly1 rational) (poly2 polynomial))
  (2arg-gcd poly2 poly1))

(defmethod 2arg-gcd ((poly1 polynomial) (poly2 polynomial))
  (wwgcd poly1 poly2))

;; WWGCD works before I can be bothered to implement a better one
(defun wwgcd (a b)
  (let ((c (pp a))
        (d (pp b))
        (r 0)
        (gamma))
    (loop while (not (zerop d)) do
         (setf r (prem c d))
         (setf c d)
         (setf d (pp r)))
    (setf gamma (gcd (content a) (content b)))
    (* gamma c)))
