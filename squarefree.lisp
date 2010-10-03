(in-package :gk-integrator)

;;;; squarefree factorisation

(defun squarefree (polynomial var)
  (let* ((S (pp polynomial var))
         (Sd (differentiate S var))
         (Sm (gcd S Sd))
         (Ss (/ S Sm))
         (Y (/ Sd Sm))
         (k 0)
         (As (make-array 5 :fill-pointer 0 :adjustable t)))
    (loop for Z = (- Y (differentiate Ss var))
       while (not (zerop Z)) do
         (vector-push-extend (gcd Ss Z) As)
         (setf Ss (/ Ss (elt As k)))
         (setf Y (/ Z (elt As k)))
         (incf k))
    (vector-push-extend Ss As)
    (setf (elt As 0) (* (elt As 0) (content polynomial var)))
    As))
