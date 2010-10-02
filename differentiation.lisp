(in-package :gk-integrator)

;;;; differentiation stuff

(defgeneric differentiate (object variable)
  (:documentation "Returns the derivative of the object."))

(defmethod differentiate ((object rational) variable)
  0)

(defmethod differentiate ((object polynomial) variable)
  (let ((result (copy object)))
    (with-poly result
      (cond
        ((var-higher-rank-p variable variable-name)
         (setf result 0))
        ((var-higher-rank-p variable-name variable)
         (error "Trying to differentiate in lower ranking variable!"))
        (t
         (loop for m on monomials do
              (cond
                ((zerop (power (car (nextm m))))
                 (setf (cdr m) (cddr m)))
                (t
                 (setf (coefficient (car (nextm m)))
                       (* (coefficient (car (nextm m)))
                          (power (car (nextm m)))))
                 (decf (power (car (nextm m))))))))))
    result))

(defmethod differentiate ((object rational-function) variable)
  (- (/ (differentiate (numerator object) variable)
        (denominator object))
     (/ (* (numerator object)
           (differentiate (denominator object) variable))
        (^ (denominator object) 2))))
