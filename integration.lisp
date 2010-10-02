(in-package :gk-integrator)

;;;; integration

(defgeneric integrate (object variable)
  (:documentation "Returns the indefinite integral of the object."))

(defmethod integrate ((object rational) variable)
  (make-mono-poly variable object 1))

(defmethod integrate ((object polynomial) variable)
  (let ((result (copy object)))
    (with-poly result
      (cond
        ((var-higher-rank-p variable variable-name)
         (setf result (make-mono-poly variable result 1)))
        ((var-higher-rank-p variable-name variable)
         (error "Trying to integrate in lower ranking variable!"))
        (t
         (loop for m in (nextm monomials) do
              (setf (coefficient m) (/ (coefficient m) (incf (power m))))))))
    result))
