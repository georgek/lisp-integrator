(in-package :gk-integrator)

;;;; generic predicates

(defgeneric zerop (object)
  (:documentation "Returns true if object is zero."))

(defmethod zerop ((object number))
  (cl:zerop object))
