(in-package :gk-integrator)

;;;; generic predicates

(defgeneric zerop (object)
  (:documentation "Returns true if object is zero."))

(defmethod zerop ((object number))
  (cl:zerop object))

(defgeneric minusp (object)
  (:documentation "Returns true if object is negative."))

(defmethod minusp ((object number))
  (cl:minusp object))

(defgeneric plusp (object)
  (:documentation "Returns true if object is negative."))

(defmethod plusp ((object number))
  (cl:plusp object))
