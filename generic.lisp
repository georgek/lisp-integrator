(in-package :gk-integrator)

;;;; general generic functions

(defgeneric copy (object)
  (:documentation "Returns a copy of the object."))

(defmethod copy ((object number))
  object)

