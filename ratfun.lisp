(in-package :gk-integrator)

;;;; rational functions

(defclass rational-function ()
  ((num
    :initarg :num
    :initform 0
    :accessor num
    :documentation "The numerator.")
   (den
    :initarg :den
    :initform 1
    :accessor den
    :documentation "The denominator.")))

(defun make-ratfun (num den)
  (reduce-ratfun (make-instance 'rational-function :num num :den den)))

(defun reduce-ratfun (ratfun)
  "Reduces the ratfun to canonical form."
  (with-slots ((num num) (den den)) ratfun
    (when (zerop num)
      (return-from reduce-ratfun 0))
    (when (and (typep den 'rational) (= den 1))
      (return-from reduce-ratfun num))
    (when (minusp den)
      (setf num (- num))
      (setf den (- den)))
    ;; remove rational coefficients
    (let ((r (* (poly-rat-part num) (poly-rat-part den))))
      (setf num (* num r) den (* den r)))
    ;; reduce
    (let ((gcd (gcd num den)))
      (setf num (polynomial-exact-division num gcd)
            den (polynomial-exact-division den gcd)))
    ratfun))

;; printing

(defmethod print-object ((object rational-function) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "(")
    (pretty-print-polynomial (slot-value object 'num) stream)
    (format stream ")/(")
    (pretty-print-polynomial (slot-value object 'den) stream)
    (format stream ")")))

;; polynomial division

(defmethod 2arg/ ((obj1 polynomial) (obj2 polynomial))
  (error "Rational functions not implemented yet."))

(defmethod 2arg/ ((obj1 rational) (obj2 polynomial))
  (error "Rational functions not implemented yet."))
