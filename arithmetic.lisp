(in-package :gk-integrator)

;;;; generic arithmetic functions
;;;; binary versions for arithmetic on numbers are defined here

;; generic two argument versions
(defgeneric 2arg+ (obj1 obj2)
  (:documentation "Adds two objects together"))

(defgeneric 2arg- (obj1 obj2)
  (:documentation "Subtracts an object from the other"))

(defgeneric 2arg* (obj1 obj2)
  (:documentation "Multiplies two objects together"))

(defgeneric 2arg/ (obj1 obj2)
  (:documentation "Divides an object by the other"))

(defgeneric ^ (base power)
  (:documentation "Multiplies base together power times"))

;; methods for numbers
(defmethod 2arg+ ((obj1 number) (obj2 number))
  (cl:+ obj1 obj2))

(defmethod 2arg- ((obj1 number) (obj2 number))
  (cl:- obj1 obj2))

(defmethod 2arg* ((obj1 number) (obj2 number))
  (cl:* obj1 obj2))

(defmethod 2arg/ ((obj1 number) (obj2 number))
  (cl:/ obj1 obj2))

(defmethod ^ ((base number) power)
  (expt base power))

;; generic version
(defmethod ^ (base power)
  (if (zerop power)
      1
      (let ((result (copy base))
            (pow (abs power)))
        (do ((mask (ash 1 (- (integer-length pow) 2)) (ash mask -1)))
            ((= mask 0))
          (if (logtest mask pow)
              (setf result (* (* result result) base))
              (setf result (* result result))))
        (if (minusp power)
            (/ 1 result)
            result))))

;; n-ary versions
(defun + (&rest objs)
  (if (null objs)
      (cl:+)
      (reduce #'2arg+ objs :initial-value 0)))

(defun - (&rest objs)
  (cond ((null objs)
         (error "Invalid number of arguments."))
        ((endp (cdr objs))
         (2arg- 0 (car objs)))
        (t
         (reduce #'2arg- objs))))

(defun * (&rest objs)
  (if (null objs)
      (cl:*)
      (reduce #'2arg* objs :initial-value 1)))

(defun / (&rest objs)
  (cond ((null objs)
         (error "Invalid number of arguments."))
        ((endp (cdr objs))
         (2arg/ 1 (car objs)))
        (t
         (reduce #'2arg/ objs))))

(defgeneric numerator (object)
  (:documentation "Returns the numerator."))

(defmethod numerator ((object rational))
  (cl:numerator object))

(defgeneric denominator (object)
  (:documentation "Returns the denominator."))

(defmethod denominator ((object rational))
  (cl:denominator object))
