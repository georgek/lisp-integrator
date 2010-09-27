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

;; methods for numbers
(defmethod 2arg+ ((obj1 number) (obj2 number))
  (cl:+ obj1 obj2))

(defmethod 2arg- ((obj1 number) (obj2 number))
  (cl:- obj1 obj2))

(defmethod 2arg* ((obj1 number) (obj2 number))
  (cl:* obj1 obj2))

(defmethod 2arg/ ((obj1 number) (obj2 number))
  (cl:/ obj1 obj2))

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
