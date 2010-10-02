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
    (if (and (typep den 'rational) (= den 1))
        num
        ratfun)))

(defmethod copy ((object rational-function))
  (with-slots (num den) object
    (make-ratfun (copy num) (copy den))))

;; printing

(defmethod print-object ((object rational-function) stream)
  (print-unreadable-object (object stream :type t)
    (print-coefficient (slot-value object 'num) stream)
    (format stream "/")
    (print-coefficient (slot-value object 'den) stream)))

;; polynomial division

(defmethod 2arg/ (obj1 (obj2 polynomial))
  (make-ratfun obj1 obj2))

;; ratfun arithmetic

(defmethod numerator ((object rational-function))
  (slot-value object 'num))

(defmethod denominator ((object rational-function))
  (slot-value object 'den))

(defun ratfun2arg+- (obj1 obj2 op)
  (make-ratfun (funcall op
                        (* (numerator obj1) (denominator obj2))
                        (* (denominator obj1) (numerator obj2)))
               (* (denominator obj1) (denominator obj2))))

(defmethod 2arg+ ((obj1 rational-function) obj2)
  (ratfun2arg+- obj1 obj2 #'+))
  
(defmethod 2arg+ (obj1 (obj2 rational-function))
  (ratfun2arg+- obj1 obj2 #'+))

(defmethod 2arg- ((obj1 rational-function) obj2)
  (ratfun2arg+- obj1 obj2 #'-))
  
(defmethod 2arg- (obj1 (obj2 rational-function))
  (ratfun2arg+- obj1 obj2 #'-))

(defun ratfun2arg* (num1 den1 num2 den2)
  (make-ratfun (* num1 num2)
               (* den1 den2)))

(defmethod 2arg* ((obj1 rational-function) obj2)
  (ratfun2arg* (numerator obj1) (denominator obj1)
               (numerator obj2) (denominator obj2)))

(defmethod 2arg* (obj1 (obj2 rational-function))
  (ratfun2arg* (numerator obj1) (denominator obj1)
               (numerator obj2) (denominator obj2)))

(defmethod 2arg/ ((obj1 rational-function) obj2)
  (ratfun2arg* (numerator obj1) (denominator obj1)
               (denominator obj2) (numerator obj2)))

(defmethod 2arg/ (obj1 (obj2 rational-function))
  (ratfun2arg* (numerator obj1) (denominator obj1)
               (denominator obj2) (numerator obj2)))
