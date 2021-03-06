(in-package :gk-integrator)

;;;; polynomials
;;;;
;;;; polynomials are simply a variable and a list of all nonzero monomials
;;;; beginning with a list head containing nil instead of a monomial, each
;;;; monomial is a cons containing the coefficient, which is either rational
;;;; or a polynomial in another variable, and the power

(defclass polynomial ()
  ((variable-name
    :initarg :variable-name
    :initform (error "Must supply a variable name.")
    :accessor variable-name
    :documentation "The variable of the polynomial.")
   (monomials
    :initarg :monomials
    :initform nil
    :accessor monomials
    :documentation "List of all nonzero monomials.")))

;; some helper macros
;; includes slots for ony polynomial
(defmacro with-poly (poly &body body)
  `(with-slots ((variable-name variable-name) (monomials monomials)) ,poly
     ,@body))

;; makes a result by copying 
(defmacro copy-into-result-poly (result variable-name monomials &body body)
  `(let ((,result (make-instance 'polynomial
                                 :variable-name ,variable-name
                                 :monomials (copy-tree ,monomials))))
     ,@body))

;; includes slots for two polynomials
(defmacro with-polys (poly1 poly2 &body body)
  `(with-slots ((variable-name1 variable-name) (monomials1 monomials)) ,poly1
     (with-slots ((variable-name2 variable-name) (monomials2 monomials)) ,poly2
       ,@body)))

;;; creating and copying polynomials

(defun make-mono-poly (variable &optional (coefficient 1) (power 1))
  "Returns a new polynomial in the given variable with a single monomial with
the given coefficient and power.  Useful for beginning to build polynomials
from a parse tree."
  (make-instance 'polynomial
                 :variable-name variable
                 :monomials (list nil (monomial coefficient power))))

(defun make-zero-poly (variable)
  "Makes a zero polynomial in the given variable, this shouldn't generally be
used, but is useful in combination with add-monomial for building up
polynomials."
  (make-instance 'polynomial
                 :variable-name variable
                 :monomials (list nil)))

(defmethod copy ((polynomial polynomial))
  "Get a new copy of the polynomial."
  (with-poly polynomial
    (make-instance 'polynomial
                   :variable-name variable-name
                   :monomials (copy-tree monomials))))

;;; access parts of polynomials and coefficients

(defun monomial (coefficient power)
  "Make a new monomial."
  (cons coefficient power))

(defun coefficient (monomial)
  "Get the coefficient part of the monomial."
  (car monomial))

(defun (setf coefficient) (coefficient monomial)
  (setf (car monomial) coefficient))

(defun power (monomial)
  "Get the power of the monomial."
  (cdr monomial))

(defun (setf power) (power monomial)
  (setf (cdr monomial) power))

(defun nextm (monomial)
  "Get the next monomial in the list, or nil if there is none."
  (cdr monomial))

(defun (setf nextm) (next monomial)
  (setf (cdr monomial) next))

(defgeneric poly-highest-var (poly1 poly2)
  (:documentation "Returns the highest ranking variable in either polynomial."))

(defmethod poly-highest-var ((poly1 polynomial) (poly2 rational))
  (slot-value poly1 'variable-name))

(defmethod poly-highest-var ((poly1 rational) (poly2 polynomial))
  (slot-value poly2 'variable-name))

(defmethod poly-highest-var ((poly1 polynomial) (poly2 polynomial))
  (with-polys poly1 poly2
    (if (var-higher-rank-p variable-name1 variable-name2)
        variable-name1
        variable-name2)))

;;; printing functions

;; coefficient printing
(defgeneric print-coefficient (coefficient &optional stream)
  (:documentation "Prints a coefficient."))

(defmethod print-coefficient ((coefficient rational) &optional (stream t))
  (format stream "~a" coefficient))

(defmethod print-coefficient ((coefficient polynomial) &optional (stream t))
  (format stream "(")
  (pretty-print-polynomial coefficient stream)
  (format stream ")"))

;; default print for polynomial objects
(defmethod print-object ((object polynomial) stream)
  (print-unreadable-object (object stream :type t)
    (pretty-print-polynomial object stream)))

(defun pretty-print-polynomial (polynomial &optional (stream t))
  (with-slots (variable-name monomials) polynomial
    (if (endp (cdr monomials))
        (format stream "zero")
        (progn
          (print-coefficient (car (car (cdr monomials))) stream)
          (format stream "*~a^~a" variable-name (cdadr monomials))
          (do ((mono (cddr monomials) (cdr mono)))
              ((endp mono))
            (format stream " + ")
            (print-coefficient (car (car mono)) stream)
            (format stream "*~a^~a" variable-name (cdr (car mono))))))))

;;; arithmetic

;; add a monomial to the polynomial (destructively)
(defun add-monomial (monomial polynomial &optional (op #'+))
  (with-slots ((monomials monomials)) polynomial
    (let ((m monomials))
      (do ()
          ((or (endp (nextm m))
               (<= (power (car (nextm m))) (power monomial))))
        (setf m (nextm m)))
      (cond
        ((or                            ; insert new monomial
          (endp (nextm m))
          (< (power (car (nextm m))) (power monomial)))
         (let ((newm (push (monomial (coefficient monomial) (power monomial))
                           (nextm m))))
           (setf (coefficient (car newm))
                 (funcall op (coefficient (car newm))))
           (setf (cdr m) newm)))
        ((= (power (car (nextm m))) (power monomial)) ; add coefficients
         (setf (coefficient (car (nextm m)))
               (funcall op (coefficient (car (nextm m)))
                        (coefficient monomial)))
         ;; remove zeros
         (when (zerop (coefficient (car (nextm m))))
           (setf (cdr m) (cdr (nextm m)))))))))

;; polynomial addition and subtraction have the same algorithm but using a
;; different operator
(defun 2arg+- (poly1 poly2 operator)
  (with-polys poly1 poly2
    (cond
      ;; different variables
      ((var-higher-rank-p variable-name1 variable-name2)
       (copy-into-result-poly result variable-name1 monomials1
         (add-monomial (monomial poly2 0) result operator)
         result))
      ((var-higher-rank-p variable-name2 variable-name1)
       (copy-into-result-poly result variable-name2 monomials2
         (add-monomial (monomial poly1 0) result operator)
         result))
      (t
       ;; same variable
       (copy-into-result-poly result variable-name1 monomials1
         (dolist (m (cdr monomials2))
           (add-monomial m result operator))
         result)))))

;; addition
(defmethod 2arg+ ((obj1 polynomial) (obj2 polynomial))
  (reduce-constant-poly
   (2arg+- obj1 obj2 #'+)))

(defmethod 2arg+ ((obj1 polynomial) (obj2 rational))
  (if (zerop obj2)
      (copy obj1)
      (with-poly obj1
        (copy-into-result-poly result variable-name monomials
          (add-monomial (monomial obj2 0) result #'+)
          (reduce-constant-poly result)))))

(defmethod 2arg+ ((obj1 rational) (obj2 polynomial))
  (2arg+ obj2 obj1))

;; subtraction
(defmethod 2arg- ((obj1 polynomial) (obj2 polynomial))
  (reduce-constant-poly
   (2arg+- obj1 obj2 #'-)))

(defmethod 2arg- ((obj1 polynomial) (obj2 rational))
  (if (zerop obj2)
      (copy obj1)
      (with-poly obj1
        (copy-into-result-poly result variable-name monomials
          (add-monomial (monomial obj2 0) result #'-)
          (reduce-constant-poly result)))))

(defmethod 2arg- ((obj1 rational) (obj2 polynomial))
  (with-poly obj2
    (let ((result (make-zero-poly variable-name)))
      (dolist (m (nextm monomials))
        (add-monomial m result #'-))
      (+ result obj1))))

;; multiplication

;; multiplying a polynomial by a constant, for multiplication with a rational
;; or a polynomial in another variable
(defun multiply-poly-by-constant (polynomial constant)
  (cond
    ((zerop constant)
     0)
    (t
     (with-poly polynomial
       (let ((result (make-zero-poly variable-name)))
         (dolist (m (nextm monomials))
           (add-monomial (monomial (* (coefficient m) constant)  (power m))
                         result #'+))
         (reduce-constant-poly result))))))

(defmethod 2arg* ((obj1 polynomial) (obj2 polynomial))
  (with-polys obj1 obj2
    (cond
      ;; different variables
      ((var-higher-rank-p variable-name1 variable-name2)
       (multiply-poly-by-constant obj1 obj2))
      ((var-higher-rank-p variable-name2 variable-name1)
       (multiply-poly-by-constant obj2 obj1))
      (t
       ;; same variables
       (let ((result (make-zero-poly variable-name1)))
         (dolist (m1 (nextm monomials1))
           (dolist (m2 (nextm monomials2))
             (add-monomial (monomial (* (coefficient m1) (coefficient m2))
                                     (+ (power m1) (power m2)))
                           result #'+)))
         (reduce-constant-poly result))))))

(defmethod 2arg* ((obj1 polynomial) (obj2 rational))
  (multiply-poly-by-constant obj1 obj2))

(defmethod 2arg* ((obj1 rational) (obj2 polynomial))
  (multiply-poly-by-constant obj2 obj1))

;; division

(defmethod 2arg/ ((obj1 polynomial) (obj2 rational))
  (with-poly obj1
    (let ((result (make-zero-poly variable-name)))
      (dolist (m (nextm monomials))
        (add-monomial (monomial (/ (coefficient m) obj2) (power m))
                      result #'+))
      (reduce-constant-poly result))))

;; polynomial division functions
(defgeneric polynomial-division (dividend divisor)
  (:documentation "Polynomial (Euclidean) division, returns quotient Q and
remainder R such that dividend = Q * divisor + R"))

(defmethod polynomial-division (dividend (divisor rational))
  (values (/ dividend divisor) 0))

(defmethod polynomial-division ((dividend rational) (divisor polynomial))
  (values 0 (copy dividend)))

(defmethod polynomial-division ((dividend polynomial) (divisor polynomial))
  (with-polys dividend divisor
    (cond
      ;; different variables
      ((var-higher-rank-p variable-name1 variable-name2)
       ;; this might divide - divide each coefficeint of the LHS by the RHS,
       ;; if one or more does not divide then the whole thing does not divide
       (let ((Q (copy dividend)))
         (with-poly Q
           (dolist (m (nextm monomials))
             (multiple-value-bind (quotient remainder)
                 (polynomial-division (coefficient m) divisor)
               (cond
                 ((zerop remainder)
                  ;; this one divided
                  (setf (coefficient m) quotient))
                 (t
                  ;; doesn't divide
                  (return-from polynomial-division
                    (values 0 (copy dividend)))))))
           ;; fully divided
           (values Q 0))))
      ((var-higher-rank-p variable-name2 variable-name1)
       ;; this definitely does not divide
       (values 0 (copy dividend)))
      ;; same variable
      (t
       (let* ((Q 0)
              (R (copy dividend))
              (d (- (deg R variable-name1) (deg divisor variable-name1)))
              (S))
         (do ()
             ((or
               (zerop R)
               (< d 0)))
           (setf S (make-mono-poly variable-name1
                                   (/ (lc R variable-name1)
                                      (lc divisor variable-name1)) d))
           (setf Q (+ Q S))
           (setf R (- R (* divisor S)))
           (setf d (- (deg R variable-name1) (deg divisor variable-name1))))
         (values (reduce-constant-poly Q)
                 (reduce-constant-poly R)))))))

(defgeneric polynomial-pseudo-division (dividend divisor)
  (:documentation "Pseudo-divide."))

(defmethod polynomial-pseudo-division
    ((dividend rational) (divisor rational))
  ;; Q=A, R=0
  (values (copy divisor) 0))

(defmethod polynomial-pseudo-division ((dividend polynomial) (divisor rational))
  ;; Q=A*B^deg(A), R=0
  (values
   (* dividend (^ divisor (deg dividend (slot-value dividend 'variable-name))))
   0))

(defmethod polynomial-pseudo-division ((dividend rational) (divisor polynomial))
  (values 0 (copy dividend)))

(defmethod polynomial-pseudo-division ((dividend polynomial) (divisor polynomial))
  "Polynomial pseudo division, returns pseudo-quotient Q and pseudo-remainder
R such that t * dividend = Q * divisor + R"
  (with-polys dividend divisor
    (cond
      ((zerop dividend)
       (values 0 0))
      ;; different variables
      ((var-higher-rank-p variable-name1 variable-name2)
       ;; Q = A*B^deg(A), R = 0
       (values
        (* dividend (^ divisor (deg dividend variable-name1)))
        0))
      ((var-higher-rank-p variable-name2 variable-name1)
       (values 0 (copy dividend)))
      ;; same varibale
      ((> (deg divisor variable-name1) (deg dividend variable-name2))
       ;; won't divide so Q=0, R=A
       (values 0 (copy dividend)))
      (t
       (let* ((v variable-name1)
              (N (+ (- (deg dividend v) (deg divisor v)) 1))
              (Q 0)
              (R (copy dividend))
              (b (lc divisor v))
              (S))
         (loop with delta = (- (deg R v) (deg divisor v))
            while (and (not (zerop R)) (>= delta 0)) do
              (setf S (* (lc R v) (make-mono-poly v 1 delta)))
              (setf N (1- N))
              (setf Q (+ (* b Q) S))
              (setf R (- (* b R) (* S divisor)))
              (setf delta (- (deg R v) (deg divisor v))))
         (values (* (^ b N) Q) (* (^ b N) R)))))))

(defun polynomial-exact-division (dividend divisor)
  "Polynomial exact division for polynomials which divide exactly, an error is
raised if the division was not exact."
  (multiple-value-bind (quotient remainder)
      (polynomial-division dividend divisor)
    (when (not (zerop remainder))
      (error "Exact division was not exact!"))
    quotient))

;;; general polynomial functions

;; predicates

(defmethod zerop ((object polynomial))
  (with-poly object
    (if (endp (cdr monomials))
        t
        nil)))

(defmethod minusp ((object polynomial))
  (if (zerop object)
      nil
      (minusp (lc object (slot-value object 'variable-name)))))

(defmethod plusp ((object polynomial))
  (if (zerop object)
      nil
      (minusp (lc object (slot-value object 'variable-name)))))

;; leading coefficient
(defgeneric lc (obj variable)
  (:documentation "Get the leading coefficient of an object."))

(defmethod lc ((obj rational) variable)
  obj)

(defmethod lc ((obj polynomial) variable)
  (if (zerop obj)
      0
      (with-poly obj
        (cond
          ((eq variable-name variable)
           (coefficient (car (nextm monomials))))
          ((var-higher-rank-p variable variable-name)
           obj)
          (t
           (error "Trying to get LC in lower ranking variable."))))))

;; degree
(defgeneric deg (obj variable)
  (:documentation "Get the degree of an object in the given variable."))

(defmethod deg ((obj rational) variable)
  0)

(defmethod deg ((obj polynomial) variable)
  (if (zerop obj)
      0
      (with-poly obj
        (cond
          ((eq variable-name variable)
           (power (car (nextm monomials))))
          ((var-higher-rank-p variable variable-name)
           0)
          (t
           (error "Trying to get deg in lower ranking variable."))))))

;; reduces a constant polynomial to polynomial in lower variable or rational
;; number
(defgeneric reduce-constant-poly (polynomial)
  (:documentation "Reducesa polynomial which is constant in its
  variable (ie. only has a nonzero coefficient for var^0) to the lower
  form (ie. lower variable, or rational number)"))

(defmethod reduce-constant-poly ((polynomial rational))
  polynomial)

(defmethod reduce-constant-poly ((polynomial polynomial))
  (with-poly polynomial
    (if (= (deg polynomial variable-name) 0)
        (if (zerop polynomial)
            0
            (coefficient (car (nextm monomials))))
        polynomial)))

;; gets the "rational part" of the polynomial ie. the lcm of the coefficients
;; of a multivariate polynomial
(defgeneric poly-rat-part (polynomial))

(defmethod poly-rat-part ((polynomial rational))
  (denominator polynomial))

(defmethod poly-rat-part ((polynomial polynomial))
  (with-poly polynomial
    (cond
      ((zerop polynomial)
       1)
      (t
       (apply 'lcm
              (mapcar (lambda (x) (poly-rat-part (coefficient x)))
                      (cdr monomials)))))))

(defgeneric content (polynomial variable)
  (:documentation "Returns the content of the polynomial, which is the gcd of
  its coefficients."))

(defmethod content ((polynomial rational) variable)
  polynomial)

(defmethod content ((polynomial polynomial) variable)
  (with-poly polynomial
    (cond
      ((zerop polynomial)
       (return-from content 0))
      ;; different variables
      ((var-higher-rank-p variable variable-name)
       (copy polynomial))
      ((var-higher-rank-p variable-name variable)
       (error "Trying to get content in higher ranking variable!"))
      (t
       (let ((cont
              (apply 'gcd (mapcar #'coefficient (cdr monomials)))))
         (if (minusp polynomial)
             (- cont)
             cont))))))

(defgeneric pp (polynomial variable)
  (:documentation "Returns the primitive part of the polynomial, which is the
  polynomial divided by its content."))

;; cont(0) = pp(0) = 0
(defmethod pp ((polynomial rational) variable)
  (if (zerop polynomial)
      0
      1))

(defmethod pp ((polynomial polynomial) variable)
  (polynomial-exact-division polynomial (content polynomial variable)))

;; utility functions for getting at the individual results of pseudo-division
(defun pquo (dividend divisor)
  "Returns the pseudo-quotient of the dividend and divisor."
  (car (multiple-value-list (polynomial-pseudo-division dividend divisor))))

(defun prem (dividend divisor)
  "Returns the pseudo-quotient of the dividend and divisor."
  (cadr (multiple-value-list (polynomial-pseudo-division dividend divisor))))

(defmethod numerator ((object polynomial))
  object)

(defmethod denominator ((object polynomial))
  1)
