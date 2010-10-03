(in-package :gk-integrator)

;;;; subresultant algorithm

(defun subresultant (a b var)
  (let ((Rs (make-array 5 :fill-pointer 0 :adjustable t))
        (R 0)
        (Ri 0)
        (i 1)
        (gamma -1)
        (delta (- (deg a var) (deg b var)))
        (betas (make-array 5 :fill-pointer 1 :adjustable t)))
    (vector-push-extend (copy a) Rs)
    (vector-push-extend (copy b) Rs)
    (vector-push-extend (^ -1 (+ delta 1)) betas)
    (loop while (not (zerop (elt Rs i))) do
         (setf Ri (lc (elt Rs i) var))
         (setf R (prem (elt Rs (1- i)) (elt Rs i)))
         (vector-push-extend (/ R (elt betas i)) Rs)
         (incf i)
         (setf gamma (* (^ (- Ri) delta)
                        (^ gamma (- 1 delta))))
         (setf delta (- (deg (elt Rs (1- i)) var) (deg (elt Rs i) var)))
         (vector-push-extend (* (- Ri) (^ gamma delta)) betas))
    
    (let ((k (1- i)))
      (cond
        ((> (deg (elt Rs k) var) 0)
         (values 0 Rs))
        
        ((= (deg (elt Rs (1- k)) var) 1)
         (values (copy (elt Rs k)) Rs))
        
        (t
         (let ((s 1)
               (c 1))
           (loop for j from 1 upto (1- k) do
                (when (and
                       (oddp (deg (elt Rs (1- j)) var))
                       (oddp (deg (elt Rs j) var)))
                  (setf s (- s)))
                (setf Ri (lc (elt Rs j) var))
                (setf delta (- (deg (elt Rs (1- j)) var) (deg (elt Rs j) var)))
                (setf c (* c
                           (^ (/ (elt betas j) (^ Ri (+ 1 delta)))
                              (deg (elt Rs j) var))
                           (^ Ri (- (deg (elt Rs (- j 1)) var)
                                    (deg (elt Rs (+ j 1)) var))))))
           (values (* s c (^ (elt Rs k) (deg (elt Rs (- k 1)) var)))
                   Rs)))))))
