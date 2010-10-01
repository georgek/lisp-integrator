(in-package :gk-integrator)

;;;; variables stuff

;;; variables

;; just use lexical ordering of variable names for now
(defun var-higher-rank-p (var1 var2)
  (string< (symbol-name var1) (symbol-name var2)))
