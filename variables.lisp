(in-package :gk-integrator)

;;;; variables stuff

;;; variables

(defvar *variable-table* nil
  "The variable table to be used during this process.")

(defparameter *next-variable-rank* 2
  "The rank of the next variable to be inserted into the table.")

;; our special new variable for integration
(defvar *t* '*t*)

(defun var-rank (var &optional (table *variable-table*))
  (gethash var table))

;; just use lexical ordering of variable names for now
;; (defun var-higher-rank-p (var1 var2)
;;   (string< (symbol-name var1) (symbol-name var2)))

(defun var-higher-rank-p (var1 var2 &optional (table *variable-table*))
  (< (gethash var1 table) (gethash var2 table)))

(defun add-var (var &optional (table *variable-table*))
  (when (null (gethash var table))
    (setf (gethash var table) *next-variable-rank*)
    (incf *next-variable-rank*)
    var))

(defun set-main-var (var &optional (table *variable-table*))
  (setf (gethash var table) 0))

