(in-package :cl-user)

(defpackage :gk-integrator
  (:use :common-lisp :cl-ppcre :dso-lex :yacc)
  (:shadow :+ :- :* :/ :zerop))
