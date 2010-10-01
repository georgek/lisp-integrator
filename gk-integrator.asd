(defsystem "gk-integrator"
  :description "My integrator program, in LISP."
  :version "0.1"
  :author "Borbus"
  :licence "GNU GPLv3"
  :depends-on ("cl-ppcre" "dso-lex" "yacc")
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "tree")
               (:file "parser")
               (:file "generic")
               (:file "predicates")
               (:file "arithmetic")
               (:file "variables")
               (:file "polynomial")
               (:file "gcd")
               (:file "ratfun")))