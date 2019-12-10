;;;; org.unaen.cl.lexer.asd

(asdf:defsystem #:org.unaen.cl.lexer
  :description "My first real CL project -- A somewhat literal implementation of lexing components: NFA's and DFA's"
  :author "Christopher H Cope <christopher.h.cope@gmail.com>"
  :license "MIT"
  :version "0.0.1"
  :class :package-inferred-system
  :depends-on (#:alexandria
	       #:org.unaen.cl.util
               #:org.unaen.cl.simple-set)
  :components ((:file "package")
               (:file "fa-classes")
               (:file "with-fa-slots")
               (:file "lexer")))
