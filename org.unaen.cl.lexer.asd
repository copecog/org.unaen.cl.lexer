;;;; org.unaen.cl.lexer.asd

(asdf:defsystem #:org.unaen.cl.lexer
  :description "My first real CL project -- A somewhat literal implementation of lexing components: NFA's and DFA's"
  :author "Christopher H Cope <christopher.h.cope@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (#:alexandria
	       #:closer-mop)
  :components ((:file "package")
	       (:file "utilities")
	       (:file "classes")
	       (:file "generics")
	       (:file "fa")
	       (:file "nfa")
	       (:file "dfa")
               (:file "lexer")
	       (:file "testing")))

