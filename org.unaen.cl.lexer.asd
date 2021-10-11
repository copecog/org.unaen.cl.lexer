;;;; org.unaen.cl.lexer.asd

(asdf:defsystem #:org.unaen.cl.lexer
  :description "A somewhat literal implementation of lexing components: NFA's and DFA's"
  :author "Christopher H Cope <christopher.h.cope@gmail.com>"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
	       #:org.unaen.cl.util
	       #:org.unaen.cl.sets
	       #:org.unaen.cl.maps)
  :components ((:file "package")
               (:file "classes")
	       (:file "util")
	       (:file "fa")
	       (:file "nfa")
	       (:file "dfa")
	       (:file "compiler")
	       (:file "lexer")))
