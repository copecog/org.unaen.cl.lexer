;;;; org.unaen.cl.lexer.asd

(asdf:defsystem #:org.unaen.cl.lexer
  :description "My first real CL project -- A somewhat literal implementation of lexing components: NFA's and DFA's"
  :author "Christopher H Cope <christopher.h.cope@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (#:alexandria
	       #:org.unaen.cl.util)
  :components ((:file "package")
	       (:file "sets-and-maps")
               (:file "v-reader")
               (:file "with-fa-slots")
               (:file "fa-classes")
               (:file "lexer")))

