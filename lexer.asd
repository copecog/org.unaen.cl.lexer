;;;; lexer.asd

(asdf:defsystem #:lexer
  :description "A somewhat literal implementation of lexing components: NFA's and DFA's"
  :author "Christopher H Cope <copecd@gmail.com>"
  :license "BSD"
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

