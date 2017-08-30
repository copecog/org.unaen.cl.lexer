;;;; lexer.asd

(asdf:defsystem #:lexer
  :description "A somewhat literal implementation of lexing components: NFA's and DFA's"
  :author "Christopher H Cope <copecd@gmail.com>"
  :license "BSD"
  :serial t
  :depends-on (#:alexandria)
  :components ((:file "package")
	       (:file "classes")
	       (:file "fa-atomic-ops")
	       (:file "push-fragment")
               (:file "lexer")))

