;;;; package.lisp

(defpackage #:lexer
  (:use #:cl)
  (:shadowing-import-from #:alexandria
			  #:mappend
			  #:set-equal)
  (:shadowing-import-from #:closer-mop
			  #:compute-slots
			  #:slot-definition-name))

