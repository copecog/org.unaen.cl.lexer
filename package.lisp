;;;; package.lisp

(defpackage #:lexer
  (:use #:cl)
  (:shadowing-import-from #:alexandria
			  #:mappend
			  #:set-equal))

