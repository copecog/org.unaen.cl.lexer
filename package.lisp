;;;; package.lisp

(uiop:define-package #:org.unaen.cl.lexer
  (:use #:cl)
  (:use-reexport #:org.unaen.cl.lexer/with-fa-slots
                 #:org.unaen.cl.lexer/fa-classes
                 #:org.unaen.cl.lexer/lexer))

(in-package #:org.unaen.cl.lexer)


