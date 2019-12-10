;;;; lexer.lisp

(uiop:define-package #:org.unaen.cl.lexer
  (:use #:common-lisp
        #:alexandria
        #:org.unaen.cl.util
        #:org.unaen.cl.simple-set)
  (:use-reexport #:org.unaen.cl.lexer/with-fa-slots
                 #:org.unaen.cl.lexer/fa-classes))

(in-package #:org.unaen.cl.lexer)
