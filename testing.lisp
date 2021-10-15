;;;; org.unaen.cl.lexer/testing.lisp

(in-package #:org.unaen.cl.lexer)

(defparameter *reglex.lang.c.float*
  '(conc (opt (lit #\+ #\-))
         (or (conc (or (conc (plus (inter #\0 #\9))
                             (lit #\.)
                             (star (inter #\0 #\9)))
	               (conc (lit #\.)
	                     (plus (inter #\0 #\9))))
		   (opt (conc (lit #\e #\E)
		              (opt (lit #\+ #\-))
		              (plus (inter #\0 #\9)))))
	      (conc (plus (inter #\0 #\9))
	            (lit #\e #\E)
	            (opt (lit #\+ #\-))
                    (plus (inter #\0 #\9))))))
  "Reglex for a C language floating point number, equivalent to regex:  [+-]?((([0-9]+.[0-9]∗ | .[0-9]+)([eE][+-]?[0-9]+)?) | [0-9]+[eE][+-]?[0-9]+)")

