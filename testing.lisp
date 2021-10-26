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
                    (plus (inter #\0 #\9)))))
  "Reglex for a C language floating point number, equivalent to regex:  [+-]?((([0-9]+.[0-9]∗ | .[0-9]+)([eE][+-]?[0-9]+)?) | [0-9]+[eE][+-]?[0-9]+)")

(defparameter *reglex.test.lit*
  '(lit #\a #\b #\c))

(defparameter *reglex.test.or*
  '(or (lit #\a #\b #\c)
       (lit #\1 #\2 #\3)))

(defparameter *reglex.test.conc*
  '(conc (lit #\a #\b #\c)
         (lit #\1 #\2 #\3)))

(defmethod push-reglex-test ((reglex-test list))
  (let ((fa-system (fa-system 'nfa)))
    (with-slots ((reglex reglex) (fa fa) (state-kernel state-kernel)) fa-system
      (with-slots ((q₀ q₀) (F F)) fa
	(values fa-system
		(setf q₀ (make-state state-kernel))
		(sets:set-add-element (push-reglex (setf reglex reglex-test)
						   q₀
						   fa-system)
				      F))))))
