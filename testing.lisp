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

(defmethod test-push-reglex ((lit (eql 'lit)))
  (let ((fa-system (fa-system 'nfa))
	(reglex-lit `(lit #\a #\b #\c)))
    (with-slots ((reglex reglex) (fa fa) (state-kernel state-kernel)) fa-system
      (with-slots ((q₀ q₀) (F F)) fa
	(push-reglex (setf reglex reglex-lit)
		     (setf q₀ (make-state state-kernel))
		     (sets:set-add-element (make-state state-kernel) F)
		     fa-system)
	fa-system))))

(defmethod test-push-reglex ((or (eql 'or)))
  (let ((fa-system (fa-system 'nfa))
	(reglex-lit `(or (lit #\a #\b #\c) (lit #\1 #\2 #\3))))
    (with-slots ((reglex reglex) (fa fa) (state-kernel state-kernel)) fa-system
      (with-slots ((q₀ q₀) (F F)) fa
	(push-reglex (setf reglex reglex-lit)
		     (setf q₀ (make-state state-kernel))
		     (sets:set-add-element (make-state state-kernel) F)
		     fa-system)
	fa-system))))

