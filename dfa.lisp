;;;; org.unaen.cl.lexer/dfa.lisp

(in-package #:org.unaen.cl.lexer)

(defmethod ε-closure ((fa-state fa-state) (nfa nfa))
  (transit-char-closure (sets:set fa-state)
			(sets:set)
			(slot-value nfa 'Δ)
			'ε))

(deftype transit-char () `(or character (eql ε)))

(defun transit-char-closure (fa-states-prev fa-states-next Δ transit-char)
  "Starting with and including FA-STATES-PREV, add all states reachable by TRANSIT-CHAR via map data structure Δ, to (=>) FA-STATES-NEXT."
  (declare (type sets:set fa-states-prev fa-states-next)
	   (type maps:map Δ)
	   (type transit-char transit-char))
  (sets:set-do-elements (state-prev fa-states-prev fa-states-next); => fa-states-next
    (let ((element (sets:set-get-element state-prev fa-states-next)))
      (when (null element)
	(sets:set-add-element state-prev fa-states-next)
	(let ((state-prev->states (maps:map-get `(,state-prev ,transit-char) Δ)))
	  (when (sets:setp state-prev->states)
	    (transit-char-closure state-prev->states fa-states-next Δ transit-char)))))))
#|
(defmethod nfa->dfa ((nfa-system fa-system))
  (let ((dfa-system (fa-system 'dfa :reglex (reglex nfa-system) :fa-system-prev nfa-system)))
    (setf (q0 (fa dfa-system))
	  (ε-closure (q0 (fa nfa-system)) (fa nfa-system)))))

|#
