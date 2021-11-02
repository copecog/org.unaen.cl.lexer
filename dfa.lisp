;;;; org.unaen.cl.lexer/dfa.lisp

(in-package #:org.unaen.cl.lexer)

(defmethod ε-closure ((fa-state fa-state) (nfa nfa))
  (transit-char-closure (sets:set fa-state)
			(sets:set)
			(slot-value nfa 'Δ)
			'ε))

(defun transit-char-closure (fa-states-prev fa-states-next Δ transit-char)
  (declare (type sets:set fa-states-prev
		          fa-states-next)
	   (type maps:map Δ)
	   (type character transit-char))
  (sets:set-do-elements (state-prev fa-states-prev)
    (unless (sets:set-get-element state-prev fa-states-next) 
      (sets:set-add-element state-prev fa-states-next)
      (transit-char-closure (maps:map-get `(,state-prev ,transit-char) Δ)
			    fa-states-next
			    Δ
			    transit-char)))
  fa-states-next)

