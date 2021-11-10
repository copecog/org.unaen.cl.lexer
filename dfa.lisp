;;;; org.unaen.cl.lexer/dfa.lisp

(in-package #:org.unaen.cl.lexer)

(defmethod ε-closure ((FA-state FA-state) (NFA NFA))
  (with-FA-slots NFA
    (transit-char-closure (sets:set FA-state)
			  (sets:set)
			  Δ
			  'ε)))

(deftype transit-char () `(or character (eql ε)))

(defun transit-char-closure (FA-states-prev FA-states-next Δ transit-char)
  "Starting with and including FA-STATES-PREV, add all states reachable by TRANSIT-CHAR via map data structure Δ, to (=>) FA-STATES-NEXT."
  (declare (type sets:set FA-states-prev FA-states-next)
	   (type maps:map Δ)
	   (type transit-char transit-char))
  (sets:set-do-elements (state-prev FA-states-prev FA-states-next); => FA-states-next
    (let ((element (sets:set-get-element state-prev FA-states-next)))
      (when (sets:set-default-element element)
	(sets:set-add-element state-prev FA-states-next)
	(let ((state-prev->states (maps:map-get `(,state-prev ,transit-char) Δ)))
	  (when (sets:setp state-prev->states)
	    (transit-char-closure state-prev->states FA-states-next Δ transit-char)))))))

(defmethod final-state-p ((FA-state FA-state) (FA-system FA-system))
  (with-FA-system-slots FA-system
      (final-state-p fa-state FA)))

(defmethod final-state-p :before ((FA-state FA-state) (FA FA))
  "Method just to catch errors if I have cross-wired states while transforming FA's."
  (with-FA-slots FA
    (unless (sets:set-member-p FA-state Q)
      (error "FA-STATE not a member of set Q in FA."))))

(defmethod final-state-p ((FA-state FA-state) (FA FA))
  (with-FA-slots FA
    (sets:set-member-p FA-state F)))

(defmethod find-closure ((FA-states sets:set) (Q-maps maps:map))
  (maps:map-find-element 

  
#|
(defmethod NFA->DFA ((NFA-system FA-system))
  (with-FA-system-dot-slots NFA-system
    (let ((DFA-system (FA-system 'DFA
				 :reglex NFA-system.reglex
				 :FA-system-prev NFA-system)))
      (with-FA-system-dot-slots DFA-system
	(with-FA-dot-slots NFA-system
	  (with-FA-dot-slots DFA-system
	    (setf DFA-system.FA.q0
		  (ε-closure NFA-system.FA.q0 NFA-system.FA))))))))
|#

