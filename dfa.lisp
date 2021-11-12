;;;; org.unaen.cl.lexer/dfa.lisp

(in-package #:org.unaen.cl.lexer)

(declaim (optimize (debug 3)))

(defmethod ε-closure ((FA-state FA-state) (NFA.Δ maps:map))
  (%ε-closure (sets:set FA-state) (sets:set) NFA.Δ))

(defmethod ε-closure ((FA-states sets:set) (NFA.Δ maps:map))
  (%ε-closure FA-states (sets:set) NFA.Δ))

(deftype transit-char () `(or character (eql ε)))

(defun %ε-closure (FA-states-prev FA-states-next Δ)
  "Starting with and including FA-STATES-PREV, recursively add all states reachable by TRANSIT-CHAR via map data structure Δ, to => FA-STATES-NEXT."
  (declare (type sets:set FA-states-prev FA-states-next) (type maps:map Δ))
  (sets:set-do-elements (state-prev FA-states-prev FA-states-next); => FA-states-next
    (unless (sets:set-member-p state-prev FA-states-next)
      (sets:set-add-element state-prev FA-states-next)
      (let ((state-prev->states (maps:map-get `(,state-prev ε) Δ)))
	(when (sets:setp state-prev->states)
	  (%ε-closure state-prev->states FA-states-next Δ))))))

#|
(defmethod ε-closure ((fa-state fa-state) (nfa nfa))
  (transit-char-closure (sets:set fa-state)
			(sets:set)
			(slot-value nfa 'Δ)
			'ε))

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
|#

(defgeneric final-state-p (FA-state/s F/FA/system)
  (:method (FA-state/s (FA-system FA-system))
    (with-FA-system-slots FA-system
      (final-state-p FA-state/s FA)))
  (:method (FA-state/s (FA FA))
    (with-FA-slots FA
      (final-state-p FA-state/s F)))
  (:method ((FA-states sets:set) (F sets:set))
    (sets:set-do-elements (final-state F)
      (when (final-state-p final-state FA-states)
	(return-from final-state-p t))))
  (:method ((FA-state FA-state) (F sets:set))
    (sets:set-member-p FA-state F)))

(defun find-closure (FA-states Q-maps)
  "Find out if previous equivalent set has already had a closure, => new|old FA-states and DFA-state|nil."
  (declare (type sets:set FA-states) (type maps:map Q-maps))
  (multiple-value-bind (FA-states-found Q-map-input-list)
      (maps:map-find-element FA-states Q-maps :test #'sets:set-equal)
    (if FA-states-found
	(values FA-states-found  (first Q-map-input-list))
	(values FA-states        nil )))); No previous mapped DFA state.

(defun add-final-state (FA-state F)
  "Add FA-state to set of final states F, => FA-state."
  (declare (type FA-state FA-state) (type sets:set F))
  (sets:set-add-element FA-state F))

(defun check/add-final-state (NFA-states NFA.F DFA-state DFA.F)
  "Check if set of NFA-states contains a final state, and if so add DFA-state to DFA final states, => DFA-state."
  (declare (type sets:set NFA-states NFA.F DFA.F) (type FA-state DFA-state))
  (if (final-state-p NFA-states NFA.F)
      (add-final-state DFA-state DFA.F)
      DFA-state))

(defun add-Q-map (DFA-state NFA-states Q-maps)
  "Add mapping of DFA-state to set of NFA-states in respective Q-maps, => DFA-state."
  (declare (type FA-state DFA-state) (type sets:set NFA-states) (type maps:map Q-maps))
  (when (maps:map-add NFA-states DFA-state Q-maps)
    DFA-state))

(deftype FA-state/s () `(or sets:set fa-state))

(defun NFA-state->DFA-state (NFA-state/s NFA.Δ NFA.F DFA.F DFA-system.state-kernel DFA-system.Q-maps)
  "Perform ε-closure on an NFA state, check if mapped to a DFA state in Q-maps, and also if has a final NFA state, => new|old DFA-state."
  (declare (type fa-state/s NFA-state/s) (type maps:map NFA.Δ DFA-system.Q-maps) (type sets:set NFA.F DFA.F) (type FA-state-kernel DFA-system.state-kernel))
  (multiple-value-bind (NFA-states-closure DFA-state) 
      (find-closure (ε-closure NFA-state/s NFA.Δ) DFA-system.Q-maps)
    (values (if DFA-state
		DFA-state
		(check/add-final-state NFA-states-closure
				       NFA.F
				       (add-Q-map (make-state DFA-system.state-kernel)
						  NFA-states-closure
						  DFA-system.Q-maps)
				       DFA.F))
	    NFA-states-closure)))

(defun get-transition-states (FA-states transit-char NFA.Δ)
  (declare (type sets:set FA-states) (type transit-char transit-char) (type maps:map NFA.Δ))
  (let ((FA-transit-states (sets:set)))
    (sets:set-do-elements (FA-state FA-states FA-transit-states)
      (sets:set-add-set-elements (maps:map-get `(,FA-state ,transit-char) NFA.Δ) FA-transit-states))))

(defun %NFA-states->DFA-states (NFA-state/s NFA-system DFA-system DFA-state-prev transit-char-prev)
  "Recursive function: Starting at NFA-state, perform ε-closure and then for each set reachable through a transition symbol in Σ in NFA, perform ε-closure repeating, => DFA start state."
  (declare (type FA-state/s NFA-state/s) (type FA-system NFA-system DFA-system) (type (or (eql nil) FA-state) DFA-state-prev) (type (or (eql nil) transit-char) transit-char-prev))
  (with-NFA-system-dot-slots NFA-system
    (with-DFA-system-dot-slots DFA-system
      (multiple-value-bind (DFA-state-new NFA-states-closure)
	  (NFA-state->DFA-state NFA-state/s NFA.Δ NFA.F	DFA.F DFA-system.state-kernel DFA-system.Q-maps)
	(when (and DFA-state-prev transit-char-prev)
	  (make-transition transit-char-prev DFA-state-prev DFA-state-new DFA))
	(sets:set-do-elements (transit-char NFA.Σ DFA-state-new); => first DFA-state-new
	  (let ((transition-states (get-transition-states NFA-states-closure transit-char NFA.Δ)))
	    (when (not (sets:empty-set-p transition-states))
	      (%NFA-states->DFA-states transition-states NFA-system DFA-system DFA-state-new transit-char))))))))

(defmethod NFA->DFA ((NFA-system FA-system))
  (with-NFA-system-dot-slots NFA-system
    (let ((DFA-system (FA-system 'DFA :reglex NFA-system.reglex :FA-system-prev NFA-system)))
      (with-DFA-system-dot-slots DFA-system
	(setf DFA.q0
	      (%NFA-states->DFA-states NFA.q0 NFA-system DFA-system nil nil)))
      DFA-system)))

