;;;; org.unaen.cl.lexer/dfa.lisp

(in-package #:org.unaen.cl.lexer)

;(declaim (optimize (debug 3)))

(defmethod ε-closure ((FA-state FA-state) (NFA.Δ maps:map))
  (%ε-closure (sets:set FA-state) (sets:set) NFA.Δ))

(defmethod ε-closure ((FA-states sets:set) (NFA.Δ maps:map))
  (%ε-closure FA-states (sets:set) NFA.Δ))

(defun %ε-closure (FA-states-prev FA-states-next Δ); => FA-states-next
  (declare (type sets:set FA-states-prev FA-states-next) (type maps:map Δ))
  (let ((FA-states-prev-new (sets:set)))
    (sets:set-do-elements (state-prev FA-states-prev)
      (sets:set-add-set-elements (get-transition 'epsilon
						 (sets:set-add-element state-prev FA-states-next)
						 Δ)
				 FA-states-prev-new))
    (if (sets:empty-set-p FA-states-prev-new)
	FA-states-next
	(%ε-closure FA-states-prev-new FA-states-next Δ))))

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

(defun NFA-states->DFA-states (NFA-state/s NFA-system DFA-system)
  (%NFA-states->DFA-states NFA-state/s NFA-system DFA-system nil nil))

(defun %NFA-states->DFA-states (NFA-state/s NFA-system DFA-system DFA-state-prev transit-symbol-prev)
  "Starting at NFA-state/s, perform ε-closure, check uniqueness and/or create new DFA-state (performed by NFA-state->DFA-state); Then loop over each set reachable through a transition symbol in Σ in NFA, recurse and repeat while forwarding new DFA-state and transition symbol to map between new DFA states; If mapping already exist, end this (circular) recursion branch; Return equivalent => starting DFA-state from starting NFA-state(s)."
  (declare (type FA-state/s NFA-state/s) (type FA-system NFA-system DFA-system) (type (or (eql nil) FA-state) DFA-state-prev) (type (or (eql nil) transit-symbol) transit-symbol-prev))
  (with-NFA-system-dot-slots NFA-system
    (with-DFA-system-dot-slots DFA-system
      (multiple-value-bind (DFA-state-next NFA-states-closure)
	  (NFA-state->DFA-state NFA-state/s NFA.Δ NFA.F	DFA.F DFA-system.state-kernel DFA-system.Q-maps)
	(when (and DFA-state-prev transit-symbol-prev)
	  (if (get-transition-p transit-symbol-prev DFA-state-prev DFA-state-next DFA.Δ)
	      (return-from %NFA-states->DFA-states DFA-state-next); Avoid circular recursion.
	      (make-transition transit-symbol-prev DFA-state-prev DFA-state-next DFA)))
	(sets:set-do-elements (transit-symbol NFA.Σ)
	  (when (not (eq transit-symbol 'epsilon))
	    (let ((transition-states (get-transition transit-symbol NFA-states-closure NFA.Δ)))
	      (when (not (sets:empty-set-p transition-states))
		(%NFA-states->DFA-states transition-states NFA-system DFA-system DFA-state-next transit-symbol)))))
      DFA-state-next))))

(defmethod NFA->DFA ((NFA-system FA-system))
  (with-NFA-system-dot-slots NFA-system
    (let ((DFA-system (FA-system 'DFA :reglex NFA-system.reglex :FA-system-prev NFA-system)))
      (with-DFA-system-dot-slots DFA-system
	(setf DFA.q0 (NFA-states->DFA-states NFA.q0 NFA-system DFA-system)))
      DFA-system)))

