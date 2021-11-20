;;;; org.unaen.cl.lexer/nfa.lisp

(in-package #:org.unaen.cl.lexer)

(defun FA-system (FA-type &key reglex FA-system-prev)
  "Return an FA class of object of type FA, NFA, or DFA (or subclass)."
  (flet ((make-FA-system (FA-type reglex FA-system-prev)
	   (let* ((Q            (sets:set))
		  (state-kernel (make-instance 'FA-state-kernel :states Q))
		  (FA           (make-instance FA-type :Q Q)))
	     (make-instance 'FA-system :reglex reglex :FA FA :FA-system-prev FA-system-prev :state-kernel state-kernel))))
    (cond ((and (equal 'NFA FA-type) reglex)
	   (let ((NFA-system (make-FA-system 'NFA reglex nil)))
	     (with-FA-system-dot-slots NFA-system
	       (with-FA-dot-slots NFA-system.FA
		 (sets:set-add-element (push-reglex reglex (setf NFA-system.FA.q₀ (make-state NFA-system.state-kernel)) NFA-system) NFA-system.FA.F)))
	     NFA-system))
	  ((and (equal 'DFA FA-type) FA-system-prev)
	   (make-FA-system 'DFA (reglex FA-system-prev) FA-system-prev))
	  ((subtypep FA-type 'FA)
	   (make-FA-system FA-type nil nil))
	  (t
	   (error "FA-type must be a class or subclass of FA")))))

(defgeneric make-state (FA-state-kernel))

(defmethod make-state ((state-kernel FA-state-kernel))
  (with-slots ((states states) (iterate iterate)) state-kernel
    (sets:set-add-element (make-instance 'FA-state :enum (incf iterate) :kernel state-kernel)
			  states)))

(deftype transit-symbol () `(or character (eql epsilon)))

(defgeneric make-transition (transit-symbol FA-state-previous FA-state-next FA)
  (:documentation "Add mapping of state-prev ✕ transit-symbol → {state-next, ...} ∊ P(Q)."))

(defmethod make-transition :before (transit-symbol (state-prev FA-state) (state-next FA-state) (FA FA))
  (with-fa-slots FA
    (unless (and (sets:set-member-p state-prev Q)
		 (sets:set-member-p state-next Q))
      (error "STATE-PREV or STATE-NEXT not a member of set Q in FA."))))

;; Instead of starting with a set Σ, we are adding the actual '(type character) or 'ε from the reglex.
(flet ((make-transition (transit-symbol state-prev state-next FA)
	 (with-FA-slots FA
	   (let* ((transit-symbol (sets:set-add-element transit-symbol Σ))
		  (map-in         `(,state-prev ,transit-symbol))
		  (states         (maps:map-get map-in Δ)))
	     (etypecase states ;If set already exists add the element, otherwise create a set with the element.
	       (sets:set (sets:set-add-element state-next states)); => state-next
	       (null     (when (maps:map-add (sets:set state-next)
					     map-in
					     Δ)
			   state-next)))))))
  
  (defmethod make-transition ((transit-symbol character) (state-prev FA-state) (state-next FA-state) (FA FA))
    (make-transition transit-symbol state-prev state-next FA))
  
  ;; Internally, the symbol EPSILON is used instead of ε, because it caused problems with CLOS dispatch.
  (defmethod make-transition ((ε (eql 'epsilon)) (state-prev FA-state) (state-next FA-state) (FA FA))
    (make-transition ε state-prev state-next FA)))

(labels ((get-set-transitions (transit-symbol FA-states-prev NFA.Δ)
	   (let ((FA-transit-states (sets:set)))
	     (sets:set-do-elements (FA-state-prev FA-states-prev FA-transit-states)
	       (sets:set-add-set-elements (get-transition transit-symbol FA-state-prev NFA.Δ) FA-transit-states))))
	 (get-transition (transit-symbol FA-state NFA.Δ)
	   (declare (type FA-state FA-state))
	   (maps:map-get `(,FA-state ,transit-symbol) NFA.Δ)))
  
  (defmethod get-transition ((ε (eql 'epsilon)) (FA-states-prev sets:set) (NFA.Δ maps:map))
    (get-set-transitions ε FA-states-prev NFA.Δ))

  (defmethod get-transition ((transit-symbol character) (FA-states-prev sets:set) (FA.Δ maps:map))
    (get-set-transitions transit-symbol FA-states-prev FA.Δ))

  (defmethod get-transition ((ε (eql 'epsilon)) (FA-state-prev FA-state) (NFA.Δ maps:map))
    (get-transition ε FA-state-prev NFA.Δ))

  (defmethod get-transition ((transit-symbol character) (FA-state-prev FA-state) (FA.Δ maps:map))
    (get-transition transit-symbol FA-state-prev FA.Δ)))

(defmethod get-transition (transit-symbol (FA-state-prev FA-state) (FA FA))
  (with-FA-dot-slots FA
    (get-transition transit-symbol FA-state-prev FA.Δ)))

(defmethod get-transition-p (transit-symbol (state-prev FA-state) (state-next FA-state) (FA FA))
  (with-FA-dot-slots FA
    (get-transition-p transit-symbol state-prev state-next FA.Δ)))

(defmethod get-transition-p (transit-symbol (state-prev FA-state) (state-next FA-state) (FA.Δ maps:map))
  (let ((transitions (get-transition transit-symbol state-prev FA.Δ)))
    (typecase transitions
      (sets:set (sets:set-member-p state-next transitions))
      (null     nil))))

#|
Some notes while thinking about what I need to implement for PUSH-REGLEX.

A relatively strict progression of operators from regular expressions:
NAME            OPERATOR      EXPRESSION     NOTES
  literal         lit           "a"            The set consisting of the one-letter string {"a"}. 
                                                 (lit #\a)
  epsilon         ε             ε              The set containing the empty string {""}.
  concatenate     conc          st             Strings constructed by concatenating a string from the language 
                                                 of s with a string from the language of t.
  or              or            s|t            Strings from both languages.
  brackets*       ors           [stvu]         (ors s t v u) => (or s (or t (or v u)))
  literals*       lits          [abcd]         (lits #\a #\b #\c #\d) => (ors (lit #\a) (lit #\b) (lit #\c) (lit #\d))
                                                 => (or (lit #\a) (or (lit #\b) (or (lit #\c) (lit #\d))))
  interval        inter         [a-c]          The set of all letters in the respective interval: 
                                                 (inter #\a #\c) => (lits #\a #\b #\c)
  intervals*      inters        [a-c1-3]       Multiple intervals:
                                                 (inters #\a #\c 1 3) => (ors (lits #\a #\b #\c) (lits #\1 #\2 #\3))
  star            star          s*             Each string in the language of s is a concatenation of ANY number 
                                                 of strings in the language of s.
  plus            plus          s+             Each string in the language of s is a concatenation of ONE or 
                                                 more strings in the language of s.
  optional        opt           s?             A string in the language of s can occur ZERO or ONE time.

*However, it makes more sense to implement inline ors with or, lits with lit, and inters with inter. It will 
also reduce the numbers of states and transitions right off (even though they will be minimized in conversion
to a DFA anyways.
|#

;;; Give a reglex list expression, a start state, and an FA-system class instance, and it will push the respective 
;;;   Finite Automata - as represented by the reglex list expression - onto the FA class instance, generating states
;;;   as necessary from the state-kernel, and then will return the exit (final) state.
(defgeneric push-reglex (reglex-list FA-state-previous FA-system &rest reglex-op-args)
  (:documentation "Accept a list/tree composed of (a) lisp'ified regular expression(s) and recursively evaluate them into an FA."))

(defmethod push-reglex ((reglex list) (state-prev FA-state) (FA-system FA-system) &rest reglex-op-args)
  "Decompose list into an operator and its operands."
  (declare (ignore reglex-op-args))
  (let ((operator (first reglex))
        (operands (rest reglex)))
    (apply #'push-reglex operator state-prev FA-system operands))); => Final state of reglex expression.

(defmethod push-reglex :before ((ε (eql 'ε)) (state-prev FA-state) (FA-system FA-system) &rest ε-args)
  (when ε-args (error "ε operator does not accept arguments.")))

(defmethod push-reglex ((ε (eql 'ε)) (state-prev FA-state) (FA-system FA-system) &rest ε-args)
  "The empty string or epsilon transition: \"\" <=> {\"\"} <=> (ε) <=> (epsilon)."
  (declare (ignore ε-args))
  (with-FA-system-slots FA-system
    (let ((state-next (make-state state-kernel)))
      (make-transition 'epsilon state-prev state-next FA)))); => state-next

(defmethod push-reglex :before ((lit (eql 'lit)) (state-prev FA-state) (FA-system FA-system) &rest lit-args)
  (unless lit-args (error "LIT operator requires one or more arguments.")))

(defmethod push-reglex ((lit (eql 'lit)) (state-prev FA-state) (FA-system FA-system) &rest lit-args)
  "One or more literal symbols (character): \"ab\" <=> {\"a\", \"b\"} <=> (lit #\a #\b)"
  (with-FA-system-slots FA-system
    (loop :for character :in lit-args
	  :and  state-char = (make-state state-kernel)
	  :with state-next = (make-state state-kernel)
	  :do (make-transition character state-prev state-char FA)
	      (make-transition 'epsilon  state-char state-next FA)
	  :finally (return state-next))))

(defmethod push-reglex :before ((or (eql 'or)) (state-prev FA-state) (FA-system FA-system) &rest or-args)
  (unless or-args (error "OR operator requires one or more arguments.")))

(defmethod push-reglex ((or (eql 'or)) (state-prev FA-state) (FA-system FA-system) &rest or-args)
  "A string in the language s xor in the language t: s|t <=> L(s) ∪ L(t) <=> (or s t)."
  (with-FA-system-slots FA-system
    (loop :for or-arg :in or-args
	  :and  state-or   = (make-state state-kernel)
	  :with state-next = (make-state state-kernel)
	  :do (make-transition 'epsilon state-prev state-or FA)
	      (make-transition 'epsilon
			       (push-reglex or-arg state-or FA-system)
			       state-next
			       FA)
	  :finally (return state-next))))

(defmethod push-reglex :before ((conc (eql 'conc)) (state-prev FA-state) (FA-system FA-system) &rest conc-args)
  (unless conc-args (error "CONC operator requires one or more arguments.")))

(defmethod push-reglex ((conc (eql 'conc)) (state-prev FA-state) (FA-system FA-system) &rest conc-args)
  "The language defined by concatenating a string from language s with a string from language t: st <=> {mn | m∈L(s), n∈L(t)} <=> (conc s t)."
  ;; Each state-next returned by a (push-reglex conc-arg ..) is the state-prev for the next conc-arg until the final one is just returned.
  (loop :for conc-arg :in conc-args
	:do (setf state-prev
		  (push-reglex conc-arg state-prev FA-system))
	:finally (return state-prev)))

(defmethod push-reglex :before ((star (eql 'star)) (state-prev FA-state) (FA-system FA-system) &rest star-arg)
  (unless (and star-arg (not (cdr star-arg)))
    (error "STAR operator requires exactly a single argument.")))

(defmethod push-reglex ((star (eql 'star)) (state-prev FA-state) (FA-system FA-system) &rest star-arg)
  "A string that is a concatenation of zero or more strings in the language s: s* <=> {\“\”} ∪ {vw | v∈L(s), w∈L(s∗)} <=> (star s)."
  (with-FA-system-slots FA-system
    (make-transition 'epsilon
		     (push-reglex star-arg state-prev FA-system)
		     state-prev
		     FA))); => state-prev

(defmethod push-reglex :before ((plus (eql 'plus)) (state-prev FA-state) (FA-system FA-system) &rest plus-arg)
  (unless (and plus-arg (not (cdr plus-arg)))
    (error "PLUS operator requires exactly a single argument.")))

(defmethod push-reglex ((plus (eql 'plus)) (state-prev FA-state) (FA-system FA-system) &rest plus-arg)
  "A string that is a concatenation of one or more strings in the language s: s+ <=> {xy | x∈L(s), y∈L(s*)} <=> (plus s)."
  (with-FA-system-slots FA-system
    (let ((state-next
	    (push-reglex plus-arg state-prev FA-system)))
      (make-transition 'epsilon state-next state-prev FA)
      state-next)))

(defmethod push-reglex ((inter (eql 'inter)) (state-prev FA-state) (FA-system FA-system) &rest inter-args)
  "Shorthand for or'ing all characters in an interval: [\"0\"-\"9\"] <=> {\"0\",\"1\", ... ,\"9\"} <=> (inter #\0 #\9) <=> (lits #\0 #\1 ... #\9)."
  (let ((lit-args (loop :for (min max) :on inter-args :by (function cddr)
			:appending (util:char-interval->list min max))))
    (apply #'push-reglex 'lit state-prev FA-system lit-args))); => Final state of LIT Reglex.

(defmethod push-reglex :before ((opt (eql 'opt)) (state-prev FA-state) (FA-system FA-system) &rest opt-arg)
  (unless (and opt-arg (not (cdr opt-arg)))
    (error "OPT operator requires exactly a single argument.")))

(defmethod push-reglex ((opt (eql 'opt)) (state-prev FA-state) (FA-system FA-system) &rest opt-arg)
  "An optional symbol or set: \"a\"? <=> {\"a\",\"\"} <=> (opt #\a)"
  (with-FA-system-slots FA-system
    (let ((state-next
	    (push-reglex opt-arg state-prev FA-system)))
      (make-transition 'epsilon state-prev state-next FA)))); => state-next

