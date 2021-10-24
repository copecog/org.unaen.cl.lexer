;;;; org.unaen.cl.lexer/fa.lisp

(in-package #:org.unaen.cl.lexer)
#|
We define a Finite Automaton as a quintuple (Q,Σ,Δ,q₀,F), where:
 - Q is a finite set of states.
 - Σ (Sigma) is a finite set of input symbols.
 - Δ (Delta) is a transition function Δ: Q ✕ Σ → P(Q).
 - q₀ is an initial (or start) state q₀ ∊ Q.
 - F is the accepting or final states F ⊆ Q.

Given an FA and a sequence of input symbols - then this sequence is in the
(regular) language defined by the FA if there exists a sequence of states
beginning at the start state and connected via transititions on each
subsequent input symbol until ending on an accepting state:
  q₀∊Q, q_1∊Q, ... , q_n∊F⊆Q.

In a nondeterminate finite automaton transistions from state to state are
based on input symbols σ∊Σ OR on an additional special symbol ε (epsilon),
called an epsilon transition, thus creating the non-deterministic nature
of the finite automaton.

It is only sufficient that a sequence of states and transitions exist for
our sequence of input symbols to be accepted as a string in our regular
language defined by the NFA.

Regular expressions themselves can not be defined with a regular language:
This is convenient for us as using an s-expression based notation will
present a type of already parsed form, thus avoiding the need of a parser
or lexer in using our forms to specify a regular language. We are actually
depending on the Reader in Common Lisp to do this for us, and writing this
program is in part for the author to eventually use the reader to full
advantage.
|#
(defun FA-system (FA-type)
  "Return an FA class of object of type FA, NFA, or DFA (or subclass)."
  (if (not (subtypep FA-type 'FA))
      (error "FA-type must be a class or subclass of FA")      
      (let* ((Q            (sets:set))
	     (FA           (make-instance FA-type
					  :Q Q))
	     (state-kernel (make-instance 'FA-state-kernel
					  :states Q)))
	(make-instance 'FA-system
		       :FA FA
		       :state-kernel state-kernel))))

(defgeneric make-state (FA-something))

(defmethod make-state ((FA-system FA-system))
  (with-slots ((state-kernel state-kernel)) FA-system
    (make-state state-kernel)))

(defmethod make-state ((state-kernel FA-state-kernel))
  (with-slots ((states states) (iterate iterate)) state-kernel
    (sets:set-add-element (make-instance 'FA-state
					 :enum (incf iterate)
					 :kernel state-kernel)
			  states)))

(defgeneric make-transition (transit-symbol FA-state-previous FA-state-next FA-something))

(defmethod make-transition ((transit-symbol character) (state-prev FA-state) (state-next FA-state) (FA-system FA-system))
  (with-slots ((FA FA)) FA-system
    (make-transition-2 transit-symbol state-prev state-next FA)))

(defmethod make-transition ((transit-symbol character) (state-prev FA-state) (state-next FA-state) (NFA NFA))
  (make-transition-2 transit-symbol state-prev state-next NFA))

(defmethod make-transition ((ε (eql 'ε)) (state-prev FA-state) (state-next FA-state) (NFA NFA))
  (make-transition-2 'ε state-prev state-next NFA))

;; Instead of starting with a set Σ, we are adding the actual '(type character) or 'ε from the reglex.
(defun make-transition-2 (transit-symbol state-prev state-next FA)
  "Add mapping of state-prev ✕ transit-symbol → {state-next, ...} ∊ P(Q)."
  (with-slots ((Σ Σ) (Δ Δ)) FA
    (let* ((transit-symbol  (sets:set-add-element transit-symbol
						  Σ))
	   (map-in         `(,state-prev ,transit-symbol))
	   (states          (maps:map-get map-in
					  Δ)))
      (etypecase states ;If set add element, otherwise create set with element.
	(sets:set (sets:set-add-element state-next
					states)); =>state-next
	(null     (when (maps:map-add (sets:set state-next)
				      map-in
				      Δ)
		    state-next))))))
#|
A relatively strict progression of operators from regular expressions:
NAME            OPERATOR        EXPRESSION     NOTES
  literal         lit             "a"            The set consisting of the one-letter string {"a"}. 
                                                   (lit #\a)
  epsilon         ε               ε              The set containing the empty string {""}.
  concatenate     conc            st             Strings constructed by concatenating a string from the language 
                                                   of s with a string from the language of t.
  or              or              s|t            Strings from both languages.
  brackets*       ors             [stvu]         (ors s t v u) => (or s (or t (or v u)))
  literals*       lits            [abcd]         (lits #\a #\b #\c #\d) => (ors (lit #\a) (lit #\b) (lit #\c) (lit #\d))
                                                   => (or (lit #\a) (or (lit #\b) (or (lit #\c) (lit #\d))))
  interval        inter           [a-c]          The set of all letters in the respective interval: 
                                                   (inter #\a #\c) => (lits #\a #\b #\c)
  intervals*      inters          [a-c1-3]       Multiple intervals:
                                                   (inters #\a #\c 1 3) => (ors (lits #\a #\b #\c) (lits #\1 #\2 #\3))
  star            star            s*             Each string in the language of s is a concatenation of ANY number 
                                                   of strings in the language of s.
  plus            plus            s+             Each string in the language of s is a concatenation of ONE or 
                                                   more strings in the language of s.
  optional        opt             s?             A string in the language of s can occur ZERO or ONE time.

*However, it makes more sense to implement inline ors with or, lits with lit, and inters with inter. It will 
also reduce the numbers of states and transitions right off (even though they will be minimized in conversion
to a DFA anyways.
|#
(defgeneric push-reglex (reglex-list FA-state-previous FA-state-next FA-system &rest reglex-op-args)
  (:documentation "Accept a list/tree composed of (a) lisp'ified regular expression(s) and recursively evaluate them into an FA."))

(defmethod push-reglex ((reglex list) (state-prev fa-state) (state-next fa-state) (fa-system fa-system) &rest reglex-op-args)
  "Decompose list into an operator and its operands."
  (declare (ignore reglex-op-args))
  (let ((operator (first reglex))
        (operands (rest reglex)))
    (apply #'push-reglex operator
                         state-prev
                         state-next
                         fa-system
                         operands)))

(defmethod push-reglex ((ε (eql 'ε)) (state-prev fa-state) (state-next fa-state) (fa-system fa-system) &rest reglex-op-args)
  "The empty string or epsilon transition: \"\" <=> {\"\"} <=> (ε) <=> (epsilon)."
  (if reglex-op-args
      (error "Additional reglex arguments passed to ε operator.")
      (with-slots ((nfa fa)) fa-system
	(make-transition 'ε
			 state-prev
			 state-next
			 nfa))))

(defmethod push-reglex ((lit (eql 'lit)) (state-prev fa-state) (state-next fa-state) (fa-system fa-system) &rest lit-args)
  "One or more literal symbols (character): \"ab\" <=> {\"a\", \"b\"} <=> (lit #\a #\b)"
  (with-slots ((state-kernel state-kernel) (fa fa)) fa-system
    ;; Make new states with character transitions from state-prev to new states, and then ε transitions from
    ;;   each of those new states to state-next.
    (loop :for character :in lit-args
	  :do (let ((char-state-prev/next (make-state state-kernel)))
		(make-transition character
				 state-prev
				 char-state-prev/next
				 fa)
		(make-transition 'ε
				 char-state-prev/next
				 state-next
				 fa))
	  :finally (return state-next))))
#|
(defmethod push-reglex ((lits (eql 'lits)) (state-prev fa-state) (state-next fa-state) (fa-system fa-system) &rest lits-args)
  "Multiple literal symbols: [\"a\"\"b\"\"c\"] <=> {\"a\",\"b\",\"c\"} <=> (lits #\a #\b #\c) <=> (ors (lit #\a) (lit #\b) (lit #\c))."
  (error "stub")))
|#
(defmethod push-reglex ((or (eql 'or)) (state-prev fa-state) (state-next fa-state) (fa-system fa-system) &rest or-args)
  "A string in the language s, (x)or in the language t: s|t <=> L(s) ∪ L(t) <=> (or s t)."
  (with-slots ((state-kernel state-kernel) (fa fa)) fa-system
    (loop :for reglex :in or-args
	  :do (let ((reglex-state-prev (make-state state-kernel))
		    (reglex-state-next (make-state state-kernel)))
		(make-transition 'ε
				 state-prev
				 reglex-state-prev
				 fa)
		(make-transition 'ε
				 (push-reglex reglex
					      reglex-state-prev
					      reglex-state-next
					      fa-system); => reglex-state-next
				 state-next
				 fa)))))
#|
(defmethod push-reglex ((ors (eql 'ors)) (state-prev fa-state) (state-next fa-state) (fa-system fa-system) &rest ors-args)
  "Langauge is set: [st...v] <=> s|t|...|v <=> L(s) ∪ L(t) ∪ ... ∪ L(v) <=> (ors s t ... v) <=> (or s (or t (or ... (or v)...)))."
  (error "stub"))
|#
(defmethod push-reglex ((conc (eql 'conc)) (state-prev fa-state) (state-next fa-state) (fa-system fa-system) &rest conc-args)
  "The language defined by concatenating a string from language s with a string from language t: st <=> {mn | m∈L(s), n∈L(t)} <=> (conc s t)."
  (error "stub"))

(defmethod push-reglex ((star (eql 'star)) (state-prev fa-state) (state-next fa-state) (fa-system fa-system) &rest star-args)
  "A string that is a concatenation of zero or more strings in the language s: s* <=> {\“\”} ∪ {vw | v∈L(s), w∈L(s∗)} <=> (star s)."
  (error "stub"))

(defmethod push-reglex ((plus (eql 'plus)) (state-prev fa-state) (state-next fa-state) (fa-system fa-system) &rest plus-args)
  "A string that is a concatenation of one or more strings in the language s: s+ <=> {xy | x∈L(s), y∈L(s*)} <=> (plus s)."
  (error "stub"))

(defmethod push-reglex ((inter (eql 'inter)) (state-prev fa-state) (state-next fa-state) (fa-system fa-system) &rest inter-args)
  "Shorthand for or'ing all characters in an interval: [\"0\"-\"9\"] <=> {\"0\",\"1\", ... ,\"9\"} <=> (inter #\0 #\9) <=> (lits #\0 #\1 ... #\9)."
  (error "stub"))

(defmethod push-reglex ((opt (eql 'opt)) (state-prev fa-state) (state-next fa-state) (fa-system fa-system) &rest opt-args)
  "An optional symbol or set: \"a\"? <=> {\"a\",\"\"} <=> (opt #\a)"
  (error "stub"))



