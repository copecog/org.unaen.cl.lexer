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
      (let* ((FA           (make-instance FA-type))
	     (state-kernel (make-instance 'FA-state-kernel))
	     (FA-system    (make-instance 'FA-system
					  :FA FA
					  :state-kernel state-kernel))
	     (Q            (slot-value FA 'Q)))
	(setf (slot-value state-kernel 'states) Q
	      (slot-value state-kernel 'system) FA-system))))

(defmethod make-state ((state-kernel FA-state-kernel))
  (sets:set-add-element (make-instance 'FA-state
				       :enum (state-kernel++ state-kernel)
				       :kernel state-kernel)
			(slot-value state-kernel 'states))))

(defmethod state-kernel++ ((state-kernel FA-state-kernel))
  (let ((iterate++ (+ (slot-value state-kernel 'iterate)
		      1)))
    (setf (slot-value state-kernel 'iterate)
	  iterate++))))

(defgeneric make-transition (transit-symbol state-prev state-next fa-inst)
  (:method ((transit-symbol character) (state-prev atom) (state-next atom) (fa-inst fa))
    (error "stub"))
  (:method ((ε (eql 'epsilon)) (state-prev atom) (state-next atom) (nfa-inst nfa))
    (error "stub"))


#|
NAME            OPERATOR        EXPRESSION     NOTES
  literal         lit | lits      "a"            The set consisting of the one-letter string {"a"}. 
                                                   (lit #\a #\b ...) => (ors (lit #\a) (lit #\b) ...)
  epsilon         ε | epsilon     ε              The set containing the empty string {""}.
  concatenate     conc            st             Strings constructed by concatenating a string from the language 
                                                   of s with a string from the language of t.
  or              or              s|t            Strings from both languages.
  brackets        ors             [ab01]         The set of these letters: "a"|"b"|"0"|"1"
  interval        inter           [a-c1-3]       The set of all letters in the respective 
                                                   intervals: "a"|"b"|"c"|"1"|"2"|"3"
  star            star            s*             Each string in the language of s is a concatenation of ANY number 
                                                   of strings in the language of s.
  plus            plus            s+             Each string in the language of s is a concatenation of ONE or 
                                                   more strings in the language of s.
  optional        opt             s?             A string in the language of s can occur ZERO or ONE time.
|#

  
(defgeneric push-reglex (regl-ex/op state-prev state-next fa-inst &rest reglex-op-args)
  (:documentation "Accept a list/tree composed of (a) lisp'ified regular expression(s) and recursively evaluate them into an FA."))

(defmethod push-reglex ((reglex list) state-prev state-next (fa-inst fa) &rest reglex-op-args)
  "Decompose list into an operator and its operands."
  (let ((operator (first reglex))
        (operands (rest reglex)))
    (if reglex-op-args
        (error "Additional reglex arguments passed when reglex in list/tree form.")
        (push-reglex operator
                     state-prev
                     state-next
                     fa-inst
                     operands))))

(defmethod push-reglex ((ε (eql 'epsilon)) state-prev state-next (nfa-inst nfa) &rest reglex-op-args)
  "Another name for the ε operator (recurses to eql 'ε method)."
  (if reglex-op-args
      (error "Additional reglex arguments passed for epsilon operator.") 
      (push-reglex 'ε
                   state-prev
                   state-next
                   nfa-inst)

;; Every method needs to return the FA and the state-begin and state-end. What does it mean to have multiple epsilon transitions?
(defmethod push-reglex ((ε (eql 'ε)) state-prev state-next (nfa-inst nfa) &rest reglex-op-args)
  "The empty string or epsilon transition: \"\" <=> {\"\"} <=> (ε) <=> (epsilon)."
  (if reglex-op-args
      (error "Additional reglex arguments passed for ε operator.") 
      (make-transition 'ε
                       state-prev
                       state-next
                       nfa-inst)))

(defmethod push-reglex ((lit (eql 'lit)) state-prev state-next (nfa-inst nfa) &rest lit-args)
  "A literal symbol (character): \"a\" <=> {\"a\"} <=> (lit #\a)"
  (apply #'push-reglex
         'or
         state-prev
         state-next
         nfa-inst
         lit-args))
#|
(defmethod push-reglex ((lits (eql 'lits)) state-prev state-next (nfa-inst nfa) &rest lits-args)
  "Multiple literal symbols: [\"a\"\"b\"\"c\"] <=> {\"a\",\"b\",\"c\"} <=> (lits #\a #\b #\c) <=> (ors (lit #\a) (lit #\b) (lit #\c))."
  (error "stub")))
|#
(defmethod push-reglex ((or (eql 'or)) state-prev state-next (nfa-inst nfa) &rest or-args)
  "A string in the language s, (x)or in the language t: s|t <=> L(s) ∪ L(t) <=> (or s t)."
  (error "stub"))
#|
(defmethod push-reglex ((ors (eql 'ors)) state-prev state-next (nfa-inst nfa) &rest ors-args)
  "Langauge is set: [st...v] <=> s|t|...|v <=> L(s) ∪ L(t) ∪ ... ∪ L(v) <=> (ors s t ... v) <=> (or s (or t (or ... (or v)...)))."
  (error "stub"))
|#
(defmethod push-reglex ((conc (eql 'conc)) state-prev state-next (nfa-inst nfa) &rest conc-args)
  "The language defined by concatenating a string from language s with a string from language t: st <=> {mn | m∈L(s), n∈L(t)} <=> (conc s t)."
  (error "stub"))

(defmethod push-reglex ((star (eql 'star)) state-prev state-next (nfa-inst nfa) &rest star-args)
  "A string that is a concatenation of zero or more strings in the language s: s* <=> {\“\”} ∪ {vw | v∈L(s), w∈L(s∗)} <=> (star s)."
  (error "stub"))

(defmethod push-reglex ((plus (eql 'plus)) state-prev state-next (nfa-inst nfa) &rest plus-args)
  "A string that is a concatenation of one or more strings in the language s: s+ <=> {xy | x∈L(s), y∈L(s*)} <=> (plus s)."
  (error "stub"))

(defmethod push-reglex ((inter (eql 'inter)) state-prev state-next (nfa-inst nfa) &rest inter-args)
  "Shorthand for or'ing all characters in an interval: [\"0\"-\"9\"] <=> {\"0\",\"1\", ... ,\"9\"} <=> (inter #\0 #\9) <=> (lits #\0 #\1 ... #\9)."
  (error "stub"))

(defmethod push-reglex ((opt (eql 'opt)) state-prev state-next (nfa-inst nfa) &rest opt-args)
  "An optional symbol or set: \"a\"? <=> {\"a\",\"\"} <=> (opt #\a)"
  (error "stub"))



