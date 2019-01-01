;;;; lexer.lisp

(in-package #:org.unaen.cl.lexer)

(alias multiple-value-bind mvb)

;;; We define a Finite Automaton as a quintuple (Q,Σ,Δ,q₀,F), where:
;;;  - Q is a finite set of states.
;;;  - Σ (Sigma) is a finite set of input symbols.
;;;  - Δ (Delta) is a transition function Δ: Q ✕ Σ → P(Q).
;;;  - q₀ is an initial (or start) state q₀ ∊ Q.
;;;  - F is the accepting or final states F ⊆ Q.

;;; Given an FA and a sequence of input symbols - then this sequence is in the
;;; (regular) language defined by the FA if there exists a sequence of states
;;; beginning at the start state and connected via transititions on each
;;; subsequent input symbol until ending on an accepting state:
;;;   q₀∊Q, q_1∊Q, ... , q_n∊F⊆Q.

;; The FA Quintuple as a class: 
(defclass FA ()
  ((Q :initarg :Q
      :initform (make-set)
      :reader Q
      :documentation "A finite set of states.")
   (Σ :initarg :Σ
      :initform (make-set)
      :reader Σ
      :documentation "A finite set of input symbols.")
   (Δ :initarg :Δ
      :initform (make-map 2)
      :reader Δ
      :documentation "A transition function Δ : Q ✕ Σ → P(Q).")
   (q₀ :initarg :q₀
       :initform (make-state)
       :reader q₀
       :documentation "An initial (or start) state q₀ ∊ Q.")
   (F :initarg :F
      :initform (make-set)
      :reader F
      :documentation "The accepting or final states F ⊆ Q"))
  (:documentation "An object containing the 5-tuple (Q,Σ,Δ,q₀,F) that represents a Finite Automaton."))

;; We are going to deal with Nondeterministic Finite Automaton first:
(defclass NFA (FA) ()
  (:documentation "An object containing the 5-tuple (Q,Σ,Δ,q₀,F) - inherited from the FA class - that represents a Nondeterministic Finite Automaton."))

;;; In an NFA, transistions from state to state are based on input symbols σ∊Σ,
;;; OR on an additional special symbol ε (epsilon), called an epsilon transition,
;;; thus creating the non-deterministic nature of the finite automaton.

;;; It is only sufficient that a sequence of states and transitions exist for
;;; our sequence of input symbols to be accepted as a string in our regular
;;; language defined by the NFA.

;;; Regular expressions themselves can not be defined with a regular language:
;;; This is convenient for us as using an s-expression based notation will
;;; present a type of already parsed form, thus avoiding the need of a parser
;;; or lexer in using our forms to specify a regular language. We are actually
;;; depending on the Reader in Common Lisp to do this for us, and writing this
;;; program is in part for the author to eventually use the reader to full
;;; advantage.

(defgeneric make-set (initial-elements))

(defgeneric make-map (map-dimension))

(defgeneric make-state (fa-inst))

(defgeneric make-transition (state-a state-b transit-symbol fa-inst))

(defgeneric push-reglex (regl-ex/op state-in state-out fa-inst &rest reglex-op-args)
  (:documentation "Accept a list/tree composed of (a) lisp'ified regular expression(s) and recursively evaluate them into an FA."))

(defmethod push-reglex ((reglex list) (fa-inst fa) state-in state-out &rest reglex-args)
  "Decompose list into an operator and its operands."
  (let ((operator (first reglex))
        (operands (rest reglex)))
    (if (null reglex-args)
        (push-reglex operator
                     fa-inst
                     state-in
                     state-out
                     operands)
        (error "Additional reglex arguments can only be passed for a specific reglex operator."))))

(defmethod push-reglex ((lit (eql 'lit)) (nfa-inst nfa) state-in state-out &rest lit-args)
  "A literal symbol (character): \"a\" <=> {\"a\"} <=> (lit #\a)"
  (dolist (transit-symbol lit-args result)
    
  (error "stub"))

(defmethod push-reglex ((ε (eql 'epsilon)) (nfa-inst nfa) state-in state-out &rest ε-args)
  "Another name for the ε operator (recurses to eql 'ε method)."
  (push-reglex 'ε nfa-inst state-in ))

(defmethod push-reglex ((ε (eql 'ε)) (nfa-inst nfa) state-in state-out &rest ε-args)
  "The empty string or epsilon transition: \"\" <=> {\"\"} <=> (ε) <=> (epsilon)."
  (make-transition state-in state-out 'ε nfa-inst) 
  (error "stub"))

(defmethod push-reglex ((or (eql 'or)) (nfa-inst nfa) state-in state-out &rest or-args)
  "A string in the language s, (x)or in the language t: s|t <=> L(s) ∪ L(t) <=> (or s t)."
  (error "stub"))

(defmethod push-reglex ((conc (eql 'conc)) (nfa-inst nfa) state-in state-out &rest conc-args)
  "The language defined by concatenating a string from language s with a string from language t: st <=> {mn | m∈L(s), n∈L(t)} <=> (conc s t)."
  (error "stub"))

(defmethod push-reglex ((star (eql 'star)) (nfa-inst nfa) state-in state-out &rest star-args)
  "A string that is a concatenation of zero or more strings in the language s: s* <=> {\“\”} ∪ {vw | v∈L(s), w∈L(s∗)} <=> (star s)."
  (error "stub"))

(defmethod push-reglex ((plus (eql 'plus)) (nfa-inst nfa) state-in state-out &rest plus-args)
  "A string that is a concatenation of one or more strings in the language s: s+ <=> {xy | x∈L(s), y∈L(s*)} <=> (plus s)."
  (error "stub"))

(defmethod push-reglex ((inter (eql 'inter)) (nfa-inst nfa) state-in state-out &rest inter-args)
  "Shorthand for or'ing all characters in an interval: [\"0\"-\"9\"] <=> {\"0\",\"1\", ... ,\"9\"} <=> (inter #\0 #\9) <=> (lits #\0 #\1 ... #\9)."
  (error "stub"))

(defmethod push-reglex ((opt (eql 'opt)) (nfa-inst nfa) state-in state-out &rest opt-args)
  "An optional symbol or set: \"a\"? <=> {\"a\",\"\"} <=> (opt #\a)"
  (error "stub"))

(defmethod push-reglex ((ors (eql 'ors)) (nfa-inst nfa) state-in state-out &rest ors-args)
  "Multiple or's: s|t|...|v <=> L(s) ∪ L(t) ∪ ... ∪ L(v) <=> (ors s t ... v) <=> (or s (or t (or ... (or v)...)))."
  (error "stub"))

(defmethod push-reglex ((lits (eql 'lits)) (nfa-inst nfa) state-in state-out &rest lits-args)
  "Multiple literal symbols: [\"a\"\"b\"\"c\"] <=> {\"a\",\"b\",\"c\"} <=> (lits #\a #\b #\c) <=> (ors (lit #\a) (lit #\b) (lit #\c))."
  (error "stub")))

#|
(defgeneric foo (par-1 &optional par-2 &rest par-rest)
  (:method ((par-1 list) &optional (par-2 'par-2-default) &rest par-rest)
    (apply #'foo
           (first par-1)
           par-2 (rest par-1)))
  (:method ((blah (eql 'blah)) &optional par-2 &rest par-rest)
    (values blah par-2 par-rest)))

(when (= 2019 (nth-value 5 (decode-universal-time (get-universal-time)))) (princ "Happy new year!"))
|#

(defparameter *reglex.lang.c.float*
  '(conc (opt (lits #\+ #\-))
         (or (conc (or (conc (plus (inter #\0 #\9))
                             (lit #\.)
                             (star (inter #\0 #\9)))
	               (conc (lit #\.)
			     (plus (inter #\0 #\9))))
		   (opt (conc (lits #\e #\E)
		              (opt (lits #\+ #\-))
		              (plus (inter #\0 #\9)))))
	     (conc (plus (inter #\0 #\9))
	           (lits #\e #\E)
	           (opt (lits #\+ #\-))
                        (plus (inter #\0 #\9)))))
  "Reglex for a C language floating point number, equivalent to regex:  [+-]?((([0-9]+.[0-9]∗ | .[0-9]+)([eE][+-]?[0-9]+)?) | [0-9]+[eE][+-]?[0-9]+)")

