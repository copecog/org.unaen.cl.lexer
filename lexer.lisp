;;;; lexer.lisp

(in-package #:org.unaen.cl.lexer)

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
      :initform 'Q
      :reader Q
      :documentation "A finite set of states.")
   (Σ :initarg :Σ
      :initform 'sigma
      :reader Σ
      :documentation "A finite set of input symbols.")
   (Δ :initarg :Δ
      :initform 'delta
      :reader Δ
      :documentation "A transition function Δ : Q ✕ Σ → P(Q).")
   (q₀ :initarg :q₀
       :initform 'q_0
       :reader q₀
       :documentation "An initial (or start) state q₀ ∊ Q.")
   (F :initarg :F
      :initform 'F
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

(defgeneric eval-reglex->nfa (regular-lisp-expression &rest reglex-tree-decrementer)
  (:documentation "Accept a list/tree composed of (a) lisp'ified regular expression(s) and recursively evaluate them into an NFA.")
  (:method ((reglex list) &rest reglex-dec)
    "Decompose list into an operator and its operands."
    (let ((operator (first reglex))
          (operands (rest reglex)))
      (apply #'eval-reglex->nfa
             operator
             operands)))
  (:method ((lit (eql 'lit)) &rest reglex-dec)
    "A literal symbol (character): \"a\" <=> {\"a\"} <=> (lit #\a)"
    reglex-dec)
  (:method ((ε (eql 'epsilon)) &rest reglex-dec)
    "Another name for the ε operator (recurses to eql 'ε method)."
    (error "Stub!"))
  (:method ((ε (eql 'ε)) &rest reglex-dec)
    "The empty string or epsilon transition: \"\" <=> {\"\"} <=> (ε) <=> (epsilon)."
    (error "Stub!"))
  (:method ((or (eql 'or)) &rest reglex-dec)
    "A string in the language s, (x)or in the language t: s|t <=> L(s) ∪ L(t) <=> (or s t)."
    (error "Stub!"))
  (:method ((conc (eql 'conc)) &rest reglex-dec)
    "The language defined by concatenating a string from language s with a string from language t: st <=> {mn | m∈L(s), n∈L(t)} <=> (conc s t)."
    (error "Stub!"))
  (:method ((star (eql 'star)) &rest reglex-dec)
    "A string that is a concatenation of zero or more strings in the language s: s* <=> {\“\”} ∪ {vw | v∈L(s), w∈L(s∗)} <=> (star s)."
    (error "Stub!"))
  (:method ((plus (eql 'plus)) &rest reglex-dec)
    "A string that is a concatenation of one or more strings in the language s: s+ <=> {xy | x∈L(s), y∈L(s*)} <=> (plus s)."
    (error "Stub!"))
  (:method ((inter (eql 'inter)) &rest reglex-dec)
    "Shorthand for or'ing all characters in an interval: [\"0\"-\"9\"] <=> {\"0\",\"1\", ... ,\"9\"} <=> (inter #\0 #\9) <=> (lits #\0 #\1 ... #\9)."
    (error "Stub!"))
  (:method ((opt (eql 'opt)) &rest reglex-dec)
    "An optional symbol or set: \"a\"? <=> {\"a\",\"\"} <=> (opt #\a)"
    (error "Stub!"))
  (:method ((ors (eql 'ors)) &rest reglex-dec)
    "Multiple or's: s|t|...|v <=> L(s) ∪ L(t) ∪ ... ∪ L(v) <=> (ors s t ... v) <=> (or s (or t (or ... (or v)...)))."
    (error "Stub!"))
  (:method ((lits (eql 'lits)) &rest reglex-dec)
    "Multiple literal symbols: [\"a\"\"b\"\"c\"] <=> {\"a\",\"b\",\"c\"} <=> (lits #\a #\b #\c) <=> (ors (lit #\a) (lit #\b) (lit #\c))."
    (error "Stub!")))

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

