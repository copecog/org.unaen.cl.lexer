;;;; lexer.lisp

(in-package #:org.unaen.cl.lexer)

;;;(declaim (optimize (speed 3) (safety 0)))

;;; We define a Finite Automaton as a quintuple (Q,Σ,Δ,q₀,F), where:
;;;   - Q is a finite set of states.
;;;   - Σ (Sigma) is a finite set of input symbols.
;;;   - Δ (Delta) is a transition function Δ:Q✕Σ→P(Q).
;;;   - q₀ is an initial (or start) state q₀∊Q.
;;;   - F is the accepting or final states F⊆Q.

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
      :documentation "A transition function Δ:Q✕Σ→P(Q).")
   (q₀ :initarg :q₀
       :initform 'q_0
       :reader q₀
       :documentation "An initial (or start) state q₀∊Q.")
   (F :initarg :F
      :initform 'F
      :reader F
      :documentation "The accepting or final states F⊆Q"))
  (:documentation "An object containing the 5-tuple (Q,Σ,Δ,q₀,F) that represents a Finite Automaton."))

;; We are going to deal with Nondeterministic Finite Automaton first:
(defclass NFA (FA) ()
  (:documentation "An object containing the 5-tuple (Q,Σ,Δ,q₀,F) - inherited from the FA class - that represents a Nondeterministic Finite Automaton."))

;;; In an NFA, transistions from state to state are based on input symbols σ∊Σ,
;;; OR on an additional special symbol ε (epsilon), called an epsilon transition
;;; (thus creating the non-deterministic nature of the finite automaton).

;;; It is only sufficient that a sequence of states and transitions exist for
;;; our sequence of input symbols to be accepted as a string in our (regular)
;;; language defined by the NFA.

;;; Regular expressions themselves can not be defined with a regular language:
;;; This is convenient for us as using an s-expression based notation will
;;; present a type of already parsed form, thus avoiding the need of a parser
;;; (or even a lexer) for our forms to specify a regular language.

;;; Regular Expression, Defined Set,                        Lisp Form,               Explanation: (note: quotes are metasyntactic to indicate the literal symbols)
;;;   "a"                 {"a"}                               (lit #\a)                A literal symbol.
;;;   ""                  {""}                                (ε)                      The empty string (epsilon transition).
;;;   s|t                 L(s) ∪ L(t)                         (or s t)                 A string in the language s, or in the language t.
;;;   st                  {mn | m∈L(s), n∈L(t)}               (conc s t)               The language defined by concatenating a string from language s with a string from language t.
;;;   s*                  {“”} ∪ {vw | v∈L(s), w∈L(s∗)}       (star s)                 A string that is a concatenation of zero or more strings in the language s.
;;;
;;;   s+                  {xy | x∈L(s), y∈L(s*)}              (plus s)                 A string that is a concatenation of one or more strings in the language s.
;;;   ["0"-"9"]           {"0","1", ... ,"9"}                 (inter #\0 #\9)          Shorthand for or'ing all characters in an interval.
;;;   "a"?                {"a",""}                            (opt #\a)                An optional symbol or set.
;;;
;;;   s|t|...|v           L(s) ∪ L(t) ∪ ... ∪ L(v)            (ors s t ... v)          Multiple or's; (or s (or t (or ... (or v)...))).
;;;   ["a""b""c"]         {"a","b","c"}                       (lits #\a #\b #\c)       Multiple literal symbols; (ors (lit #\a) (lit #\b) (lit #\c)).

;; regex tree for a floating point number
;; [+-]? ( ( ([0-9]+.[0-9]∗|.[0-9]+) ([eE][+-]?[0-9]+)? ) | [0-9]+[eE][+-]?[0-9]+ )
(defparameter *test-regex-tree-1*
  '(conc (opt (lits #\+ #\-))
         (or (conc (or (conc (plus (inter #\0 #\9))
                             (lits #\.)
                             (star (inter #\0 #\9)))
	               (conc (lits #\.)
			     (plus (inter #\0 #\9))))
		   (opt (conc (lits #\e #\E)
		              (opt (lits #\+ #\-))
		              (plus (inter #\0 #\9)))))
	     (conc (plus (inter #\0 #\9))
	           (lits #\e #\E)
	           (opt (lits #\+ #\-))
	           (plus (inter #\0 #\9))))))

;; regex tree for "[abc][123]"
(defparameter *test-regex-tree-2*
  '(conc (lits #\a #\b #\c)
         (lits #\1 #\2 #\3)))

;; regex tree for "[ab]*ac"
(defparameter *test-regex-tree-3*
  '(conc (star (lits #\a #\b))
         (lits #\a)
         (lits #\c)))

