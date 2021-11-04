;;;; org.unaen.cl.lexer/classes.lisp

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

(defclass FA ()
  ((Q :initarg :Q
      :initform (sets:set)
      :accessor Q
      :documentation "A finite set of states.")
   (Σ :initarg :Σ
      :initform (sets:set)
      :accessor Σ
      :documentation "A finite set of input symbols.")
   (Δ :initarg :Δ
      :initform (maps:map :dimension 2)
      :reader Δ
      :documentation "A transition function Δ : Q ✕ Σ → P(Q).")
   (q₀ :initarg :q₀
       :accessor q₀
       :documentation "An initial (or start) state q₀ ∊ Q.")
   (F :initarg :F
      :initform (sets:set)
      :accessor F
      :documentation "The accepting or final states F ⊆ Q"))
  (:documentation "An object containing the 5-tuple (Q,Σ,Δ,q₀,F) that represents a Finite Automaton."))

(defclass FA-system ()
  ((reglex :initarg :reglex
           :initform '()
           :accessor reglex
           :documentation "\"Regular Lisp Expression\" object that the current FA object is derived from.")
   (FA :initarg :FA
       :initform nil
       :accessor FA
       :documentation "Current FA instance for reglex.")
   (FA-system-prev :initarg :FA-system-prev
		   :initform nil
		   :accessor FA-system-prev
		   :documentation "Previous FA system that this system was derived from.")
   (Q-maps :initarg :Q-maps
           :initform (maps:map); (map-get new-state Q-maps) => {old-state-1, old-state-2, ...}
           :accessor Q-maps
           :documentation "Contains a map from derived FA states to the respective previous FA states.")
   (state-kernel :initarg :state-kernel
                 :initform (make-instance 'FA-state-kernel)
                 :accessor state-kernel
                 :documentation "Object containing state of generation for FA states."))
  (:documentation "All relevant data for productions of various finite automatons."))

(defclass FA-state ()
  ((enum :initarg :enum
         :initform nil
         :accessor enum
         :documentation "Unique enumeration for this state.")
   (kernel :initarg :kernel
           :initform nil
           :accessor kernel
           :documentation "The FA-state-kernel instance from which this state was generated."))
  (:documentation "An object with a unique identity representing a specific state in a finite automaton."))

(defclass FA-state-kernel ()
  ((iterate :initarg :iterate
	    :initform 0
	    :accessor iterate
            :documentation "An integer to iterate for state enumeration.")
   (states :initarg :states
           :initform (sets:set)
           :accessor states
           :documentation "The states generated from this kernel, should be same (EQ) set to Q in FA when kernel part of system."))
  (:documentation "An object used to contain the state of parameters for state generation."))

(defclass NFA (FA) ()
  (:documentation "An object containing the 5-tuple (Q,Σ,Δ,q₀,F) - inherited from the FA class - that represents a Nondeterministic Finite Automaton."))

(defclass DFA (FA) ()
  (:documentation "An object containing the 5-tuple (Q,Σ,Δ,q₀,F) - inherited from the FA class - that represents a Deterministic Finite Automaton."))

