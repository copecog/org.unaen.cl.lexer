;;;; org.unaen.cl.lexer/classes.lisp

(in-package #:org.unaen.cl.lexer)

;; The FA Quintuple as a class: 
(defclass FA ()
  ((Q :initarg :Q
      :initform (sets:set)
      :reader Q
      :documentation "A finite set of states.")
   (Σ :initarg :Σ
      :initform (sets:set)
      :reader Σ
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
      :reader F
      :documentation "The accepting or final states F ⊆ Q"))
  (:documentation "An object containing the 5-tuple (Q,Σ,Δ,q₀,F) that represents a Finite Automaton."))

(defclass FA-system ()
  ((reglex :initarg :reglex
           :initform '()
           :reader reglex
           :documentation "\"Regular Lisp Expression\" object that the current FA object is derived from.")
   (FA :initarg :FA
       :initform nil
       :accessor FA
       :documentation "Current FA instance for reglex.")
   (FA-prev :initarg :FA-prev
	    :initform (maps:map)
	    :reader FA-prev
	    :documentation "A map of FA object to FA object it was derived from.")
   (Q-maps :initarg :Q-maps
           :initform (maps:map)
           :reader Q-maps
           :documentation "Contains a map from derived FA states to the respective previous FA states.")
   (state-kernel :initarg :state-kernel
                 :initform nil
                 :reader state-kernel
                 :documentation "Object containing state of generation for FA states."))
  (:documentation "All relevant data for productions of various finite automatons."))

(defclass FA-state ()
  ((enum :initarg :enum
         :initform nil
         :reader enum
         :documentation "Unique enumeration for this state.")
   (kernel :initarg :kernel
           :initform nil
           :reader kernel
           :documentation "The FA-state-kernel instance from which this state was generated."))
  (:documentation "An object with a unique identity representing a specific state in a finite automaton."))

(defclass FA-state-kernel ()
  ((iterate :initarg :iterate
	    :initform 0
	    :reader iterate
            :documentation "An integer to iterate for state enumeration.")
   (states :initarg :states
           :initform nil
           :reader states
           :documentation "The states generated from this kernel, should be same (EQ) set to Q in FA when kernel part of system.")
   (system :initarg :system
	   :initform nil
	   :reader system
	   :documentation "The respective parent FA-system when part of a system."))
  (:documentation "An object used to contain the state of parameters for state generation."))

(defclass NFA (FA) ()
  (:documentation "An object containing the 5-tuple (Q,Σ,Δ,q₀,F) - inherited from the FA class - that represents a Nondeterministic Finite Automaton."))

(defclass DFA (FA) ()
  (:documentation "An object containing the 5-tuple (Q,Σ,Δ,q₀,F) - inherited from the FA class - that represents a Nondeterministic Finite Automaton."))
