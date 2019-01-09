;;;; classes.lisp

(in-package #:org.unaen.cl.lexer)

(defclass state-names ()
  ((preface :initarg :preface
	    :initform "q_"
	    :reader preface
	    :documentation "A unique preface for each set of states Q.")
   (iterate :initarg :iterate
	    :initform 0
	    :reader iterate
	    :documentation "Each integer used for a single state q∊Q."))
  (:documentation "An object used to contain the state of parameters for state-name generation."))

(defclass FA ()
  ((regex-tree :initarg :regex-tree
	       :initform nil
	       :reader regex-tree
	       :documentation "Regular expression object that the current FA object is derived from.")
   (FA-prev :initarg :FA-prev
	    :initform nil
	    :reader FA-prev
	    :documentation "Previous FA object that currect FA object is derived from.")
   (Q-map :initarg :Q-map
	  :initform (make-state-vector 1)
	  :reader Q-map
	  :documentation "Map from current states of Q to FA-prev states of Q.")
   (Q :initarg :Q
      :initform (make-state-vector 1)
      :reader Q
      :documentation "A finite set of states.")
   (Σ :initarg :Σ
      :initform 'cl-utf
      :reader Σ
      :documentation "A finite set of input symbols.")
   (Σ-in-use :initarg :Σ-in-use
	     :initform (list)
	     :reader Σ-in-use
	     :documentation "The effective Σ that follows from the actual symbols in the regex-tree.")
   (Δ :initarg :Δ
      :initform (make-state-vector 1)
      :reader Δ
      :documentation "A transition function Δ: Q ✕ Σ → P(Q).")
   (q₀ :initarg :q₀
       :initform nil
       :reader q₀
       :documentation "An initial (or start) state q₀∊Q.")
   (q0-prev :initarg :q0-prev
	    :initform (list)
	    :reader q0-prev
	    :documentation "The start states from previous FA-prev's.")
   (F :initarg :F
      :initform (make-state-vector 1)
      :reader F
      :documentation "The accepting or final states F ⊆ Q")
   (dsn :initarg dsn
	:initform (make-instance 'state-names)
	:reader dsn
	:documentation "The parameters (object) for which the state-name's are generated from."))
  (:documentation "An object containing the 5-tuple (Q,Σ,Δ,q₀,F) that represents a Finite Automaton as well as auxillary information according to the implementation."))

(defclass NFA (FA) ()
  (:documentation "A Nondeterministic Finite Automaton derived from the Finite Automaton Class."))

(defclass DFA (FA) ()
  (:documentation "A Deterministric Finite Automaton derived from the Finite Automaton Class."))
