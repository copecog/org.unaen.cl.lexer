;;;; lexer.lisp

(in-package #:lexer)

;;; "lexer" goes here. Hacks and glory await!


;; Classes

;; https://en.wikipedia.org/wiki/Automata_theory
(defclass finite-automata ()
  ((Q :initarg :Q
      :initform (make-array 10
			    :initial-element nil
			    :adjustable t
			    :fill-pointer 0)
      :accessor Q
      :documentation "A finite set of states Q.")
   (Sigma :initarg :Sigma
	  :initform 'cl-utf
          :accessor Sigma
          :documentation "A finite set of input symbols Σ.")
   (Delta :initarg :Delta
	  :initform (error "Must supply an initial Δ.")
          :accessor Delta
          :documentation "A transition function Δ : Q × Σ → P(Q).")
   (q_0 :initarg :q_0
	:initform (error "Must supply an initial q_0.")
	:accessor q_0
	:documentation "An initial (or start) state q_0 ∈ Q.")
   (F :initarg :F
      :initform (error "Must supply an initial F.")
      :accessor F
      :documentation "A set of states F distinguished as accepting (or final) states F ⊆ Q."))
  (:documentation "A Finite Automaton is represented formally by a 5-tuple: (Q, Σ, Δ, q_0, F)."))
    
;; https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton
(defclass nfa (finite-automata) ()
  (:documentation "A Nondeterministic Finite Automaton."))

;; https://en.wikipedia.org/wiki/Deterministic_finite_automaton
(defclass dfa (finite-automata) ()
  (:documentation "A Deterministic Finite Automaton."))


;; Creation of automaton objects

(defun make-Q-vector (&key (size-of-q 10))
  (make-array size-of-q
	      :initial-element nil
	      :adjustable t
	      :fill-pointer 0))

(defun make-Q (class &rest keys)
  (case class
    (finite-automata 

(defun make-Sigma ()
  'Sigma-slot)

(defun make-Delta ()
  'Delta-slot)

(defun make-q_0 ()
  'q_0-slot)

(defun make-F ()
  'F-slot)

(defun make-finite-automaton (&key
				(Q (make-Q 'finite-automata))
				(Sigma (make-Sigma 'finite-automata))
				(Delta (make-Delta 'finite-automata))
				(q_0 (make-q_0 'finite-automata))
				(F (make-F 'finite-automata)))
  (make-instance 'finite-automata
		 :Q Q
		 :Sigma Sigma
		 :Delta Delta
		 :q_0 q_0
		 :F F))

(defun make-nfa (&key
		   (Q (make-Q 'nfa))
		   (Sigma (make-Sigma 'nfa))
		   (Delta (make-Delta 'nfa))
		   (q_0 (make-q_0 'nfa))
		   (F (make-F 'nfa)))
  (make-instance 'nfa
		 :Q Q
		 :Sigma Sigma
		 :Delta Delta
		 :q_0 q_0
		 :F F))

(defun make-dfa (&key
		   (Q (make-Q 'nfa))
		   (Sigma (make-Sigma 'nfa))
		   (Delta (make-Delta 'nfa))
		   (q_0 (make-q_0 'nfa))
		   (F (make-F 'nfa)))
  (make-instance 'dfa
		 :Q Q
		 :Sigma Sigma
		 :Delta Delta
		 :q_0 q_0
		 :F F)) 

(defun make-automaton (class &rest class-initarg-keys)
  (case class
    (finite-automaton (make-finite-automaton class-initarg-keys))
    (nfa (make-nfa class-initarg-keys))
    (dfa (make-dfa class-initarg-keys))
    (otherwise (make-instance class class-initarg-keys)))) ;just act like make-instance and perhaps error
