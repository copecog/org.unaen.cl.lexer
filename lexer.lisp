;;;; lexer.lisp

(in-package #:lexer)

;;; "lexer" goes here. Hacks and glory await!

;; https://en.wikipedia.org/wiki/Automata_theory
(defclass finite-automaton ()
  ((Q :initarg :Q
      :initform (error "Must supply an initial Q.")
      :accessor Q
      :documentation "A finite set of states Q.")
   (Sigma :initarg :Sigma
	  :initform (error "Must supply an initial Σ.")
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
(defclass nfa (finite-automaton) ()
  (:documentation "A Nondeterministic Finite Automaton."))

;; https://en.wikipedia.org/wiki/Deterministic_finite_automaton
(defclass dfa (finite-automaton) ()
  (:documentation "A Deterministic Finite Automaton."))

(defun make-automaton (class
		       &rest rest
		       &key
			 (Q (make-Q))
			 (Sigma (make-Sigma))
			 (Delta (make-Delta))
			 (q_0 (make-q_0))
			 (F (make-F)))
  (case class
    (finite-automaton
     (make-instance 'finite-automaton :Q Q :Sigma Sigma :Delta Delta :q_0 q_0 :F F))

(defun make-Q (&key (size-of-q 10))
  (make-array size-of-q
	      :initial-element nil
	      :adjustable t
	      :fill-pointer 0))
	
(defun make-Sigma ()
  'Sigma-slot)

(defun make-Delta ()
  'Delta-slot)

(defun make-q_0 ()
  'q_0-slot)

(defun make-F ()
  'F-slot)
