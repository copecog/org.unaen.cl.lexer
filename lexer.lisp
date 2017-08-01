;;;; lexer.lisp

(in-package #:lexer)

;;; "lexer" goes here. Hacks and glory await!

;; https://en.wikipedia.org/wiki/Automata_theory
(defclass f-automata ()
  ((Q :initarg :Q
      :accessor Q
      :documentation "A finite set of states Q.")
   (Sigma :initarg :Sigma
          :accessor Sigma
          :documentation "A finite set of input symbols Σ.")
   (Delta :initarg :Delta
          :accessor Delta
          :documentation "A transition function Δ : Q × Σ → P(Q).")
   (q_0 :initarg :q_0
	:accessor q_0
	:documentation "An initial (or start) state q0 ∈ Q.")
   (F :initarg :F
      :accessor F
      :documentation "A set of states F distinguished as accepting (or final) states F ⊆ Q."))
  (:documentation "A Finite Automata is represented formally by a 5-tuple: (Q, Σ, Δ, q_0, F)."))

;; https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton
(defclass n-f-automata (f-automata)
  ()
  (:documentation "A nondeterministic variant of a finite automata."))

;; https://en.wikipedia.org/wiki/Deterministic_finite_automaton
(defclass d-f-automata (f-automata)
  ()
  (:documentation "A deterministic variant of a finite automata."))
