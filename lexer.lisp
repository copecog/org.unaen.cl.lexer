;;;; lexer.lisp
;;;;
;;;; Copyright (C) 2017 Christopher H Cope
;;;; All rights reserved.
;;;;
;;;; This software may be modified and distributed under the terms
;;;; of the BSD license.  See the LICENSE file for details.

;;;; This is intended as a most literal implementation according to the
;;;; mathematical definition of finite automata used by lexers.

(in-package #:lexer)

;;; "lexer" goes here. Hacks and glory await!

;; Classes

;; https://en.wikipedia.org/wiki/Automata_theory
;;
;;   Whatever type each state is, it is enumerated by the integer indices of an
;; array (Q).
;;
;;   Whatever the input symbols are, they need enumeration--however, we are going
;; to use the built in character encoding functions in lisp to give us that
;; mapping (signified by a 'cl-utf slot-value).
;;
;;   Using the enumeration from Q we store the set of transitions as a hash table
;; for each input symbol under the same index integer.
;;
;;   The q_0 slot is the standard name in the mathematical definition for the
;; starting state.
;;
;;   F is an array of the same size as Q with the accepting states under the same
;; respective indices as the states under Q.

(defclass q-state ()
  ((name :initarg :name
	 :initform nil 
	 :reader name
	 :documentation "The external name for this specific state.")
   (enumerator :initarg :enumerator
	       :initform (error "Requires an enumeration integer.")
	       :reader enumerator
	       :documentation "The numerical enumeration this state takes on.")
   (transitions :initarg :transitions
	        :initform (make-hash-table)
	        :accessor transitions
	        :documentation "The transitions hash table.")
   (q_0 ;:allocation :class
        :initarg :q_0
	;initform starting state of containing finite automaton
	:reader q_0
	:documentation "Initial state in containing automaton.")
   (F ;:allocation :class
      :initarg :F
      ;initform accepting states of containing finite automaton
      :reader F
      :documentation "Final states in containing automaton.")))
 
(defclass fa ()
  ((Q :initarg :Q
      ;initform array of q-state names
      :accessor Q
      :documentation "A set of states Q.")
   (Sigma :initarg :Sigma
	  :initform 'cl-utf ;using lisp integrated character encoding
          :reader Sigma
          :documentation "A finite set of input symbols Σ.")
   (Delta :initarg :Delta
	  ;initform array of q-state ojects containing transitions (Q × Σ)
          :accessor Delta
          :documentation "A transition function Δ : Q × Σ → P(Q).") ∈ Q
   (q_0 :initarg :q_0
	;initform first q-state object
        :reader q_0
        :documentation "An initial (or start) state q_0 ∈ Q.")
   (F :initarg :F
      ;initform array of accepting q-state objects 
      :accessor F
      :documentation #.(format nil "A set of states F distinguished as ~
                                   accepting (or final) states F ⊆ Q.")))
  (:documentation #.(format nil "A Finite Automaton is represented formally ~
                                 by a 5-tuple: (Q, Σ, Δ, q_0, F).")))
    
; https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton
(defclass nfa (finite-automata) ()
  (:documentation "A Nondeterministic Finite Automaton."))

; https://en.wikipedia.org/wiki/Deterministic_finite_automaton
(defclass dfa (finite-automata) ()
  (:documentation "A Deterministic Finite Automaton."))

; Initialize name field from enumeration in q-state.
(defmethod initialize-instance :after ((instance q-state)
				       &key (name-preface "q_"))
  (unless (slot-value instance 'name)
    (setf (slot-value instance 'name)
	  (format nil "~a~d" name-preface (slot-value instance 'enumerator)))))

; Make instance of q-state class.
(defun make-q-state (enumerator &key (name-preface "q_"))
  (make-instance 'q-state
		 :enumerator enumerator
		 :name-preface name-preface))

; Make array of q-state names.
(defun make-Q-set (number-of-states &key (name-preface "q_"))
  (do ((Q (make-array number-of-states
		      :initial-element nil
		      :adjustable t
		      :fill-pointer 0))
       (entry-number 0 (1+ entry-number)))
      ((= entry-number number-of-states) Q) ;end on number after last entry
    (vector-push (format nil "~a~d" name-preface entry-number))))

; Make data structure with Delta function maps in automaton.
(defun make-Delta-map (number-of-states &rest rest)
  (do ((Delta (make-array number-of-states
	 	          :initial-element nil
		          :adjustable t
		          :fill-pointer 0))
       (entry-number 0 (1+ entry-number)))
      ((= entry-number number-of-states) Delta) ;end on number after last entry
    (vector-push (make-q-state entry-number rest) Delta)))
