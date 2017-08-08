(in-package #:lexer)

(defclass name ()
  ((name :initarg :name
	 ;:initform (format nil "~a~d" name-preface iterate)
	 :reader name
	 :documentation "External name for a specific state.")))

(defclass name-preface ()
  ((name-preface :initarg :name-preface
		 :initform "q_"
		 :reader name-preface
		 :documentation "Naming convention for new states in series.")))

(defclass iterate ()
  ((iterate :initarg :iterate
	    :initform (error "Requires a valid iterate!")
	    :reader iterate
	    :documentation "Enumeration for each state in a series.")))

;;   Whatever the input symbols are, they need enumeration--however, we are going
;; to use the built in character encoding functions in lisp to give us that
;; mapping (signified by a 'cl-utf slot-value).
(defclass alphabet ()
  ((alphabet :initarg :alphabet
	     :initform 'cl-utf
	     :reader alphabet
	     :documentation "The input alphabet, or set of input symbols.")))

(defclass trans ()
  ((trans :initarg :trans
	  :initform (make-hash-table :test 'eql) ;:test 'eql works for chars.
	  :reader trans
	  :documentation "Transitions from a specific state.")))

;;   The q_0 slot is the standard name in the mathematical definition for the
;; starting state.
(defclass init-state ()
  ((init-state :initarg :init-state
	       ;:initform
	       :reader init-state
	       :documentation "Initial state of a finite state automaton.")))

;;   F is an array of the same size as Q with the accepting states under the same
;; respective indices as the states under Q.
(defclass final-states ()
  ((final-states :initarg :final-states
		 ;:initform
		 :reader final-states
		 :documentation "Final states of a finite state automaton.")))

(defclass state (name iterate trans init-state final-states name-preface) ()
  (:documentation "A specific state in a finite state automaton."))

;;   Whatever type each state is, it is enumerated by the integer indices of an
;; array (Q).
(defclass state-names ()
  ((state-names :initarg :state-names
		;:initform
	        :reader state-names
	        :documentation "All names for states of a finite automaton.")))

;;   Using the enumeration from Q we store the set of transitions as a hash table
;; for each input symbol under the same index integer.
(defclass states ()
  ((states :initarg :states
	   ;:initform
	   :reader states
	   :documentation "All states of a finite automaton.")))

;; https://en.wikipedia.org/wiki/Automata_theory
(defclass FA (state-names alphabet states init-state final-states) ()
  (:documentation #.(format nil "A Finite Automaton is represented formally ~
                                 by a 5-tuple: (Q, Σ, Δ, q_0, F).")))

;; https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton
(defclass NFA (FA) ()
  (:documentation "A Nondeterministic Finite Automaton."))

;; https://en.wikipedia.org/wiki/Deterministic_finite_automaton
(defclass DFA (FA) ()
  (:documentation "A Deterministic Finite Automaton."))

(defmethod initialize-instance ((state-instance state) &key &allow-other-keys)
  (with-slots (name name-preface iterate) state-instance
    (setf name (format nil "~a~d" name-preface iterate)))) 
      

;(defmethod initialize-instance ((fa-instance FA) &key &allow-other-keys)
;  (with-slots (state-names states init-state final-states) fa-instance
;      ))
