(in-package #:lexer)

(defclass name ()
  ((name :initarg :name
	 ;:initform
	 :reader name
	 :documentation "External name for a specific state.")))

(defclass name-preface ()
  ((name-preface :initarg :name-preface
		 :initform "q_"
		 :reader name-preface
		 :documentation "Naming convention for new states in series.")))

(defclass iterate ()
  ((iterate :initarg :iterate
	    :initform 0
	    :reader iterate
	    :documentation "Enumeration for each state in a series.")))

(defclass alphabet ()
  ((alphabet :initarg :alphabet
	     :initform 'cl-utf
	     :reader alphabet
	     :documentation "The input alphabet, or set of input symbols.")))

(defclass trans ()
  ((trans :initarg :trans
	  :initform (make-hash-table :test 'eql)
	  :reader trans
	  :documentation "Transitions from a specific state.")))

(defclass init-state ()
  ((init-state :initarg :init-state
	       ;:initform
	       :reader init-state
	       :documentation "Initial state of a finite state automaton.")))

(defclass final-states ()
  ((final-states :initarg :final-states
		 ;:initform
		 :reader final-states
		 :documentation "Final states of a finite state automaton.")))

(defclass state (name iterate trans init-state final-states name-preface) ()
  (:documentation "A specific state in a finite state automaton."))

(defclass state-names ()
  ((state-names :initarg :state-names
		;:initform
	        :reader state-names
	        :documentation "All names for states of a finite automaton.")))

(defclass states ()
  ((states :initarg :states
	   ;:initform
	   :reader states
	   :documentation "All states of a finite automaton.")))

(defclass FA (state-names alphabet states init-state final-states) ()
  (:documentation #.(format nil "A Finite Automaton is represented formally ~
                                 by a 5-tuple: (Q, Σ, Δ, q_0, F).")))
