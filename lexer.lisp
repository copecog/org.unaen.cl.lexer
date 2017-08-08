(in-package #:lexer)

;; Arbitrary abstractions
(defun make-state-name (preface iterate)
  (format nil "~a~d" preface iterate))

(defun make-state-vector (size)
  (make-array size
	      :initial-element nil
	      :adjustable t
	      :fill-pointer 0))

;; ------------------------------------------------------------------------------
;;   Buildup for individual states.

;;   There are several classes of states across varying classes of finite
;; automata as well as transitional sets of states while functionally mapping
;; from one automaton to another.
;;   This scheme with a preface and iterate allows us to have systematic naming
;; that is still flexible.
(defclass name ()
  ((name :initarg :name
	 ;:initform (format nil "~a~d" name-preface iterate)
	 :reader name
	 :documentation "External name for a specific state.")
   (name-preface :initarg :name-preface
		 :initform "q_"
		 :reader name-preface
		 :documentation "Naming convention for new states in series.")
   (iterate :initarg :iterate
	    :initform 0
	    :reader iterate
	    :documentation "Enumeration for each state in a series.")))

;;   To initialize a state, we name the state using the name-preface
;; and the iterate.
(defmethod initialize-instance :after ((name-instance name)
				       &key &allow-other-keys)
  (with-slots (name (preface name-preface) iterate) name-instance
    (setf name (make-state-name preface iterate))))

;;   The transitions are a hashtable that associates the literal next state
;; object with a character.
(defclass transitions ()
  ((transitions :initarg :transitions
	        :initform (make-hash-table :test 'eql) ; 'eql perfect for chars.
	        :reader transitions
	        :documentation "Transitions from a specific state.")))

;;   A state q ∈ Q in finite automata. (Note: The "iterate" slot inherited
;; from the name class is used as the permanent enumeration for the respective
;; state class instance.)
(defclass state (name transitions) ()
  (:documentation "A specific state in a finite state automaton."))

;; ------------------------------------------------------------------------------
;; Buildup of finite automata

;;    Note: The "iterate" slot inherited from the name class is used to track
;; the last state name/instance that was created as well as the "name-preface"
;; for creating new states. I haven't figured out what to do with :initarg
;; iterate's for making these classes of objects directly...

;;   The set Q of states, however named--each state is enumerated by the integer
;; indices of an array.
(defclass state-names (name)
  ((state-names :initarg :state-names
		:initform (make-state-vector 1)
	        :reader state-names
	        :documentation "All names for states of a finite automaton.")))

;;   Push initial state name onto "state-names".
(defmethod initialize-instance :after ((state-names-instance state-names)
				       &key &allow-other-keys)
  (with-slots (name state-names) state-names-instance
    (vector-push-extend name state-names)))

;;   The set Σ of input symbols.  Whatever the input symbols are, they need
;; enumeration--however, we are going to use the built in character encoding in
;; lisp to give us that mapping (signified by a 'cl-utf slot-value).
(defclass alphabet ()
  ((alphabet :initarg :alphabet
	     :initform 'cl-utf
	     :reader alphabet
	     :documentation "The input alphabet, or set of input symbols.")))

;;   A transition function Δ : Q × Σ → P(Q). Using the enumeration from Q we
;; store each state object under an array, and each state contains the
;; transitions on symbols from Σ, which gives us our function map.
(defclass states (name)
  ((states :initarg :states
	   :initform (make-state-vector 1)
	   :reader states
	   :documentation "All states of a finite automaton.")))

;;   Make initial state instance and push onto "states". 
(defmethod initialize-instance :after ((states-instance states) &rest keys)
  (with-slots (name-preface iterate states) states-instance
    (vector-push-extend (apply #'make-instance 'state keys) states)))

;;   The initial or starting state of the finite automata. Usually named as
;; q_0 ∈ Q in the mathematical definition.
(defclass init-state-name (name)
  ((init-state-name :initarg :init-state-name
	            ;:initform name
	            :reader init-state-name
	            :documentation "Initial state of a finite automaton.")))

;;   By default the first state name generated by the name-preface.
(defmethod initialize-instance :after ((instance init-state-name)
				       &key &allow-other-keys)
  (with-slots (name init-state-name) instance
    (setf init-state-name name)))

;;   The set F of final or accepting states. F is an array of the same size as Q
;; with the accepting states under the same respective indices as under Q.
(defclass final-state-names ()
  ((final-state-names :initarg :final-state-names
		      :initform (make-state-vector 1)
		      :reader final-state-names
		      :documentation "Final states of a finite automaton.")))

;;   The class of Finite Automata.
;; https://en.wikipedia.org/wiki/Automata_theory
(defclass FA (state-names alphabet states init-state-name final-state-names) ()
  (:documentation #.(format nil "A Finite Automaton is represented formally ~
                                 by a 5-tuple: (Q, Σ, Δ, q_0, F).")))

;; ------------------------------------------------------------------------------

;; https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton
(defclass NFA (FA) ()
  (:documentation "A Nondeterministic Finite Automaton."))

;; https://en.wikipedia.org/wiki/Deterministic_finite_automaton
(defclass DFA (FA) ()
  (:documentation "A Deterministic Finite Automaton."))


