(in-package #:lexer)

;; https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton

(defclass preface ()
  ((preface :initarg :preface
	    :reader preface)))

(defclass iterate ()
  ((iterate :initarg :iterate
	    :reader iterate)))

(defclass states-names (preface iterate)
  ((preface :initform "q_")
   (iterate :initform 0))
  (:documentation "Reference to create a series of related states."))

(defclass Q ()
  ((Q :initarg :Q
      :initform (make-state-vector 1)
      :reader Q
      :documentation "A finite set of states Q.")))

(defclass Sigma ()
  ((Sigma :initarg :Sigma
	  :initform 'cl-utf
	  :reader Sigma
	  :documentation "A finite set of input symbols Σ.")))

(defclass Delta ()
  ((Delta :initarg :Delta
	  :initform (make-state-vector 1)
	  :reader Delta
	  :documentation "A transition function Δ : Q × Σ → P(Q).")))

(defclass Q-Sigma-Delta (Q Sigma Delta) ()
  (:documentation "Q, Sigma, and Delta a priori for complete Delta meaning."))

(defclass q_0 ()
  ((q_0 :initarg :q_0
	:reader q_0
	:documentation "An initial (or start) state q_0 ∈ Q.")))

(defclass Q-q_0 (Q q_0) ()
  (:documentation "Q and q_0 ∈ Q a priori for complete q0 meaning."))

(defclass F ()
  ((F :initarg :F
      :initform (make-state-vector 1)
      :reader F
      :documentation "A set of states F distinguished as accepting (or final) states F ⊆ Q.")))

(defclass Q-F (Q F) ()
  (:documentation "Q and F a priori for complete F meaning."))

(defclass FA (Q-Sigma-Delta Q-q_0 Q-F)
  ((DSN :initarg DSN
	:initform (make-instance 'states-names)
	:reader DSN
	:documentation "_D_efault _S_tates _N_ames."))
  (:documentation "An FA is represented formally by a 5-tuple, (Q, Σ, Δ, q0, F)."))

(defun make-state-vector (size &key (initial-element nil) (adjustable t) (fill-pointer 0))
  (make-array size
	      :initial-element initial-element
	      :adjustable adjustable
	      :fill-pointer fill-pointer))

(defgeneric make-Delta (Sigma-type)
  (:documentation "Create respective character-transition map data-structure."))

(defmethod make-Delta ((Sigma-type (eql 'cl-utf)))
  (make-hash-table :test 'eql)) ;eql for cl char's

(defgeneric make-state-name (states-names)
  (:documentation "Name a new state from a reference class instance for a series of states."))
	      
(defmethod make-state-name ((states-names-instance states-names))
  (with-slots (preface iterate) states-names-instance
    (format nil "~a~d" preface (1- (incf iterate)))))

(defmethod initialize-instance :after ((FA-instance FA)
				       &key &allow-other-keys)
  (push-state (make-state-name (DSN FA-instance)) FA-instance :start-p t))

(defgeneric push-state (state-name finite-automaton
			&key &allow-other-keys)
  (:documentation "Push a state into the finite automaton."))

(defmethod push-state :before (state-name (Q-instance Q)
			       &key &allow-other-keys)
  (with-slots (Q-slot) Q-instance
    (vector-push-extend state-name Q-slot))) ; Push our state-name onto our vector of state names.

(defmethod push-state :before (state-name (Q-Sigma-Delta-instance Q-Sigma-Delta)
			       &key &allow-other-keys)
  (with-slots (Sigma-slot Delta-slot) Q-Sigma-Delta-instance
    (vector-push-extend (make-Delta Sigma-slot) Delta-slot))) ; Extend Delta for state-name.

(defmethod push-state :before (state-name (Q-q_0-instance Q-q_0)
			       &key (start-p nil) &allow-other-keys)
  (with-slots (q_0-slot) Q-q_0-instance
    (when start-p
      (setf q_0-slot state-name)))) ; If start state then set as start state.

(defmethod push-state :before (state-name (Q-F-instance Q-F)
			       &key (final-p nil) &allow-other-keys)
  (with-slots (F-slot) Q-F-instance
    (if final-p
	(vector-push-extend state-name F-slot) ;   Either it is a final state
	(vector-push-extend nil F-slot))))     ; or it is not - so empty space instead.

(defmethod push-state (state (FA-instance FA) &key &allow-other-keys)
  FA-instance) ; Return mutated FA after all said and done.

(defclass NFA (FA) ()
  (:documentation "Non-deterministic Finite Automata."))

;; The notion of pushing it into the object rather than onto a stack.
(defgeneric push-transit (state-A state-B transit-char FA &key &allow-other-keys)
  (:documentation "Push a single transition on trans-char from state-A to state-B in finite automaton."))

(defmethod push-transit ((state-A integer) (state-B integer) transit-char (Delta Q-Sigma-Delta)
			 &key &allow-other-keys)
  (with-slots (





(defun print-FA (FA)
  (print (Q FA))
  (print (Sigma FA))
  (print (Delta FA))
  (print (q_0 FA))
  (print (F FA))
  nil)
