;;;; lexer.lisp
;;;;
;;;; Copyright (C) 2017 Christopher H Cope
;;;; All rights reserved.
;;;;
;;;; This software may be modified and distributed under the terms
;;;; of the BSD license.  See the LICENSE file for details.

;;;;   This is intended as a most literal implementation according to the
;;;; mathematical definition of finite automata used by lexers.

;;;;   The plan is to make something that takes a sort of expression mapping
;;;; directly to piece-wise construction of regular expression NFAs (since
;;;; regular expression grammar itself is context-free), and then convert the
;;;; NFA to a DFA, and the DFA to a generated function that can be used as a
;;;; scanner.

;;;;   After I complete this, I want to write a parser for context free
;;;; grammars that couples with this scanner so that I can generate
;;;; scanner/lexer functions for regular expressions and perl-style regular
;;;; expressions.

(in-package #:lexer)


;;; Perpendicular Utility Stuffs
(defun make-state-vector (size &key (initial-element nil) (adjustable t) (fill-pointer 0))
  (make-array size
	      :initial-element initial-element
	      :adjustable adjustable
	      :fill-pointer fill-pointer))


;;; Generic Function Prototypes
(defgeneric make-Δ (Σ-type)
  (:documentation "Create respective character-transition map data-structure."))

(defgeneric make-state-name (state-names)
  (:documentation "Name a new state from a reference class instance for a series of states."))

(defgeneric push-state (state finite-automaton &key &allow-other-keys)
  (:documentation "Push a state into the finite automaton."))

(defgeneric push-state-next (finite-automata &key &allow-other-keys)
  (:documentation "Push the next implied by state-names under finite-automata."))

;; The notion of pushing it into the object rather than onto a stack.
(defgeneric push-transit (state-A state-B transit-char finite-automata &key &allow-other-keys)
  (:documentation "Push a single transition on transit-char from state-A to state-B in finite-automata."))

(defgeneric delete-transit (state-A state-B transit-char finite-automata &key &allow-other-keys)
  (:documentation "Remove a transition on transit-char from state-A to state-B in finite-automata."))

(defgeneric get-transit (state transit-char finite-automata &key &allow-other-keys)
  (:documentation "Get list of states to transition to on transition character."))

(defgeneric push-fragment (fragment-type finite-automata &key &allow-other-keys)
  (:documentation "Push new finite state automaton fragment onto finite-automata by type."))


;;; Class Definitions

;;   There are several classes of states across varying classes of finite
;; automata as well as transitional sets of states while functionally mapping
;; from one automaton to another.
;;   This scheme with a preface and iterate allows us to have systematic naming
;; that is still flexible.
(defclass state-names ()
  ((preface :initarg :preface
	    :initform "q_"
	    :reader preface)
   (iterate :initarg :iterate
	    :initform 0
	    :reader iterate))
  (:documentation "Reference to create a series of related states."))

;;   The set Q of states, however named--each state is enumerated by the integer
;; indices of an array.
(defclass Q ()
  ((Q :initarg :Q
      :initform (make-state-vector 1)
      :reader Q
      :documentation "A finite set of states Q.")))

;;   The set Σ of input symbols.  Whatever the input symbols are, they need
;; enumeration--however, we are going to use the built in character encoding in
;; lisp to give us that mapping (signified by a 'cl-utf slot-value).
(defclass Σ ()
  ((Σ :initarg :Σ
	  :initform 'cl-utf
	  :reader Σ
	  :documentation "A finite set of input symbols Σ.")))

;;   A transition function Δ : Q × Σ → P(Q). Using the enumeration from Q we
;; store each state object under an array, and each state contains the
;; transitions on symbols from Σ, which gives us our function map.
(defclass Δ ()
  ((Δ :initarg :Δ
	  :initform (make-state-vector 1)
	  :reader Δ
	  :documentation "A transition function Δ : Q × Σ → P(Q).")))

;;   The initial or starting state of the finite automata. Usually named as
;; q₀ ∈ Q in the mathematical definition.
(defclass q₀ ()
  ((q₀ :initarg :q₀
	:reader q₀
	:documentation "An initial (or start) state q₀ ∈ Q.")))

;;   The set F of final or accepting states. F is an array of the same size as Q
;; with the accepting states under the same respective indices as under Q.
(defclass F ()
  ((F :initarg :F
      :initform (make-state-vector 1)
      :reader F
      :documentation "A set of states F distinguished as accepting (or final) states F ⊆ Q.")))

(defclass finite-automata (Q Σ Δ q₀ F)
  ((dsn :initarg dsn
	:initform (make-instance 'state-names)
	:reader dsn
	:documentation "_D_efault _S_tate _N_ames."))
  (:documentation "An finite automata is represented formally by a 5-tuple, (Q, Σ, Δ, q0, F)."))

;; https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton
(defclass nondeterministic-finite-automata (finite-automata) ()
  (:documentation "Nondeterministic Finite Automata."))

;;; Method Definitions
;;   By default the first state name generated by the name-preface.
(defmethod initialize-instance :after ((finite-automata-instance finite-automata) &key &allow-other-keys)
  (push-state 'next finite-automata-instance :start-p t))

;;   The transitions are a hashtable that associates the literal next state
;; object with a character.
(defmethod make-Δ ((Σ-type (eql 'cl-utf)))
  (make-hash-table :test 'eql)) ;eql for cl char's

;;   To initialize a state, we name the state using the name-preface
;; and the iterate.
(defmethod make-state-name ((state-names-instance state-names))
  (with-slots (preface iterate) state-names-instance
    (format nil "~a~d" preface (1- (incf iterate)))))

(defmethod push-state ((next (eql 'next)) (finite-automata-instance finite-automata) &key (start-p nil) (final-p nil)
			       &allow-other-keys)
  (push-state (make-state-name (dsn finite-automata-instance))
	      finite-automata-instance
	      :start-p start-p
	      :final-p final-p))

(defmethod push-state ((state integer) (finite-automata-instance finite-automata) &key (start-p nil) (final-p nil)
		       &allow-other-keys)
  (with-slots (Q) finite-automata-instance
    (if (<= state (fill-pointer Q))
	(values finite-automata-instance state)
	(values finite-automata-instance nil))))

;; All the :before methods I use to organize the assigns for the single push-state operation.

(defmethod push-state :before ((state-name string) (Q-instance Q) &key &allow-other-keys)
  (with-slots (Q) Q-instance
    (vector-push-extend state-name Q))) ; Push our state-name onto our vector of state names.

(defmethod push-state :before ((state-name string) (Δ-instance Δ) &key &allow-other-keys)
  (with-slots (Σ Δ) Δ-instance
    (vector-push-extend (make-Δ Σ) Δ))) ; Extend Δ for state-name.

(defmethod push-state :before ((state-name string) (q₀-instance q₀) &key (start-p nil) &allow-other-keys)
  (with-slots (q₀) q₀-instance
    (when start-p
      (setf q₀ state-name)))) ; If start state then set as start state.

(defmethod push-state :before ((state-name string) (F-instance F) &key (final-p nil) &allow-other-keys)
  (with-slots (F) F-instance
    (if final-p
	(vector-push-extend state-name F) ;   Either it is a final state
	(vector-push-extend nil F))))     ; or it is not - so empty space instanceead.

(defmethod push-state ((state-name string) (finite-automata-instance finite-automata) &key &allow-other-keys)
  (with-slots (Q Δ F) finite-automata-instance
    (values finite-automata-instance                        ; Return mutated finite-automata after all said and done...
	    (1- (and (fill-pointer Q)
		     (fill-pointer Δ)      ; (and quick consistency check)
		     (fill-pointer F)))))) ; ...as well as the state number.

(defmethod push-state-next ((finite-automata-instance finite-automata) &key (start-p nil) (final-p nil) &allow-other-keys)
  (push-state (make-state-name (dsn finite-automata-instance))
		               finite-automata-instance
		               :start-p start-p
		               :final-p final-p))

(defmethod push-transit :before ((state-A integer)
				 (state-B integer)
				 (transit-char character)
				 (Δ-instance Δ)
				 &key &allow-other-keys)
  (with-slots (Δ) Δ-instance
    (let ((Δ.state-A (aref Δ state-A)))
      (push state-B (gethash transit-char Δ.state-A nil)))))

;;; I need to think how to line up these series of methods...
(defmethod push-transit :before ((state-A integer)
				 (state-B integer)
				 (epsilon (eql 'epsilon))
				 (nondeterministic-finite-automata-instance nondeterministic-finite-automata)
				 &key &allow-other-keys)
  (with-slots (Δ) nondeterministic-finite-automata-instance
    (let ((Δ.state-A (aref Δ state-A)))
      (push state-B (gethash epsilon Δ.state-A nil)))))

(defmethod push-transit (state-A
			 state-B
			 transit-char
			 (finite-automata-instance finite-automata)
			 &key &allow-other-keys)
  finite-automata-instance)

(defmethod delete-transit :before ((state-A integer)
				   (state-B integer)
				   transit-char
				   (Δ-instance Δ)
				   &key &allow-other-keys)
  (with-slots (Δ) Δ-instance
    (let* ((Δ.state-A (aref Δ state-A))
           (Δ.state-A.char (gethash transit-char Δ.state-A)))
      (if Δ.state-A.char
	(let ((Δ.state-A.char.no-state-B (delete state-B Δ.state-A.char)))
	  (if Δ.state-A.char.no-state-B
	      (setf (gethash transit-char Δ.state-A) Δ.state-A.char.no-state-B)
	      (remhash transit-char Δ.state-A)))
	(remhash transit-char Δ.state-A)))))

(defmethod delete-transit (state-A
			   state-B
			   transit-char
			   (finite-automata-instance finite-automata)
			   &key &allow-other-keys)
  finite-automata-instance)

(defmethod get-transit ((state integer)
			transit-char
			(Δ-instance Δ)
			&key &allow-other-keys)
  (with-slots (Δ) Δ-instance
    (let* ((Δ.state (aref Δ state))
	   (Δ.state.char (gethash transit-char Δ.state)))
      Δ.state.char)))


;;;   This recursive generic function is my initial way of organizing the problem and will probably
;;; run into space constraints in short order. Also, the goal was to make this not write-only code
;;; so that I could read this at some arbitrary future date.

;; begin-state[transit-char]-->end-state
(defmethod push-fragment ((fragment-type (eql 'regex-literal))
			  (nondeterministic-finite-automata-instance nondeterministic-finite-automata)
			  &key
			    transit-char
			    (begin-state 'next)
			    (end-state 'next)
			  &allow-other-keys)
  (multiple-value-bind (nondeterministic-finite-automata-instance begin-state)
      (push-state begin-state nondeterministic-finite-automata-instance)
    (multiple-value-bind (nondeterministic-finite-automata-instance end-state)
	(push-state end-state nondeterministic-finite-automata-instance)
      (values (push-transit begin-state
			    end-state
			    transit-char
			    nondeterministic-finite-automata-instance)
	      begin-state
	      end-state))))

;; begin-state[epsilon]-->end-state
(defmethod push-fragment ((fragment-type (eql 'regex-epsilon))
			  (nondeterministic-finite-automata-instance nondeterministic-finite-automata)
			  &key
			    (begin-state 'next)
			    (end-state 'next)
			  &allow-other-keys)
  (push-fragment 'regex-literal
		 nondeterministic-finite-automata-instance
		 :transit-char 'epsilon
		 :begin-state begin-state
		 :end-state end-state))

;; begin[epsilon]-->A-in + A-out[epsilon]-->B-in + B-out[epsilon]-->end
(defmethod push-fragment ((fragment-type (eql 'regex-concat))
			  (nondeterministic-finite-automata-instance nondeterministic-finite-automata)
			  &key
			    (begin-state 'next)
			    (end-state 'next)
			    (state-A-in 'next)
			    (state-A-out 'next)
			    (state-B-in 'next)
			    (state-B-out 'next)
			  &allow-other-keys)
  (multiple-value-bind (nondeterministic-finite-automata-instance begin-state state-A-in)
      ;; begin[epsilon]-->A-in
      (push-fragment 'regex-epsilon
		     nondeterministic-finite-automata-instance
		     :begin-state begin-state
		     :end-state state-A-in)
    (multiple-value-bind (nondeterministic-finite-automata-instance state-A-out state-B-in)
	;; A-out[epsilon]-->B-in
	(push-fragment 'regex-epsilon
		       nondeterministic-finite-automata-instance
		       :begin-state state-A-out
		       :end-state state-B-in)
      (multiple-value-bind (nondeterministic-finite-automata-instance state-B-out end-state)
	  ;; B-out[epsilon]-->end
	  (push-fragment 'regex-epsilon
	         	 nondeterministic-finite-automata-instance
			 :begin-state state-B-out
			 :end-state end-state)
	(values nondeterministic-finite-automata-instance begin-state end-state)))))
      
;; begin[epsilon] -->A-in A-out[epsilon]--> ||
;;      ||        -->B-in B-out[epsilon]--> end
(defmethod push-fragment ((fragment-type (eql 'regex-or))
			  (nondeterministic-finite-automata-instance nondeterministic-finite-automata)
			  &key
			    (begin-state 'next)
			    (end-state 'next)
			    (state-A-in 'next)
			    (state-A-out 'next)
			    (state-B-in 'next)
			    (state-B-out 'next)
			  &allow-other-keys)
  (multiple-value-bind (nondeterministic-finite-automata-instance begin-state state-A-in)
      ;; begin[epsilon]-->A-in
      (push-fragment 'regex-epsilon
		     nondeterministic-finite-automata-instance
		     :begin-state begin-state
		     :end-state state-A-in)
    (multiple-value-bind (nondeterministic-finite-automata-instance begin-state state-B-in)
	;; begin[epsilon]-->B-in
	(push-fragment 'regex-epsilon
		       nondeterministic-finite-automata-instance
		       :begin-state begin-state
		       :end-state state-B-in)
      (multiple-value-bind (nondeterministic-finite-automata-instance state-A-out end-state)
	  ;; A-out[epsilon]-->end
	  (push-fragment 'regex-epsilon
			 nondeterministic-finite-automata-instance
			 :begin-state state-A-out
			 :end-state end-state)
	(multiple-value-bind (nondeterministic-finite-automata-instance state-B-out end-state)
	    ;; B-out[epsilon]-->end
	    (push-fragment 'regex-epsilon
			   nondeterministic-finite-automata-instance
			   :begin-state state-B-out
			   :end-state end-state) 
	  (values nondeterministic-finite-automata-instance begin-state end-state))))))

;; begin*[epsilon]-->A-in A-out[epsilon]-->begin*
(defmethod push-fragment ((fragment-type (eql 'regex-star))
			  (nondeterministic-finite-automata-instance nondeterministic-finite-automata)
			  &key
			    (begin-state 'next)
			    (state-A-in 'next)
			    (state-A-out 'next)
			  &allow-other-keys)
  (multiple-value-bind (nondeterministic-finite-automata-instance begin-state state-A-in)
      ;; begin*[epsilon]-->A-in
      (push-fragment 'regex-epsilon
		     nondeterministic-finite-automata-instance
		     :begin-state begin-state
		     :end-state state-A-in)
    (multiple-value-bind (nondeterministic-finite-automata-instance state-A-out begin-state)
	;; A-out[epsilon]-->begin*
	(push-fragment 'regex-epsilon
		       nondeterministic-finite-automata-instance
		       :begin-state state-A-out
		       :end-state begin-state)
      (values nondeterministic-finite-automata-instance begin-state begin-state))))
