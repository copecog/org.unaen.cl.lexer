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
(defun make-states-vector (size &key (initial-element nil) (adjustable t) (fill-pointer 0))
  (make-array size
	      :initial-element initial-element
	      :adjustable adjustable
	      :fill-pointer fill-pointer))


;;; Generic Function Prototypes
(defgeneric make-Delta (Sigma-type)
  (:documentation "Create respective character-transition map data-structure."))

(defgeneric make-state-name (states-names)
  (:documentation "Name a new state from a reference class instance for a series of states."))

(defgeneric push-state (state-name finite-automaton &key &allow-other-keys)
  (:documentation "Push a state into the finite automaton."))

(defgeneric push-state-next (finite-automata &key &allow-other-keys)
  (:documentation "Push the next implied by states-names under FA."))

;; The notion of pushing it into the object rather than onto a stack.
(defgeneric push-transit (state-A state-B transit-char FA &key &allow-other-keys)
  (:documentation "Push a single transition on transit-char from state-A to state-B in FA."))

(defgeneric delete-transit (state-A state-B transit-char FA &key &allow-other-keys)
  (:documentation "Remove a transition on transit-char from state-A to state-B in FA."))

(defgeneric get-transit (state transit-char FA &key &allow-other-keys)
  (:documentation "Get list of states to transition to on transition character."))

(defgeneric push-fragment (fragment-type FA &key &allow-other-keys)
  (:documentation "Push new finite state automaton fragment onto FA by type."))


;;; Class Definitions

;;   There are several classes of states across varying classes of finite
;; automata as well as transitional sets of states while functionally mapping
;; from one automaton to another.
;;   This scheme with a preface and iterate allows us to have systematic naming
;; that is still flexible.
(defclass states-names ()
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
      :initform (make-states-vector 1)
      :reader Q
      :documentation "A finite set of states Q.")))

;;   The set Σ of input symbols.  Whatever the input symbols are, they need
;; enumeration--however, we are going to use the built in character encoding in
;; lisp to give us that mapping (signified by a 'cl-utf slot-value).
(defclass Sigma ()
  ((Sigma :initarg :Sigma
	  :initform 'cl-utf
	  :reader Sigma
	  :documentation "A finite set of input symbols Σ.")))

;;   A transition function Δ : Q × Σ → P(Q). Using the enumeration from Q we
;; store each state object under an array, and each state contains the
;; transitions on symbols from Σ, which gives us our function map.
(defclass Delta ()
  ((Delta :initarg :Delta
	  :initform (make-states-vector 1)
	  :reader Delta
	  :documentation "A transition function Δ : Q × Σ → P(Q).")))

;;   The initial or starting state of the finite automata. Usually named as
;; q_0 ∈ Q in the mathematical definition.
(defclass q_0 ()
  ((q_0 :initarg :q_0
	:reader q_0
	:documentation "An initial (or start) state q_0 ∈ Q.")))

;;   The set F of final or accepting states. F is an array of the same size as Q
;; with the accepting states under the same respective indices as under Q.
(defclass F ()
  ((F :initarg :F
      :initform (make-states-vector 1)
      :reader F
      :documentation "A set of states F distinguished as accepting (or final) states F ⊆ Q.")))

(defclass FA (Q Sigma Delta q_0 F)
  ((DSN :initarg DSN
	:initform (make-instance 'states-names)
	:reader DSN
	:documentation "_D_efault _S_tates _N_ames."))
  (:documentation "An FA is represented formally by a 5-tuple, (Q, Σ, Δ, q0, F)."))

;; https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton
(defclass NFA (FA) ()
  (:documentation "Non-deterministic Finite Automata."))


;;; Method Definitions
;;   By default the first state name generated by the name-preface.
(defmethod initialize-instance :after ((FA-inst FA) &key &allow-other-keys)
  (push-state 'next FA-inst :start-p t))

;;   The transitions are a hashtable that associates the literal next state
;; object with a character.
(defmethod make-Delta ((Sigma-type (eql 'cl-utf)))
  (make-hash-table :test 'eql)) ;eql for cl char's

;;   To initialize a state, we name the state using the name-preface
;; and the iterate.
(defmethod make-state-name ((states-names-inst states-names))
  (with-slots (preface iterate) states-names-inst
    (format nil "~a~d" preface (1- (incf iterate)))))

(defmethod push-state :around ((next (eql 'next)) (FA-inst FA) &key (start-p nil) (final-p nil)
			       &allow-other-keys)
  (call-next-method (make-state-name (DSN FA-inst))
		    FA-inst
		    :start-p start-p
		    :final-p final-p))

;; All the :before methods I use to organize the assigns for the single push-state operation.

(defmethod push-state :before (state-name (Q-inst Q) &key &allow-other-keys)
  (with-slots (Q) Q-inst
    (vector-push-extend state-name Q))) ; Push our state-name onto our vector of state names.

(defmethod push-state :before (state-name (Delta-inst Delta) &key &allow-other-keys)
  (with-slots (Sigma Delta) Delta-inst
    (vector-push-extend (make-Delta Sigma) Delta))) ; Extend Delta for state-name.

(defmethod push-state :before (state-name (q_0-inst q_0) &key (start-p nil) &allow-other-keys)
  (with-slots (q_0) q_0-inst
    (when start-p
      (setf q_0 state-name)))) ; If start state then set as start state.

(defmethod push-state :before (state-name (F-inst F) &key (final-p nil) &allow-other-keys)
  (with-slots (F) F-inst
    (if final-p
	(vector-push-extend state-name F) ;   Either it is a final state
	(vector-push-extend nil F))))     ; or it is not - so empty space instead.

(defmethod push-state (state (FA-inst FA) &key &allow-other-keys)
  (with-slots (Q Delta F) FA-inst
    (values FA-inst                   ; Return mutated FA after all said and done
	    (and (fill-pointer Q)
		 (fill-pointer Delta)
		 (fill-pointer F))))) ; and the next state (or current number of states).

(defmethod push-state-next ((FA-inst FA) &key (start-p nil) (final-p nil) &allow-other-keys)
  (push-state (make-state-name (DSN FA-inst))
		               FA-inst
		               :start-p start-p
		               :final-p final-p))

(defmethod push-transit :before ((state-A integer)
				 (state-B integer)
				 (transit-char character)
				 (Delta-inst Delta)
				 &key &allow-other-keys)
  (with-slots (Delta) Delta-inst
    (let ((Delta.state-A (aref Delta state-A)))
      (push state-B (gethash transit-char Delta.state-A nil)))))

;;; I need to think how to line up these series of methods...
(defmethod push-transit :before ((state-A integer)
				 (state-B integer)
				 (epsilon (eql 'epsilon))
				 (NFA-inst NFA)
				 &key &allow-other-keys)
  (with-slots (Delta) NFA-inst
    (let ((Delta.state-A (aref Delta state-A)))
      (push state-B (gethash epsilon Delta.state-A nil)))))

(defmethod push-transit (state-A
			 state-B
			 transit-char
			 (FA-inst FA)
			 &key &allow-other-keys)
  FA-inst)

(defmethod delete-transit :before ((state-A integer)
				   (state-B integer)
				   transit-char
				   (Delta-inst Delta)
				   &key &allow-other-keys)
  (with-slots (Delta) Delta-inst
    (let* ((Delta.state-A (aref Delta state-A))
           (Delta.state-A.char (gethash transit-char Delta.state-A)))
      (if Delta.state-A.char
	(let ((Delta.state-A.char.no-state-B (delete state-B Delta.state-A.char)))
	  (if Delta.state-A.char.no-state-B
	      (setf (gethash transit-char Delta.state-A) Delta.state-A.char.no-state-B)
	      (remhash transit-char Delta.state-A)))
	(remhash transit-char Delta.state-A)))))

(defmethod delete-transit (state-A
			   state-B
			   transit-char
			   (FA-inst FA)
			   &key &allow-other-keys)
  FA-inst)

(defmethod get-transit ((state integer)
			transit-char
			(Delta-inst Delta)
			&key &allow-other-keys)
  (with-slots (Delta) Delta-inst
    (let* ((Delta.state (aref Delta state))
	   (Delta.state.char (gethash transit-char Delta.state)))
      Delta.state.char)))


;;;   This recursive generic function is my initial way of organizing the problem and will probably
;;; run into space constraints in short order. Also, the goal was to make this not write-only code
;;; so that I could read this at some arbitrary future date.

;; begin-state[transit-char]-->end-state
(defmethod push-fragment ((fragment-type (eql 'regex-literal))
			  (NFA-inst NFA)
			  &key
			    transit-char
			    (begin-state 'next)
			    (end-state 'next)
			  &allow-other-keys)
  (multiple-value-bind (|NFA+begin| pushed-begin)
      (push-state begin-state NFA-inst)
    (multiple-value-bind (|NFA+begin+end| pushed-end)
	(push-state end-state |NFA+begin|)
      (values (push-transit pushed-begin pushed-end transit-char |NFA+begin+end|)
	      pushed-begin
	      pushed-end))))

;; begin-state[epsilon]-->end-state
(defmethod push-fragment ((fragment-type (eql 'regex-epsilon))
			  (NFA-inst NFA)
			  &key
			    (begin-state 'next)
			    (end-state 'next)
			  &allow-other-keys)
  (push-fragment 'regex-literal
		 NFA-inst
		 :transit-char 'epsilon
		 :begin-state begin-state
		 :end-state end-state))

;; begin[epsilon]-->A-in + A-out[epsilon]-->B-in + B-out[epsilon]-->end
(defmethod push-fragment ((fragment-type (eql 'regex-concat))
			  (NFA-inst NFA)
			  &key
			    (begin-state 'next)
			    (end-state 'next)
			    (state-A-in 'next)
			    (state-A-out 'next)
			    (state-B-in 'next)
			    (state-B-out 'next)
			  &allow-other-keys)
  (multiple-value-bind (|NFA+begin+A-in| pushed-begin pushed-A-in)
      ;; begin[epsilon]-->A-in
      (push-fragment 'regex-epsilon
		     NFA-inst
		     :begin-state begin-state
		     :end-state state-A-in)
    (multiple-value-bind (|NFA+begin+A-in+A-out+B-in| pushed-A-out pushed-B-in)
	;; A-out[epsilon]-->B-in
	(push-fragment 'regex-epsilon
		       |NFA+begin+A-in|
		       :begin-state state-A-out
		       :end-state state-B-in)
      (multiple-value-bind (|NFA+begin+A-in+A-out+B-in+B-out+end| pushed-B-out pushed-end)
	  ;; B-out[epsilon]-->end
	  (push-fragment 'regex-epsilon
	         	 |NFA+begin+A-in+A-out+B-in|
			 :begin-state state-B-out
			 :end-state end-state)
	(values |NFA+begin+A-in+A-out+B-in+B-out+end| pushed-begin pushed-end)))))
      
;; begin[epsilon] -->A-in A-out[epsilon]--> ||
;;      ||        -->B-in B-out[epsilon]--> end
(defmethod push-fragment ((fragment-type (eql 'regex-or))
			  (NFA-inst NFA)
			  &key
			    (begin-state 'next)
			    (end-state 'next)
			    (state-A-in 'next)
			    (state-A-out 'next)
			    (state-B-in 'next)
			    (state-B-out 'next)
			  &allow-other-keys)
  (multiple-value-bind (|NFA+begin+A-in| pushed-begin pushed-A-in)
      ;; begin[epsilon]-->A-in
      (push-fragment 'regex-epsilon
		     NFA-inst
		     :begin-state begin-state
		     :end-state state-A-in)
    (multiple-value-bind (|NFA+begin+A-in+B-in| pushed-begin-2 pushed-B-in)
	;; begin[epsilon]-->B-in
	(push-fragment 'regex-epsilon
		       |NFA+begin+A-in|
		       :begin-state pushed-begin
		       :end-state state-B-in)
      (multiple-value-bind (|NFA+begin+A-in+B-in+A-out+end| pushed-A-out pushed-end)
	  ;; A-out[epsilon]-->end
	  (push-fragment 'regex-epsilon
			 |NFA+begin+A-in+B-in|
			 :begin-state state-A-out
			 :end-state end-state)
	(multiple-value-bind (|NFA+begin+A-in+B-in+A-out+end+B-out| pushed-B-out pushed-end-2)
	    ;; B-out[epsilon]-->end
	    (push-fragment 'regex-epsilon
			   |NFA+begin+A-in+B-in+A-out+end|
			   :begin-state state-B-out
			   :end-state pushed-end) 
	  (values |NFA+begin+A-in+B-in+A-out+end+B-out| pushed-begin-2 pushed-end-2))))))

;; begin*[epsilon]-->A-in A-out[epsilon]-->begin*
(defmethod push-fragment ((fragment-type (eql 'regex-star))
			  (NFA-inst NFA)
			  &key
			    (begin-state 'next)
			    (state-A-in 'next)
			    (state-A-out 'next)
			  &allow-other-keys)
  (multiple-value-bind (|NFA+begin+A-in| pushed-begin pushed-A-in)
      ;; begin*[epsilon]-->A-in
      (push-fragment 'regex-epsilon
		     NFA-inst
		     :begin-state begin-state
		     :end-state state-A-in)
    (multiple-value-bind (|NFA+begin+A-in+A-out| pushed-A-out pushed-begin-2)
	;; A-out[epsilon]-->begin*
	(push-fragment 'regex-epsilon
		       |NFA+begin+A-in|
		       :begin-state pushed-A-out
		       :end-state pushed-begin-2)
      (values |NFA+begin+A-in+A-out| pushed-begin-2 pushed-begin-2))))
		       
		       
		       
		       

