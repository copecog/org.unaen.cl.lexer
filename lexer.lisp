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
(defclass states-names ()
  ((preface :initarg :preface
	    :initform "q_"
	    :reader preface)
   (iterate :initarg :iterate
	    :initform 0
	    :reader iterate))
  (:documentation "Reference to create a series of related states."))

(defclass Q ()
  ((Q :initarg :Q
      :initform (make-states-vector 1)
      :reader Q
      :documentation "A finite set of states Q.")))

(defclass Sigma ()
  ((Sigma :initarg :Sigma
	  :initform 'cl-utf
	  :reader Sigma
	  :documentation "A finite set of input symbols Σ.")))

(defclass Delta ()
  ((Delta :initarg :Delta
	  :initform (make-states-vector 1)
	  :reader Delta
	  :documentation "A transition function Δ : Q × Σ → P(Q).")))

(defclass q_0 ()
  ((q_0 :initarg :q_0
	:reader q_0
	:documentation "An initial (or start) state q_0 ∈ Q.")))

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
(defmethod initialize-instance :after ((FA-inst FA) &key &allow-other-keys)
  (push-state 'next FA-inst :start-p t))

(defmethod make-Delta ((Sigma-type (eql 'cl-utf)))
  (make-hash-table :test 'eql)) ;eql for cl char's

(defmethod make-state-name ((states-names-inst states-names))
  (with-slots (preface iterate) states-names-inst
    (format nil "~a~d" preface (1- (incf iterate)))))

(defmethod push-state :around ((next (eql 'next)) (FA-inst FA) &key (start-p nil) (final-p nil)
			       &allow-other-keys)
  (call-next-method (make-state-name (DSN FA-inst))
		    FA-inst
		    :start-p start-p
		    :final-p final-p))

;; All the :before methods I use to organize the set's for the single push-state operation.
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

;; [begin-state]--transit-char-->[end-state]
(defmethod push-fragment ((fragment-type (eql 'regex-literal))
			  (NFA-inst NFA)
			  &key
			    transit-char
			    (begin-state 'next)
			    (end-state 'next)
			  &allow-other-keys)
  (multiple-value-bind (FA+begin pushed-begin)
      (push-state begin-state NFA-inst)
    (multiple-value-bind (FA+begin+end pushed-end)
	(push-state end-state FA+begin)
      (values (push-transit pushed-begin pushed-end transit-char FA+begin+end)
	      pushed-begin
	      pushed-end))))

;; [begin-state]--epsilon-->[end-state]
(defmethod push-fragment :around ((fragment-type (eql 'regex-epsilon))
			          (NFA-inst NFA)
			          &key
			            (begin-state 'next)
			            (end-state 'next)
				  &allow-other-keys)
  (call-next-method 'regex-literal
		    NFA-inst
		    :transit-char 'epsilon
		    :begin-state begin-state
		    :end-state end-state))

;; [begin]--epsilon-->[A-in] [A-out]--epsilon-->[B-in] [B-out]--epsilon-->[end]
(defmethod push-fragment ((fragment-type (eql 'regex-concat))
			  (NFA-inst NFA)
			  &key
			    (begin-state 'next)
			    (end-state 'next)
			    (state-A-in 'next)
			    (state-A-out 'next)
			    (state-B-in 'next)
			    (state-B-out 'next) &allow-other-keys)
  (multiple-value-bind (FA+begin pushed-begin)
      (push-state begin-state NFA-inst)
    (multiple-value-bind (FA+begin+A-in pushed-A-in)
	(push-state state-A-in FA+begin)
     (push-transit  ))))
      
;;         --epsilon-->[A-in] [A-out]--epsilon-->
;; [begin]                                        [end]
;;         --epsilon-->[B-in] [B-out]--epsilon-->
(defmethod push-fragment ((fragment-type (eql 'regex-or))
			  (NFA-inst NFA)
			  &key
			    (begin-state 'next)
			    (end-state 'next)
			    (state-A-in 'next)
			    (state-A-out 'next)
			    (state-B-in 'next)
			    (state-B-out 'next) &allow-other-keys)
  nil)

;; [begin]--epsilon-->[A-in] [A-out]--epsilon-->[begin]
(defmethod push-fragment ((fragment-type (eql 'regex-star))
			  (NFA-inst NFA)
			  &key
			    (begin-state 'next)
			    (state-A-in 'next)
			    (state-A-out 'next) &allow-other-keys)
  nil)

