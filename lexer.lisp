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
      :initform (make-vector 1)
      :reader Q
      :documentation "A finite set of states Q.")))

(defclass Sigma ()
  ((Sigma :initarg :Sigma
	  :initform 'cl-utf
	  :reader Sigma
	  :documentation "A finite set of input symbols Σ.")))

(defclass Delta (Q Sigma)
  ((Delta :initarg :Delta
	  :initform (make-vector 1)
	  :reader Delta
	  :documentation "A transition function Δ : Q × Σ → P(Q).")))

(defclass q_0 (Q)
  ((q_0 :initarg :q_0
	:reader q_0
	:documentation "An initial (or start) state q0 ∈ Q.")))

(defclass F (Q)
  ((F :initarg :F
      :initform (make-vector 1)
      :reader F
      :documentation "A set of states F distinguished as accepting (or final) states F ⊆ Q.")))

(defclass FA (Q Sigma Delta q_0 F)
  ((DSN :initarg DSN
	:initform (make-instance 'states-names)
	:reader DSN
	:documentation "Default States Names."))
  (:documentation "An NFA is represented formally by a 5-tuple, (Q, Σ, Δ, q0, F)."))

(defun make-vector (size &key (initial-element nil) (adjustable t) (fill-pointer 0))
  (make-array size
	      :initial-element initial-element
	      :adjustable adjustable
	      :fill-pointer fill-pointer))

(defgeneric make-Delta (Sigma-type)
  (:documentation "Create respective character-transition map data-structure."))

(defmethod make-Delta ((Sigma-type (eql 'cl-utf)))
  (make-hash-table :test 'eql)) ;eql for cl char's

(defgeneric make-state (states-names)
  (:documentation "Name a new state from a reference class instance for a series of states."))
	      
(defmethod make-state ((states-names states-names))
  (with-slots (preface iterate) states-names
    (format nil "~a~d" preface (1- (incf iterate)))))

(defmethod initialize-instance :after ((FA FA) &key &allow-other-keys)
  (push-state (make-state (DSN FA)) FA :start-p t))

(defgeneric push-state (state finite-automaton &key &allow-other-keys)
  (:documentation "Push a state into the finite automaton."))

(defmethod push-state :before (state (Q Q) &key &allow-other-keys)
  (with-slots (Q) Q
    (vector-push-extend state Q)))

(defmethod push-state :before (state (Delta Delta) &key &allow-other-keys)
  (with-slots (Sigma Delta) Delta
    (vector-push-extend (make-Delta Sigma) Delta)))

(defmethod push-state :before (state (q_0 q_0) &key (start-p nil) &allow-other-keys)
  (with-slots (q_0) q_0
    (when start-p
      (setf q_0 state))))

(defmethod push-state :before (state (F F) &key (final-p nil) &allow-other-keys)
  (with-slots (F) F
    (if final-p
	(vector-push-extend state F)
	(vector-push-extend nil F))))

(defmethod push-state (state (FA FA) &key &allow-other-keys)
  FA)

(defun print-FA (FA)
  (print (Q FA))
  (print (Sigma FA))
  (print (Delta FA))
  (print (q_0 FA))
  (print (F FA))
  nil)

(defclass NFA (FA) ())

;(defgeneric set-transit (state-A
