;;;; lexer.lisp
;;;;
;;;; Copyright (C) 2017 Christopher H Cope
;;;; All rights reserved.
;;;;
;;;; This software may be modified and distributed under the terms
;;;; of the BSD license.  See the LICENSE file for details.

(in-package #:lexer)

;;; Generic Function Prototypes
(defgeneric make-state-vector (size &key &allow-other-keys)
  (:documentation "Return the default chosen array type for states."))

(defgeneric make-state-name (state-names)
  (:documentation "Name a new state from a reference class instance for a series of states."))

(defgeneric make-Δ (Σ-type)
  (:documentation "Create respective character-transition map data-structure."))

(defgeneric push-state (state finite-automaton &key &allow-other-keys)
  (:documentation "Push a state into the finite automaton."))

(defgeneric push-transit (state-A state-B transit-char FA)
  (:documentation "Push a single transition on transit-char from state-A to state-B in FA."))

(defgeneric delete-transit (state-A state-B transit-char FA)
  (:documentation "Remove a transition on transit-char from state-A to state-B in FA."))

(defgeneric get-transit (state transit-char Δ)
  (:documentation "Get list of states to transition to on transition character."))

(defgeneric push-next-states (states-in-tree FA))

;;; Method Definitions
(defmethod make-state-vector ((size integer) &key
					       (initial-element nil)
					       (adjustable t)
					       (fill-pointer 0)
			      &allow-other-keys)
  (make-array size
	      :initial-element initial-element
	      :adjustable adjustable
	      :fill-pointer fill-pointer))

;;   The transitions are a hashtable that associates the literal next state
;; object with a character.
(defmethod make-Δ ((Σ-type (eql 'cl-utf)))
  (make-hash-table :test 'eql)) ;eql for cl char's

;;   To initialize a state, we name the state using the name-preface
;; and the iterate.
(defmethod make-state-name ((state-names-instance state-names))
  (with-slots (preface iterate) state-names-instance
    (format nil "~a~d" preface (1- (incf iterate)))))

(defmethod push-state ((next (eql 'next))
		       (FA-instance FA)
		       &key
			 (start-p nil)
			 (final-p nil)
		       &allow-other-keys)
  (push-state (make-state-name (dsn FA-instance))
	      FA-instance
	      :start-p start-p
	      :final-p final-p))

(defmethod push-state ((state integer)
		       (FA-inst FA)
		       &key &allow-other-keys)
  (values FA-inst (when (<= state (fill-pointer (slot-value FA-inst 'Q)))
		    state)))
			  
;; All the :before methods I use to organize the assigns for the single push-state operation.
(defmethod push-state :before ((state-name string)
			       (Q-inst Q)
			       &key &allow-other-keys)
  (vector-push-extend state-name (slot-value Q-inst 'Q)))
					; Push our state-name onto our vector of state names.

(defmethod push-state :before ((state-name string)
			       (Δ-inst Δ)
			       &key &allow-other-keys)
  (with-slots (Σ Δ) Δ-inst
    (vector-push-extend (make-Δ Σ) Δ))) ; Extend Δ for state-name.

(defmethod push-state :before ((state-name string)
			       (q₀-inst q₀)
			       &key
				 (start-p nil)
			       &allow-other-keys)
  (when start-p
    (setf (slot-value q₀-inst 'q₀-name) state-name))) ; If start state then set as start state.

(defmethod push-state :after ((state-name string)
			      (FA-instance FA)
			      &key
				(start-p nil)
			      &allow-other-keys)
  (with-slots (q₀ Δ) FA-instance
    (when start-p
      (setf q₀ (aref Δ (1- (fill-pointer Δ)))))))

(defmethod push-state :before ((state-name string)
			       (F-instance F)
			       &key
				 (final-p nil)
			       &allow-other-keys)
  (with-slots (F) F-instance
    (if final-p
	(vector-push-extend state-name F) ;   Either it is a final state
	(vector-push-extend nil F))))     ; or it is not - so empty space pushed.

(defmethod push-state ((state-name string)
		       (FA-instance FA)
		       &key &allow-other-keys)
  (with-slots (Q Δ F) FA-instance
    (values FA-instance                    ; Return mutated FA after all said and done...
	    (1- (and (fill-pointer Q)
		     (fill-pointer Δ)      ; (and quick consistency check)
		     (fill-pointer F)))))) ; ...as well as the state number.

(defmethod push-state :before ((states-list list)
			       (Q-inst Q)
			       &key &allow-other-keys)
  (vector-push-extend states-list (slot-value Q-inst 'Q)))

(defmethod push-state ((states-list list)
		       (Q-inst Q)
		       &key &allow-other-keys)
  Q-inst)

(defmethod push-next-states ((states-tree list)
		           (FA-instance FA))
  (cons (push-next-states (car states-tree) FA-instance)
        (push-next-states (cdr states-tree) FA-instance)))

(defmethod push-next-states ((next (eql 'next))
			   (FA-instance FA))
  (multiple-value-bind (FA-instance new-state)
      (push-state 'next FA-instance)
    new-state))

(defmethod push-next-states ((next (eql nil))
			   (FA-instance FA))
  nil)

(defmethod push-next-states (next
			   (FA-instance FA))
  next)

(defmethod push-transit :before ((state-A integer)
				 (state-B integer)
				 (transit-char character)
				 (Δ-inst Δ))
  (push state-B	(gethash transit-char
			 (aref (slot-value Δ-inst 'Δ)
			       state-A)
			 nil)))

(defmethod push-transit :before ((state-A integer)
				 (state-B integer)
				 (ε (eql 'ε))
				 (NFA-inst NFA))
  (push state-B (gethash ε
			 (aref (slot-value NFA-inst 'Δ)
			       state-A)
			 nil)))

(defmethod push-transit :before ((state-A integer)
				 (state-B integer)
				 (transit-char character)
				 (Σ-inst Σ))
  (pushnew transit-char (slot-value Σ-inst 'Σ-in-use)))

(defmethod push-transit (state-A
			 state-B
			 transit-char
			 (FA-instance FA))
  FA-instance)

;; delete-transit used during testing...
(defmethod delete-transit :before ((state-A integer)
				   (state-B integer)
				   transit-char
				   (Δ-instance Δ))
  (with-slots (Δ) Δ-instance
    (let* ((Δ.state-A (aref Δ state-A))
           (Δ.state-A.transit-char.states (gethash transit-char Δ.state-A)))
      (if Δ.state-A.transit-char.states
	(let ((Δ.state-A.transit-char.states-no-B (delete state-B Δ.state-A.transit-char.states)))
	  (if Δ.state-A.transit-char.states-no-B
	      (setf (gethash transit-char Δ.state-A) Δ.state-A.transit-char.states-no-B)
	      (remhash transit-char Δ.state-A)))
	(remhash transit-char Δ.state-A)))))

(defmethod delete-transit (state-A
			   state-B
			   transit-char
			   (FA-instance FA))
  FA-instance)

(defun get-transit-2 (state transit-char Δ-inst)
  (gethash transit-char (aref (slot-value Δ-inst 'Δ) state)))

(defmethod get-transit ((state integer)
			(transit-char character)
			(Δ-inst Δ))
  (get-transit-2 state transit-char Δ-inst))

;; It turns out getting all the transitions on a list isn't useful.
(defmethod get-transit ((states-in list)
			(transit-char character)
			(Δ-inst Δ))
  (let ((states-out (list)))
    (dolist (state states-in states-out)
      (setf states-out (nunion (get-transit state transit-char Δ-inst)
			       states-out)))))

;; ==> (state-integer-a state-integer-b ... state-integer-n)
(defmethod ε-closure ((state integer)
		      (NFA-inst NFA))
  (ε-closure-2 (list state) (list) NFA-inst))

(defun ε-closure-2 (states-in states-out Δ-inst &key (transit-char 'ε))
  (let ((state (pop states-in)))
    (if state
	(if (member state states-out)
	    (ε-closure-2 states-in states-out Δ-inst :transit-char transit-char)
	    (ε-closure-2 (append states-in (get-transit-2 state transit-char Δ-inst))
			 (push state states-out)
			 Δ-inst
			 :transit-char transit-char))
	states-out)))

(defmethod get-state ((state-name string)
		      (Q-inst Q))
  (find-name-iter state-name Q-inst 0))

(defmethod get-state ((state integer)
		      (Q-inst Q))
  (aref (Q Q-inst) state))

(defun find-name-iter (state-name Q-inst cell-iter)
  (if (< cell-iter (fill-pointer (Q Q-inst)))
      (if (equal state-name
		 (aref (Q Q-inst) cell-iter))
	  cell-iter
	  (find-name-iter state-name Q-inst (1+ cell-iter)))))

(defmethod get-Δ ((state integer)
		  (Δ-inst Δ))
  (aref (Δ Δ-inst) state))
