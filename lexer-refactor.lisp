(in-package #:lexer)

;; https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton
;; An NFA is represented formally by a 5-tuple, (Q, Σ, Δ, q0, F), consisting of
;;   a finite set of states Q
;;   a finite set of input symbols Σ
;;   a transition function Δ : Q × Σ → P(Q).
;;   an initial (or start) state q0 ∈ Q
;;   a set of states F distinguished as accepting (or final) states F ⊆ Q.

(defclass states ()
  ((preface :initarg :preface
	    :initform "q_"
	    :reader preface)
   (iterate :initarg :iterate
	    :initform 0
	    :reader iterate)))

(defclass FA (states)
  ((Q :initarg :Q
      :initform (make-vector 1)
      :reader Q)
   (Sigma :initarg :Sigma
	  :initform 'cl-utf
	  :reader Sigma)
   (Delta :initarg :Delta
	  :initform (make-vector 1)
	  :reader Delta)
   (q_0 :initarg :q_0
        ;:initform (format nil "~a~d" preface iterate)
        :reader q_0)
   (F :initarg :F
      :initform (make-vector 1)
      :reader F)))

(defun make-vector (size &key (initial-element nil) (adjustable t)
			   (fill-pointer 1))
  (make-array size :initial-element initial-element
		   :adjustable adjustable
		   :fill-pointer fill-pointer))

(defmethod make-Delta ((Sigma (eql 'cl-utf)))
  (make-hash-table :test 'eql))
	      
(defmethod make-state ((states states))
  (with-slots (preface iterate) states
    (format nil "~a~d" preface (1- (incf iterate)))))

(defmethod initialize-instance :after ((FA FA) &key &allow-other-keys)
  (push-state 'next FA :q_0-p t))

(defmethod push-state (state (FA FA) &key (q_0-p nil) (F-p nil))
  (with-slots (Q Sigma Delta q_0 F) FA
    (vector-push-extend state Q)
    (vector-push-extend (make-Delta Sigma) Delta)
    (when q_0-p (setf q_0 state))
    (if F-p
	(vector-push-extend state F)
	(vector-push-extend nil F))))

(defmethod push-state ((state (eql 'next)) (FA FA) &key (q_0-p nil) (F-p nil))
  (push-state (make-state FA) FA :q_0-p q_0-p :F-p F-p))
