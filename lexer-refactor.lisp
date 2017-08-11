(in-package #:lexer)

;; https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton
;; An NFA is represented formally by a 5-tuple, (Q, Σ, Δ, q0, F), consisting of
;;   a finite set of states Q
;;   a finite set of input symbols Σ
;;   a transition function Δ : Q × Σ → P(Q).
;;   an initial (or start) state q0 ∈ Q
;;   a set of states F distinguished as accepting (or final) states F ⊆ Q.

(defclass states ()
  ((preface :initarg :preface  :initform "q_"             :reader preface)
   (iterate :initarg :iterate  :initform 0                :reader iterate)))

(defclass FA (states)
  ((Q       :initarg :Q        :initform (make-vector 1)  :reader Q)
   (Sigma   :initarg :Sigma    :initform 'cl-utf          :reader Sigma)
   (Delta   :initarg :Delta    :initform (make-vector 1)  :reader Delta)
   (q_0     :initarg :q_0                                 :reader q_0)
   (F       :initarg :F        :initform (make-vector 1)  :reader F)))

(defun make-vector (size &key (initial-element nil) (adjustable t) (fill-pointer 1))
  (make-array size :initial-element initial-element
		   :adjustable adjustable
		   :fill-pointer fill-pointer))

(defmethod make-Delta ((Sigma (eql 'cl-utf)))
  (make-hash-table :test 'eql)) ;eql for cl char's
	      
(defmethod make-state ((states states))
  (with-slots (preface iterate) states
    (format nil "~a~d" preface (1- (incf iterate)))))

(defmethod initialize-instance :after ((FA FA) &key &allow-other-keys)
  (push-state 'next FA :q_0-p t))

(defmethod push-state (state (FA FA) &key (q_0-p nil) (F-p nil))
  (with-slots (Q Sigma Delta q_0 F) FA
    (vector-push-extend state Q)
    (vector-push-extend (make-Delta Sigma) Delta)
    (when q_0-p
      (setf q_0 state))
    (if F-p
	(vector-push-extend state F)
	(vector-push-extend nil F))))

(defmethod push-state ((state (eql 'next)) (FA FA) &key (q_0-p nil) (F-p nil))
  (push-state (make-state FA) FA  :q_0-p q_0-p  :F-p F-p))


;; testing out some thangs


(defclass class-1 ()
  ((slot-1 :initarg :slot-1
	   :initform (list 'init-slot-1)
	   :accessor slot-1)))

(defclass class-2 (class-1)
  ((slot-2 :initarg :slot-2
	   :initform (list 'init-slot-2)
	   :accessor slot-2)))


(defgeneric func-1 (thing-1))

(defmethod func-1 :around ((thing-1 class-1))
  (with-slots (slot-1) thing-1
    (push 'func-1-around slot-1))
  (call-next-method))

(defmethod func-1 :before ((thing-1 class-1))
  (with-slots (slot-1) thing-1
    (push 'func-1-before slot-1)))

(defmethod func-1 ((thing-1 class-1))
  (with-slots (slot-1) thing-1
    (push 'func-1-prime slot-1)))

(defmethod func-1 :after ((thing-1 class-1))
  (with-slots (slot-1) thing-1
    (push 'func-1-after slot-1)))


(defgeneric func-2 (thing-1 thing-2))

(defmethod func-2 :around (thing-1 (thing-2 class-2))
  (with-slots (slot-1 slot-2) thing-2
    (push 'func-2-around slot-1)
    (push 'func-2-around slot-2))
  (call-next-method))

(defmethod func-2 :before (thing-1 (thing-2 class-2))
  (with-slots (slot-1 slot-2) thing-2
    (push 'func-2-before slot-1)
    (push 'func-2-before slot-2)))

(defmethod func-2 (thing-1 (thing-2 class-2))
  (with-slots (slot-1 slot-2) thing-2
    (push 'func-2-prime slot-1)
    (push 'func-2-prime slot-2)))

(defmethod func-2 :after (thing-1 (thing-2 class-2))
  (with-slots (slot-1 slot-2) thing-2
    (push 'func-2-after slot-1)
    (push 'func-2-after slot-2)))

(defmethod func-2 ((thing-1 (eql 'blah)) (thing-2 class-2))
  (with-slots (slot-1 slot-2) thing-2
    (push 'func-2-prime-blah slot-1)
    (push 'func-2-prime-blah slot-2))
  (call-next-method))


(defgeneric func-3 (thing))

(defmethod func-3 :around ((thing-1 class-1))
  (with-slots (slot-1) thing-1
    (push 'func-3-around slot-1))
  (call-next-method))

(defmethod func-3 :before ((thing-1 class-1))
  (with-slots (slot-1) thing-1
    (push 'func-3-before slot-1)))

(defmethod func-3 ((thing-1 class-1))
  (with-slots (slot-1) thing-1
    (push 'func-3-prime slot-1)))

(defmethod func-3 :after ((thing-1 class-1))
  (with-slots (slot-1) thing-1
    (push 'func-3-after slot-1)))

(defmethod func-3 :around ((thing-2 class-2))
  (with-slots (slot-1 slot-2) thing-2
    (push 'func-3-around-2 slot-1)
    (push 'func-3-around-2 slot-2))
  (call-next-method))

(defmethod func-3 :before ((thing-2 class-2))
  (with-slots (slot-1 slot-2) thing-2
    (push 'func-3-before-2 slot-1)
    (push 'func-3-before-2 slot-2)))

(defmethod func-3 ((thing-2 class-2))
  (with-slots (slot-1 slot-2) thing-2
    (push 'func-3-prime-2 slot-1)
    (push 'func-3-prime-2 slot-2)))

(defmethod func-3 :after ((thing-2 class-2))
  (with-slots (slot-1 slot-2) thing-2
    (push 'func-3-after-2 slot-1)
    (push 'func-3-after-2 slot-2)))


(defun test-func-1 ()
  (let ((thing-class-1 (make-instance 'class-1)))
    (func-1 thing-class-1)
    (slot-1 thing-class-1)))

(defun test-func-2 ()
  (let ((thing-class-1 (make-instance 'class-1))
	(thing-class-2 (make-instance 'class-2)))
    (func-2 thing-class-1 thing-class-2)
    (list (slot-1 thing-class-1)
	  (slot-2 thing-class-2))))

(defun test-func-3 ()
  (let ((thing-class-1 (make-instance 'class-1))
	(thing-class-2 (make-instance 'class-2)))
    (func-3 thing-class-1)
    (func-3 thing-class-2)
    (list (list 'thing-class-1 (slot-1 thing-class-1))
	  (list 'thing-class-2 (slot-1 thing-class-2) (slot-2 thing-class-2)))))
