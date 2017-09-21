(in-package #:lexer)

(defclass state-names ()
  ((preface :initarg :preface
	    :initform "q_"
	    :reader preface)
   (iterate :initarg :iterate
	    :initform 0
	    :reader iterate)))

(defclass FA ()
  ((regex-tree :initarg :regex-tree
	       :initform nil
	       :reader regex-tree)
   (FA-prev :initarg :FA-prev
	    :initform nil
	    :reader FA-prev)
   (Q-map :initarg :Q-map
	  :initform (make-state-vector 1)
	  :reader Q-map)
   (Q :initarg :Q
      :initform (make-state-vector 1)
      :reader Q)
   (Σ :initarg :Σ
      :initform 'cl-utf
      :reader Σ)
   (Σ-in-use :initarg :Σ-in-use
	     :initform (list)
	     :reader Σ-in-use)
   (Δ :initarg :Δ
      :initform (make-state-vector 1)
      :reader Δ)
   (q₀ :initarg :q₀
       :initform nil
       :reader q₀)
   (q0-prev :initarg :q0-prev
	    :initform (list)
	    :reader q0-prev)
   (F :initarg :F
      :initform (make-state-vector 1)
      :reader F)
   (dsn :initarg dsn
	:initform (make-instance 'state-names)
	:reader dsn)))

(defclass NFA (FA) ())

(defclass DFA (FA) ())
