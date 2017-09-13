(in-package #:lexer)

(defclass state-names ()
  ((preface :initarg :preface
	    :initform "q_"
	    :reader preface)
   (iterate :initarg :iterate
	    :initform 0
	    :reader iterate)))

(defclass FA ()
  ((Q :initarg :Q
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
       :reader q₀)
   (F :initarg :F
      :initform (make-state-vector 1)
      :reader F)
   (dsn :initarg dsn
	:initform (make-instance 'state-names)
	:reader dsn)))

(defclass NFA (FA) ())

(defclass DFA (FA) ())
