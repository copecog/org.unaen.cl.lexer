;;;; generics.lisp

(in-package #:org.unaen.cl.lexer)

;;; fa.lisp
(defgeneric make-state-vector (size &key &allow-other-keys))

(defgeneric make-Δ (Σ-type))

(defgeneric make-state-name (state-names))

(defgeneric push-state-2 (state FA-inst Δ-p start-p final-p))

(defgeneric push-state (state FA-inst &key &allow-other-keys))

(defgeneric get-transit-2 (state transit-char FA-inst))

(defgeneric get-transit (state transit-char Δ-inst))

;;; fa.lisp  nfa.lisp
(defgeneric push-transit-2 (state-A state-B transit-char FA-inst Σ-inst))

;;; nfa.lisp
(defgeneric push-next-states (states-in-tree FA-inst))

(defgeneric push-fragment-2 (fragment-type specifications-list NFA-inst))

(defgeneric push-fragment (regex-fragment-tree NFA-inst &rest pass-forward-args))

(defgeneric regex-tree->NFA (regex-tree))

;;; dfa.lisp
(defgeneric find-name-equal (thing1 thing2))

(defgeneric get-state (return-value state-property FA-inst))

(defgeneric push-state-new (state FA-inst &key &allow-other-keys))

(defgeneric ε-closure (state NFA-inst))

(defgeneric is-final-p (FA-inst))

(defgeneric NFA->DFA-map (NFA-inst))

(defgeneric state->group (DFA-inst state state-groups))

(defgeneric state-equal (DFA-inst state-A state-B &key &allow-other-keys))

(defgeneric group-consistent-p (DFA-inst state-groups))

(defgeneric push-group-states (state-groups DFA-inst &key &allow-other-keys))

(defgeneric is-start-p (FA-inst))

(defgeneric group-consistent (DFA-inst state-groups))
  
(defgeneric minimize-states (DFA-inst state-groups))
  
(defgeneric DFA->DFA-min-map (DFA-inst))
  
(defgeneric NFA->DFA (NFA-inst))
  
(defgeneric regex-tree->DFA (regex-tree))

(defgeneric map-group-transits (state-groups DFA-inst))

;;; testing.lisp
(defgeneric get-Δ (state FA-inst))

(defgeneric get-all-transit (transit-char FA-inst))

(defgeneric list-FA (FA-inst))
