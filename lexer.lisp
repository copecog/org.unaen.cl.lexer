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

;;; Q Σ Δ q₀ F   ε

(defmethod NFA->DFA :before ((Q-inst Q))
  nil)

(defmethod NFA->DFA :before ((Σ-inst Σ))
  nil)

(defmethod NFA->DFA :before ((Δ-inst Δ))
  nil)

(defmethod NFA->DFA :before ((q₀-inst q₀))
  nil)

(defmethod NFA->DFA :before ((F-inst F))
  nil)

(defmethod NFA->DFA ((NFA-inst NFA))
  (let ((DFA-inst (make-instance 'DFA))	(states-map (make-instance 'Q)))
    (with-slots ((nfa.Q Q) (nfa.Σ Σ) (nfa.Δ Δ) (nfa.q₀ q₀) (nfa.F F)) NFA-inst
      (with-slots ((dfa.Q Q) (dfa.Σ Σ) (dfa.Δ Δ) (dfa.q₀ q₀) (dfa.F F)) DFA-inst
	nil))))

		    

