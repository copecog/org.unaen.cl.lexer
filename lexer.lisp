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

(defmethod nfa->dfa ((nfa-inst nfa) &key &allow-other-keys)
  (let ((debug (list))
	(states-map (make-instance 'Q)))
    (push-state (ε-closure (get-state (q₀-name nfa-inst) nfa-inst) nfa-inst) states-map)
    (dolist (nfa-state (get-state 0 states-map) states-map)
;      (push nfa-state debug))))
      (dolist (transit-char (Σ-in-use nfa-inst))
;	(push (list nfa-state transit-char) debug)))))
	(dolist (next-state (get-transit nfa-state transit-char nfa-inst))
;	  (push (list nfa-state transit-char '-> next-state) debug))))))
	  (push-state (ε-closure next-state nfa-inst) states-map))))))
