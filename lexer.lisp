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

(defun NFA->DFA-iter (NFA-inst Q-map state-iter)
  (if (< state-iter (fill-pointer (Q Q-map)))
      (progn
	(dolist (NFA-state (get-state state-iter Q-map))
	  (dolist (transit-char (Σ-in-use NFA-inst))
	    (dolist (NFA-transit-state (get-transit NFA-state transit-char NFA-inst))
	      (push-transit state-iter
			    (push-state-new (ε-closure NFA-transit-state
						       NFA-inst)
					    Q-map)
			    transit-char
			    Q-map))))
	(NFA->DFA-iter NFA-inst Q-map (1+ state-iter)))
      Q-map))

(defmethod NFA->DFA-Q-map ((NFA-inst NFA))
  (let ((Q-map (make-instance 'FA)))
    (push-state (ε-closure (get-state (q₀ NFA-inst)
				      NFA-inst)
			   NFA-inst)
		Q-map
		:start-p t)
    (NFA->DFA-iter NFA-inst
		   Q-map
		   0)))
