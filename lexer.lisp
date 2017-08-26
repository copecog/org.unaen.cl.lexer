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

;;; Class Definitions


;;; Q Σ Δ q₀ F   ε

(defvar *debug* (list))

;(defmethod NFA->DFA :around ((NFA-inst NFA))
;  (push 'NFA->around *debug*)
;  (call-next-method))

;(defmethod NFA->DFA :around ((Q-inst Q))
;  (push 'Q->around *debug*)
;  (call-next-method))

;(defmethod NFA->DFA :around ((Σ-inst Σ))
;  (push 'Σ->around *debug*)
;  (call-next-method))

;(defmethod NFA->DFA :around ((Δ-inst Δ))
;  (push 'Δ->around *debug*)
;  (call-next-method))

;(defmethod NFA->DFA :around ((q₀-inst q₀))
;  (push 'q₀->around *debug*)
;  (call-next-method))

;(defmethod NFA->DFA :around ((F-inst F))
;  (push 'F->around *debug*)
;  (call-next-method))

(defmethod NFA->DFA :before ((NFA-inst NFA))
  (push 'NFA->before *debug*))

(defmethod NFA->DFA :before ((Q-inst Q))
  (push 'Q->before *debug*))

(defmethod NFA->DFA :before ((Σ-inst Σ))
  (push 'Σ->before *debug*))

(defmethod NFA->DFA :before ((Δ-inst Δ))
  (push 'Δ->before *debug*))

(defmethod NFA->DFA :before ((q₀-inst q₀))
  (push 'q₀->before *debug*))

(defmethod NFA->DFA :before ((F-inst F))
  (push 'F->before *debug*))

(defmethod NFA->DFA ((NFA-inst NFA))
  (push 'NFA->prime *debug*))

;(defmethod NFA->DFA :after ((F-inst F))
;  (push 'F->after *debug*))

;(defmethod NFA->DFA :after ((q₀-inst q₀))
;  (push 'q₀->after *debug*))

;(defmethod NFA->DFA :after ((Δ-inst Δ))
;  (push 'Δ->after *debug*))

;(defmethod NFA->DFA :after ((Σ-inst Σ))
;  (push 'Σ->after *debug*))

;(defmethod NFA->DFA :after ((Q-inst Q))
;  (push 'Q->after *debug*))

;(defmethod NFA->DFA :after ((NFA-inst NFA))
;  (push 'NFA->after *debug*))




