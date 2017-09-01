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

(defparameter *test-regex-tree*
  '(conc (opt (#\+ #\-))
         (or (conc (or (conc (plus (inter #\0 #\9))
                             (#\.)
                             (star (inter #\0 #\9)))
	               (conc (#\.)
			     (plus (inter #\0 #\9))))
		   (opt (conc (#\e #\E)
		              (opt (#\+ #\-))
		              (plus (inter #\0 #\9)))))
	     (conc (plus (inter #\0 #\9))
	           (#\e #\E)
	           (opt (#\+ #\-))
	           (plus (inter #\0 #\9))))) )

(defparameter *test-regex-tree-2*
  '(conc (#\a #\b #\c)
         (#\1 #\2 #\3)))

(defparameter *test-regex-figure-2.5*
  '(conc (star (#\a #\b))
         (#\a)
         (#\c)) )

;;    Find all states we can transition to and perform ε-closure on them.
;;    This set of states is a single state in our DFA--If it is a new set, then push this set
;; onto our Q-map (states map) as a new state.
;;    Set a new transition on the same transition character from the current DFA state
;; (state-iter) to the recently discovered DFA state--although, not necessarily the first
;; time it was discovered (wrapped up in push-state-new).
(defun NFA->DFA-iter (NFA-inst Q-map state-iter)
  (cond ((< state-iter (fill-pointer (Q Q-map)))
	 (dolist (transit-char (Σ-in-use NFA-inst))
	   (push-transit state-iter
			 (push-state-new (ε-closure (mappend #'(lambda (x)
								 (get-transit x
									      transit-char
									      NFA-inst))
							     (aref (Q Q-map) state-iter))
						    NFA-inst)
					 Q-map
					 :final-p (is-NFA-final-p (F NFA-inst)))
			 transit-char
			 Q-map))
	 (NFA->DFA-iter NFA-inst Q-map (1+ state-iter)))
	(t Q-map)))

;; Part of hack to check final states when converting NFA to DFA.
(defun is-NFA-final-p (states-final)
  #'(lambda (states-check)
      (reduce #'(lambda (x y) (or x y))
	      states-check
	      :key #'(lambda (x) (aref states-final x)))))

;; Make a states map, each DFA state containing a list (set) of NFA states.
(defmethod NFA->DFA-map ((NFA-inst NFA))
  (let ((Q-map (make-instance 'DFA)))
    (push-state (ε-closure (get-state (q₀ NFA-inst)
				      NFA-inst)
			   NFA-inst)
		Q-map
		:start-p t)
    (NFA->DFA-iter NFA-inst
		   Q-map
		   0)))

;; (Q Σ Σ-in-use Δ q₀ F dsn)
(defmethod DFA-map->DFA ((DFA-map DFA))
  (when (= (fill-pointer (Q DFA-map))
	   (fill-pointer (Δ DFA-map))
	   (fill-pointer (F DFA-map))) ;quick sanity check
    (let ((DFA-inst (make-instance 'DFA
				   :Σ (Σ DFA-map)
				   :Σ-in-use (Σ-in-use DFA-map)
				   :Δ (Δ DFA-map))))
      (map 'nil #'(lambda (x) (push-state 'next DFA-inst :delta-p nil :final-p x)) (F DFA-map))
      (setf (slot-value DFA-inst 'q₀) (get-state (q₀ DFA-map) DFA-map))
      DFA-inst)))

(defun list-fa (fa-inst)
  (list (list 'Q '-> (Q fa-inst))
	(list 'Σ '-> (Σ fa-inst))
	(list 'Σ-in-use '-> (Σ-in-use fa-inst))
	(list 'Δ '->(Δ fa-inst))
	(list 'q₀ '-> (q₀ fa-inst))
	(list 'F '-> (F fa-inst))))

      
