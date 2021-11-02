;;;; org.unaen.cl.lexer/nfa.lisp

(in-package #:org.unaen.cl.lexer)

(defun FA-system (FA-type)
  "Return an FA class of object of type FA, NFA, or DFA (or subclass)."
  (if (not (subtypep FA-type 'FA))
      (error "FA-type must be a class or subclass of FA")      
      (let* ((Q            (sets:set))
	     (FA           (make-instance FA-type :Q Q))
	     (state-kernel (make-instance 'FA-state-kernel :states Q)))
	(make-instance 'FA-system :FA FA :state-kernel state-kernel))))

(defgeneric make-state (FA-state-kernel))

(defmethod make-state ((state-kernel FA-state-kernel))
  (with-slots ((states states) (iterate iterate)) state-kernel
    (sets:set-add-element (make-instance 'FA-state :enum (incf iterate) :kernel state-kernel)
			  states)))

(defgeneric make-transition (transit-symbol FA-state-previous FA-state-next FA)
  (:documentation "Add mapping of state-prev ✕ transit-symbol → {state-next, ...} ∊ P(Q)."))

(defmethod make-transition :before (transit-symbol (state-prev FA-state) (state-next FA-state) (FA FA))
  (with-slots ((Q Q)) FA
    (unless (and (sets:set-get-element state-prev Q)
		 (sets:set-get-element state-next Q))
      (error "STATE-PREV or STATE-NEXT not a member of set Q in FA."))))

;; Instead of starting with a set Σ, we are adding the actual '(type character) or 'ε from the reglex.
(flet ((make-transition (transit-symbol state-prev state-next FA)
	 (with-slots ((Σ Σ) (Δ Δ)) FA
	   (let* ((transit-symbol (sets:set-add-element transit-symbol Σ))
		  (map-in         `(,state-prev ,transit-symbol))
		  (states         (maps:map-get map-in Δ)))
	     (etypecase states ;If set already exists add the element, otherwise create a set with the element.
	       (sets:set (sets:set-add-element state-next states)); => state-next
	       (null     (when (maps:map-add (sets:set state-next)
					     map-in
					     Δ)
			   state-next)))))))
  
  (defmethod make-transition ((transit-symbol character) (state-prev FA-state) (state-next FA-state) (FA FA))
    (make-transition transit-symbol state-prev state-next FA))
  
  (defmethod make-transition ((ε (eql 'ε)) (state-prev FA-state) (state-next FA-state) (NFA NFA))
    (make-transition 'ε state-prev state-next NFA)))

#|
Some notes while thinking about what I need to implement for PUSH-REGLEX.

A relatively strict progression of operators from regular expressions:
NAME            OPERATOR      EXPRESSION     NOTES
  literal         lit           "a"            The set consisting of the one-letter string {"a"}. 
                                                 (lit #\a)
  epsilon         ε             ε              The set containing the empty string {""}.
  concatenate     conc          st             Strings constructed by concatenating a string from the language 
                                                 of s with a string from the language of t.
  or              or            s|t            Strings from both languages.
  brackets*       ors           [stvu]         (ors s t v u) => (or s (or t (or v u)))
  literals*       lits          [abcd]         (lits #\a #\b #\c #\d) => (ors (lit #\a) (lit #\b) (lit #\c) (lit #\d))
                                                 => (or (lit #\a) (or (lit #\b) (or (lit #\c) (lit #\d))))
  interval        inter         [a-c]          The set of all letters in the respective interval: 
                                                 (inter #\a #\c) => (lits #\a #\b #\c)
  intervals*      inters        [a-c1-3]       Multiple intervals:
                                                 (inters #\a #\c 1 3) => (ors (lits #\a #\b #\c) (lits #\1 #\2 #\3))
  star            star          s*             Each string in the language of s is a concatenation of ANY number 
                                                 of strings in the language of s.
  plus            plus          s+             Each string in the language of s is a concatenation of ONE or 
                                                 more strings in the language of s.
  optional        opt           s?             A string in the language of s can occur ZERO or ONE time.

*However, it makes more sense to implement inline ors with or, lits with lit, and inters with inter. It will 
also reduce the numbers of states and transitions right off (even though they will be minimized in conversion
to a DFA anyways.
|#

;;; Give a reglex list expression, a start state, and an FA-system class instance, and it will push the respective 
;;;   Finite Automata - as represented by the reglex list expression - onto the FA class instance, generating states
;;;   as necessary from the state-kernel, and then will return the exit (final) state.
(defgeneric push-reglex (reglex-list FA-state-previous FA-system &rest reglex-op-args)
  (:documentation "Accept a list/tree composed of (a) lisp'ified regular expression(s) and recursively evaluate them into an FA."))

(defmethod push-reglex ((reglex list) (state-prev fa-state) (fa-system fa-system) &rest reglex-op-args)
  "Decompose list into an operator and its operands."
  (declare (ignore reglex-op-args))
  (let ((operator (first reglex))
        (operands (rest reglex)))
    (apply #'push-reglex operator state-prev fa-system operands))); => Final state of reglex expression.

(defmethod push-reglex :before ((ε (eql 'ε)) (state-prev fa-state) (fa-system fa-system) &rest ε-args)
  (when ε-args (error "ε operator does not accept arguments.")))

(defmethod push-reglex ((ε (eql 'ε)) (state-prev fa-state) (fa-system fa-system) &rest ε-args)
  "The empty string or epsilon transition: \"\" <=> {\"\"} <=> (ε) <=> (epsilon)."
  (declare (ignore ε-args))
  (with-slots ((fa fa) (state-kernel state-kernel)) fa-system
    (let ((state-next (make-state state-kernel)))
      (make-transition 'ε state-prev state-next fa)))); => state-next

(defmethod push-reglex :before ((lit (eql 'lit)) (state-prev fa-state) (fa-system fa-system) &rest lit-args)
  (unless lit-args (error "LIT operator requires one or more arguments.")))

(defmethod push-reglex ((lit (eql 'lit)) (state-prev fa-state) (fa-system fa-system) &rest lit-args)
  "One or more literal symbols (character): \"ab\" <=> {\"a\", \"b\"} <=> (lit #\a #\b)"
  (with-slots ((state-kernel state-kernel) (fa fa)) fa-system
    (loop :for character :in lit-args
	  :and  state-char = (make-state state-kernel)
	  :with state-next = (make-state state-kernel)
	  :do (make-transition character state-prev state-char fa)
	      (make-transition 'ε        state-char state-next fa)
	  :finally (return state-next))))

(defmethod push-reglex :before ((or (eql 'or)) (state-prev fa-state) (fa-system fa-system) &rest or-args)
  (unless or-args (error "OR operator requires one or more arguments.")))

(defmethod push-reglex ((or (eql 'or)) (state-prev fa-state) (fa-system fa-system) &rest or-args)
  "A string in the language s xor in the language t: s|t <=> L(s) ∪ L(t) <=> (or s t)."
  (with-slots ((state-kernel state-kernel) (fa fa)) fa-system
    (loop :for or-arg :in or-args
	  :and  state-or   = (make-state state-kernel)
	  :with state-next = (make-state state-kernel)
	  :do (make-transition 'ε state-prev state-or fa)
	      (make-transition 'ε
			       (push-reglex or-arg state-or fa-system)
			       state-next
			       fa)
	  :finally (return state-next))))

(defmethod push-reglex :before ((conc (eql 'conc)) (state-prev fa-state) (fa-system fa-system) &rest conc-args)
  (unless conc-args (error "CONC operator requires one or more arguments.")))

(defmethod push-reglex ((conc (eql 'conc)) (state-prev fa-state) (fa-system fa-system) &rest conc-args)
  "The language defined by concatenating a string from language s with a string from language t: st <=> {mn | m∈L(s), n∈L(t)} <=> (conc s t)."
  (with-slots ((state-kernel state-kernel) (fa fa)) fa-system
    ;; Each state-next returned by a (push-reglex conc-arg ..) is the state-prev for the next conc-arg until the final one is just returned.
    (loop :for conc-arg :in conc-args
	  :do (setf state-prev
		    (push-reglex conc-arg state-prev fa-system))
	  :finally (return state-prev))))

(defmethod push-reglex :before ((star (eql 'star)) (state-prev fa-state) (fa-system fa-system) &rest star-arg)
  (unless (and star-arg (not (cdr star-arg)))
    (error "STAR operator requires exactly a single argument.")))

(defmethod push-reglex ((star (eql 'star)) (state-prev fa-state) (fa-system fa-system) &rest star-arg)
  "A string that is a concatenation of zero or more strings in the language s: s* <=> {\“\”} ∪ {vw | v∈L(s), w∈L(s∗)} <=> (star s)."
  (with-slots ((fa fa)) fa-system
    (make-transition 'ε
		     (push-reglex star-arg state-prev fa-system)
		     state-prev
		     fa))); => state-prev

(defmethod push-reglex :before ((plus (eql 'plus)) (state-prev fa-state) (fa-system fa-system) &rest plus-arg)
  (unless (and plus-arg (not (cdr plus-arg)))
    (error "PLUS operator requires exactly a single argument.")))

(defmethod push-reglex ((plus (eql 'plus)) (state-prev fa-state) (fa-system fa-system) &rest plus-arg)
  "A string that is a concatenation of one or more strings in the language s: s+ <=> {xy | x∈L(s), y∈L(s*)} <=> (plus s)."
  (with-slots ((fa fa)) fa-system
    (let ((state-next
	    (push-reglex plus-arg state-prev fa-system)))
      (make-transition 'ε state-next state-prev fa)
      state-next)))

(defmethod push-reglex ((inter (eql 'inter)) (state-prev fa-state) (fa-system fa-system) &rest inter-args)
  "Shorthand for or'ing all characters in an interval: [\"0\"-\"9\"] <=> {\"0\",\"1\", ... ,\"9\"} <=> (inter #\0 #\9) <=> (lits #\0 #\1 ... #\9)."
  (let ((lit-args
	  (loop :for (min max) :on inter-args :by (function cddr)
		:appending (util:char-interval->list min max))))
    (apply #'push-reglex 'lit state-prev fa-system lit-args))); => Final state of LIT Reglex.

(defmethod push-reglex :before ((opt (eql 'opt)) (state-prev fa-state) (fa-system fa-system) &rest opt-arg)
  (unless (and opt-arg (not (cdr opt-arg)))
    (error "OPT operator requires exactly a single argument.")))

(defmethod push-reglex ((opt (eql 'opt)) (state-prev fa-state) (fa-system fa-system) &rest opt-arg)
  "An optional symbol or set: \"a\"? <=> {\"a\",\"\"} <=> (opt #\a)"
  (with-slots ((fa fa)) fa-system
    (let ((state-next
	    (push-reglex opt-arg state-prev fa-system)))
      (make-transition 'ε state-prev state-next fa)))); => state-next

