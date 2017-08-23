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

;;   There are several classes of states across varying classes of finite
;; automata as well as transitional sets of states while functionally mapping
;; from one automaton to another.
;;   This scheme with a preface and iterate allows us to have systematic naming
;; that is still flexible.
(defclass state-names ()
  ((preface :initarg :preface
	    :initform "q_"
	    :reader preface)
   (iterate :initarg :iterate
	    :initform 0
	    :reader iterate))
  (:documentation "Reference to create a series of related states."))

;;   The set Q of states, however named--each state is enumerated by the integer
;; indices of an array.
(defclass Q ()
  ((Q :initarg :Q
      :initform (make-state-vector 1)
      :reader Q
      :documentation "A finite set of states Q.")))

;;   The set Σ of input symbols.  Whatever the input symbols are, they need
;; enumeration--however, we are going to use the built in character encoding in
;; lisp to give us that mapping (signified by a 'cl-utf slot-value).
(defclass Σ ()
  ((Σ :initarg :Σ
      :initform 'cl-utf
      :reader Σ
      :documentation "A finite set of input symbols Σ.")
   (Σ-in-use :initarg :Σ-in-use
	      :initform (list)
	      :reader Σ-in-use
	      :documentation "A list of actual symbol objects in use.")))

;;   A transition function Δ : Q × Σ → P(Q). Using the enumeration from Q we
;; store each state object under an array, and each state contains the
;; transitions on symbols from Σ, which gives us our function map.
(defclass Δ ()
  ((Δ :initarg :Δ
      :initform (make-state-vector 1)
      :reader Δ
      :documentation "A transition function Δ : Q × Σ → P(Q).")))

;;   The initial or starting state of the finite automata. Usually named as
;; q₀ ∈ Q in the mathematical definition.
(defclass q₀ ()
  ((q₀ :initarg :q₀
       :reader q₀
       :documentation "An initial (or start) state q₀ ∈ Q.")))

;;   The set F of final or accepting states. F is an array of the same size as Q
;; with the accepting states under the same respective indices as under Q.
(defclass F ()
  ((F :initarg :F
      :initform (make-state-vector 1)
      :reader F
      :documentation "A set of states F distinguished as accepting (or final) states F ⊆ Q.")))

(defclass FA (Q Σ Δ q₀ F)
  ((dsn :initarg dsn
	:initform (make-instance 'state-names)
	:reader dsn
	:documentation "_D_efault _S_tate _N_ames."))
  (:documentation "An finite automata is represented formally by a 5-tuple, (Q, Σ, Δ, q0, F)."))

;; https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton
(defclass NFA (FA) ()
  (:documentation "Nondeterministic Finite Automata."))


;;; Generic Function Prototypes
(defgeneric make-state-vector (size &key &allow-other-keys)
  (:documentation "Return the default chosen array type for states."))

(defgeneric make-state-name (state-names)
  (:documentation "Name a new state from a reference class instance for a series of states."))

(defgeneric make-Δ (Σ-type)
  (:documentation "Create respective character-transition map data-structure."))

(defgeneric push-state (state finite-automaton &key &allow-other-keys)
  (:documentation "Push a state into the finite automaton."))

(defgeneric push-transit (state-A state-B transit-char FA &key &allow-other-keys)
  (:documentation "Push a single transition on transit-char from state-A to state-B in FA."))

(defgeneric delete-transit (state-A state-B transit-char FA &key &allow-other-keys)
  (:documentation "Remove a transition on transit-char from state-A to state-B in FA."))

(defgeneric get-transit (state transit-char FA &key &allow-other-keys)
  (:documentation "Get list of states to transition to on transition character."))

(defgeneric push-fragment (regex-fragment-list FA &rest arguments &key &allow-other-keys)
  (:documentation "Push new finite state automaton fragment onto FA by type."))

(defgeneric push-fragment-3 (fragment-type specifications-list FA &key &allow-other-keys))

(defgeneric char-interval->list (char-start char-end))


;;; Method Definitions
;;   By default the first state name generated by the name-preface.
(defmethod initialize-instance :after ((FA-instance FA) &key &allow-other-keys)
  (push-state 'next FA-instance :start-p t))

(defmethod make-state-vector ((size integer) &key
					       (initial-element nil)
					       (adjustable t)
					       (fill-pointer 0)
			      &allow-other-keys)
  (make-array size
	      :initial-element initial-element
	      :adjustable adjustable
	      :fill-pointer fill-pointer))

;;   The transitions are a hashtable that associates the literal next state
;; object with a character.
(defmethod make-Δ ((Σ-type (eql 'cl-utf)))
  (make-hash-table :test 'eql)) ;eql for cl char's

;;   To initialize a state, we name the state using the name-preface
;; and the iterate.
(defmethod make-state-name ((state-names-instance state-names))
  (with-slots (preface iterate) state-names-instance
    (format nil "~a~d" preface (1- (incf iterate)))))

(defmethod push-state ((next (eql 'next))
		       (FA-instance FA)
		       &key
			 (start-p nil)
			 (final-p nil)
		       &allow-other-keys)
  (push-state (make-state-name (dsn FA-instance))
	      FA-instance
	      :start-p start-p
	      :final-p final-p))

(defmethod push-state ((state integer)
		       (FA-instance FA)
		       &key
			 (start-p nil)
			 (final-p nil)
		       &allow-other-keys)
  (with-slots (Q) FA-instance
    (values FA-instance (when (<= state (fill-pointer Q))
			  state))))
			  
;; All the :before methods I use to organize the assigns for the single push-state operation.
(defmethod push-state :before ((state-name string)
			       (Q-instance Q)
			       &key
			       &allow-other-keys)
  (with-slots (Q) Q-instance
    (vector-push-extend state-name Q))) ; Push our state-name onto our vector of state names.

(defmethod push-state :before ((state-name string)
			       (Δ-instance Δ)
			       &key
			       &allow-other-keys)
  (with-slots (Σ Δ) Δ-instance
    (vector-push-extend (make-Δ Σ) Δ))) ; Extend Δ for state-name.

(defmethod push-state :before ((state-name string)
			       (q₀-instance q₀)
			       &key
				 (start-p nil)
			       &allow-other-keys)
  (with-slots (q₀) q₀-instance
    (when start-p
      (setf q₀ state-name)))) ; If start state then set as start state.

(defmethod push-state :before ((state-name string)
			       (F-instance F)
			       &key
				 (final-p nil)
			       &allow-other-keys)
  (with-slots (F) F-instance
    (if final-p
	(vector-push-extend state-name F) ;   Either it is a final state
	(vector-push-extend nil F))))     ; or it is not - so empty space pushed.

(defmethod push-state ((state-name string)
		       (FA-instance FA)
		       &key
		       &allow-other-keys)
  (with-slots (Q Δ F) FA-instance
    (values FA-instance                    ; Return mutated FA after all said and done...
	    (1- (and (fill-pointer Q)
		     (fill-pointer Δ)      ; (and quick consistency check)
		     (fill-pointer F)))))) ; ...as well as the state number.

;;; I don't like how the work for push-transit is organized...
(defmethod push-transit :before ((state-A integer)
				 (state-B integer)
				 (transit-char character)
				 (Δ-instance Δ)
				 &key &allow-other-keys)
  (with-slots (Δ) Δ-instance
    (let ((Δ.state-A (aref Δ state-A)))
      (push state-B (gethash transit-char Δ.state-A nil)))))

(defmethod push-transit :before ((state-A integer)
				 (state-B integer)
				 (ε (eql 'ε))
				 (NFA-instance NFA)
				 &key &allow-other-keys)
  (with-slots (Δ) NFA-instance
    (let ((Δ.state-A (aref Δ state-A)))
      (push state-B (gethash ε Δ.state-A nil)))))

(defmethod push-transit :before ((state-A integer)
				 (state-B integer)
				 transit-char
				 (Σ-instance Σ)
				 &key &allow-other-keys)
  (with-slots (Σ-in-use) Σ-instance
    (pushnew transit-char Σ-in-use)))

(defmethod push-transit (state-A
			 state-B
			 transit-char
			 (FA-instance FA)
			 &key &allow-other-keys)
  FA-instance)

;; experimenting with symbol naming and readability...
(defmethod delete-transit :before ((state-A integer)
				   (state-B integer)
				   transit-char
				   (Δ-instance Δ)
				   &key &allow-other-keys)
  (with-slots (Δ) Δ-instance
    (let* ((Δ.state-A (aref Δ state-A))
           (Δ.state-A.transit-char.states (gethash transit-char Δ.state-A)))
      (if Δ.state-A.transit-char.states
	(let ((Δ.state-A.transit-char.states-no-B (delete state-B Δ.state-A.transit-char.states)))
	  (if Δ.state-A.transit-char.states-no-B
	      (setf (gethash transit-char Δ.state-A) Δ.state-A.transit-char.states-no-B)
	      (remhash transit-char Δ.state-A)))
	(remhash transit-char Δ.state-A)))))

(defmethod delete-transit (state-A
			   state-B
			   transit-char
			   (FA-instance FA)
			   &key &allow-other-keys)
  FA-instance)

(defmethod get-transit ((state integer)
			transit-char
			(Δ-instance Δ)
			&key &allow-other-keys)
  (with-slots (Δ) Δ-instance
    (let* ((Δ.state (aref Δ state))
	   (Δ.state.transit-char.states (gethash transit-char Δ.state)))
      Δ.state.transit-char.states)))


;;;    push-fragment-3 is the work-horse: it actually processes lists of states and/or
;;; transit-characters for contained sub-fragments and creates as well as links those
;;; respective states through transitions on those respective transit-characters.
;;;
;;; fragment-type
;;;     regex-literal
;;;         begin-state[transit-char]-->end-state
;;;     regex-ε
;;;         begin-state[ε]-->end-state
;;;     regex-concat
;;;          begin[ε]-->A-in + A-out[ε]-->B-in + B-out[ε]-->end
;;;     regex-or
;;;           begin[ε] -->A-in A-out[ε]--> ||
;;;              ||    -->B-in B-out[ε]--> end
;;;     regex-star
;;;           begin*[ε]-->A-in A-out[ε]-->begin*
;;;     regex-interval
;;;
;;;     regex-optional
;;;

;; (push-fragment-3 'regex-literal '((fragment-literal-in fragment-literal-out) (char-1 char-2 ... char-n)) NFA)
;; ==> NFA fragment-literal-in fragment-literal-out
(defmethod push-fragment-3 ((fragment-type (eql 'regex-literal))
			    (argument-list list)
			    (NFA-instance NFA)
			    &key &allow-other-keys)
  (let ((fragment-literal-in (caar argument-list))
	(fragment-literal-out (cadar argument-list))
	(transit-character-list (cadr argument-list)))
    (multiple-value-bind (NFA-instance fragment-literal-in)
	(push-state fragment-literal-in
		    NFA-instance)
      (multiple-value-bind (NFA-instance fragment-literal-out)
	  (push-state fragment-literal-out
	              NFA-instance)
	(values
	 (dolist (transit-character transit-character-list NFA-instance)
	   (setf NFA-instance (push-transit fragment-literal-in
					    fragment-literal-out
					    transit-character
					    NFA-instance)))
	 fragment-literal-in
	 fragment-literal-out)))))

;; (push-fragment-3 'regex-ε '((state-1-in state-1-out) (state-2-in state-2-out) ... (state-n-in state-n-out)) NFA)
;; --> (push-fragment-3 'regex-literal '((state-1-in state-1-out) ('ε)) NFA) ... (push-fragment-3 'regex-literal '((state-n-in state-n-out) ('ε)) NFA)
;; ==> NFA state-1-in state-2-out
(defmethod push-fragment-3 ((fragment-type (eql 'regex-ε))
			    (argument-list list)
			    (NFA-instance NFA)
			    &key &allow-other-keys)
  (let ((fragment-ε-in (caar argument-list))
	(fragment-ε-out-n nil))
    (dolist (state-pair argument-list)
      (multiple-value-bind (NFA-instance-m fragment-ε-in-m fragment-ε-out-m)
	  (push-fragment-3 'regex-literal (list state-pair (list 'ε)) NFA-instance)
	(setf fragment-ε-out-n fragment-ε-out-m)
	(setf NFA-instance NFA-instance-m)))
    (values NFA-instance fragment-ε-in fragment-ε-out-n)))

;; (push-fragment-3 'regex-concat '((fragment-concat-in fragment-concat-out) (frag-1-in frag-2-out) ... (frag-n-in frag-n-out)) NFA)
;; --> (push-fragment-3 'regex-ε '((fragment-concat-in frag-1-in) (frag-2-out frag-3-in) ... (frag-n-out fragment-concat-out)) NFA)
;; ==> NFA fragment-concat-in fragment-concat-out
(defmethod push-fragment-3 ((fragment-type (eql 'regex-concat))
			    (argument-list list)
			    (NFA-instance NFA)
			    &key &allow-other-keys)
  (let ((fragment-concat-in (caar argument-list))
	(fragment-concat-out (cadar argument-list))
	(fragment-1st-in (caadr argument-list))
	(fragments-to-concat (cdr argument-list)))
    (labels ((chain-pairs (old-list new-list)
	       (if (cdr old-list)
		   (chain-pairs (cdr old-list)
			        (append new-list
					(list (list (cadar old-list) (caadr old-list)))))
		   (values (cadar old-list) new-list))))
      (multiple-value-bind (fragment-nth-out chain-pairs)
	  (chain-pairs fragments-to-concat (list))
	(push-fragment-3 'regex-ε
			 (append (list (list fragment-concat-in fragment-1st-in))
			         chain-pairs
			         (list (list fragment-nth-out fragment-concat-out)))
			 NFA-instance)))))

;; (push-fragment-3 'regex-or '((fragment-or-in fragment-or-out) (frag-1-in frag-1-out) ... (frag-n-in frag-n-out)) NFA)
;; --> (push-fragment-3 'regex-ε '((fragment-or-in frag-1-in) ... (fragment-or-in frag-n-in) (frag-1-out fragment-or-out) ... (frag-n-out fragment-or-out)) NFA)
;; ==> NFA fragment-or-in fragment-or-out
(defmethod push-fragment-3 ((fragment-type (eql 'regex-or))
			    (argument-list list)
			    (NFA-instance NFA)
			    &key &allow-other-keys)
  (let ((fragment-or-in (caar argument-list))
	(fragment-or-out (cadar argument-list))
	(frags-to-or (cdr argument-list))
	(frag-or-pairs (list)))
    (push-fragment-3 'regex-ε
		     (dolist (frag frags-to-or frag-or-pairs)
		       (setf frag-or-pairs (append frag-or-pairs
					           (list (list fragment-or-in (car frag)))
						   (list (list (cadr frag) fragment-or-out)))))
		     NFA-instance)))

;; (push-fragment-3 'regex-star '((fragment-star-in fragment-star-out) (frag-in frag-out)) NFA)
;; --> (push-fragment-3 'regex-ε '((fragment-star-in fragment-star-out) (fragment-star-out fragment-star-in) (fragment-star-in frag-in) (frag-out fragment-star-out)) NFA)
;; ==> NFA fragment-star-in fragment-star-out
(defmethod push-fragment-3 ((fragment-type (eql 'regex-star))
			    (argument-list list)
			    (NFA-instance NFA)
			    &key &allow-other-keys)
  (let ((fragment-star-in (caar argument-list))
	(fragment-star-out (cadar argument-list))
	(fragment-in (caadr argument-list))
	(fragment-out (caddr argument-list)))
    (push-fragment-3 'regex-ε
		     (list (list fragment-star-in fragment-star-out)
			   (list fragment-star-out fragment-star-in)
			   (list fragment-star-in fragment-in)
			   (list fragment-out fragment-star-out))
		     NFA-instance)))

;; (push-fragment-3 'regex-plus '((fragment-plus-in fragment-plus-out) (frag-in frag-out)) NFA)
;; --> (push-fragment-3 'regex-or '((fragment-plus-in fragment-plus-out) (frag-in frag-out)) NFA)
;; ==> NFA state-begin state-end
(defmethod push-fragment-3 ((fragment-type (eql 'regex-plus))
			    (argument-list list)
			    (NFA-instance NFA)
			    &key &allow-other-keys)
  (push-fragment-3 'regex-or argument-list NFA-instance)) 

;; (push-fragment-3 'regex-interval '((fragment-interval-in fragment-interval-out) (interval-1-char-start interval-1-char-end) ... (interval-n-char-start interval-n-char-end)) NFA)
;; --> (push-fragment-3 'regex-literal '((fragment-interval-in fragment-interval-out) (interval-1-char-start interval-1-char-2 ... interval-1-char-end ... interval-n-char-start interval-n-char-2 ... interval-n-char-end)) NFA)
;; ==> NFA state-begin state-end
(defmethod push-fragment-3 ((fragment-type (eql 'regex-interval))
			    (argument-list list)
			    (NFA-instance NFA)
			    &key &allow-other-keys)
  (let ((start-end (car argument-list))
	(intervals-list (cdr argument-list))
	(char-list (list)))
    (push-fragment-3 'regex-literal
		     (list start-end
			   (dolist (interval intervals-list char-list)
			     (setf char-list
				   (append char-list
					   (char-interval->list (first interval)
								(second interval))))))
		     NFA-instance)))

(defmethod char-interval->list ((char1 character) (char2 character))
  (when (char< char1 char2)
    (do ((char-iter char1 (code-char (1+ (char-code char-iter))))
	 (char-list (list) (push char-iter char-list)))
	((char> char-iter char2) (nreverse char-list)))))

;; (push-fragment-3 'regex-optional '((fragment-optional-in fragment-optional-out) (fragment-in fragment-out)) NFA)
;; --> (push-fragment-3 'regex-ε '((fragment-optional-in fragment-optional-out) (fragment-optional-in fragment-in) (fragment-out fragment-optional-out)) NFA)
;; ==> NFA state-begin state-end
(defmethod push-fragment-3 ((fragment-type (eql 'regex-optional))
			    (argument-list list)
			    (NFA-instance NFA)
			    &key &allow-other-keys)
  (let ((fragment-optional-in (caar argument-list))
	(fragment-optional-out (cadar argument-list))
	(fragment-in (caadr argument-list))
	(fragment-out (caddr argument-list)))
    (push-fragment-3 'regex-ε
		     (list (list fragment-optional-in fragment-optional-out)
			   (list fragment-optional-in fragment-in)
			   (list fragment-out fragment-optional-out))
		     NFA-instance)))


;;;    push-fragment maps a simpler but lisp-like syntax for manually specifying
;;; regular expressions. This implies that the regular expression has been parsed and
;;; that this lisp-like syntax is in actuality a form of a parse tree being handed
;;; over to build the NFA.
;;;
;;;    It makes sense to me to build it up to this point because the next major
;;; thing I'm going to build would be something that actually generates this tree
;;; from a regular expression.

;; Okay, Defining the "lisp-like" Syntax:
;;     - A regex fragment is defined as an independent finite automaton with a single
;; start state and a single finish state which is either given or implicitly created.
;;     - Each fragment is represented by a Lisp list.
;;     - The first element in the list is the regex operator with the rest being the
;; arguments.
;;     - In the case for when it is a character like in "(#\a #\b ... )". Then, it is
;; shorthand for "(liter #\a #\b ... )" -- which is a list of alternatives in the
;; regex such as "[abc]" (which is also equivalent to a|b|c).
;;
;; regex
;;     A literal                        a         (#\a)
;;     A list of possible literals      [abc]     (#\a #\b #\c)
;;     Kleene-star                      a*        (star (#\a))
;;     One or more                      a+        (plus (#\a))
;;     Optional                         a?        (opt (#\a))
;;     Concatenate                      abc       (conc (#\a) (#\b) (#\c))
;;     Or                               a|b|c     (or (#\a) (#\b) (#\c)) <=> (#\a #\b #\c)
;;     An interval of literals          [a-z]     (inter #\a #\z) <=> (#\a ... #\z)

;; A (C language style) regular expression for a float number:
;;
;;     [+-]?((([0-9]+.[0-9]∗|.[0-9]+)([eE][+-]?[0-9]+)?)|[0-9]+[eE][+-]?[0-9]+)

;; [+-]
;;
;;    (#\+ #\-)

;; [+-]?
;;
;;    (opt (#\+ #\-))

;; [0-9]
;;
;;    (inter 0 9)

;; [0-9]+
;;
;;    (plus (inter 0 9))

;; .
;;
;;    (#\.)

;; [0-9]*
;;
;;    (star (inter 0 9))

;; [0-9]+.[0-9]∗
;;
;;    (conc (plus (inter 0 9))
;;          (#\.)
;;          (star (inter 0 9)))

;; ([0-9]+.[0-9]∗ | .[0-9]+)
;;
;;    (or (conc (plus (inter 0 9))
;;    	        (#\.)
;;	        (star (inter 0 9)))
;;        (conc (#\.)
;;	        (plus (inter 0 9))))

;; ([eE][+-]?[0-9]+) ?
;;
;;    (opt (conc (#\e #\E)
;;	         (opt (#\+ #\-))
;;	         (plus (inter 0 9))))

;; ( ([0-9]+.[0-9]∗|.[0-9]+) ([eE][+-]?[0-9]+)? )
;;
;;    (conc (or (conc (plus (inter 0 9))
;;		      (#\.)
;;		      (star (inter 0 9)))
;;	        (conc (#\.)
;;	              (plus (inter 0 9))))
;;          (opt (conc (#\e #\E)
;;		       (opt (#\+ #\-))
;;		       (plus (inter 0 9)))))

;; [0-9]+[eE][+-]?[0-9]+
;;
;;    (conc (plus (inter 0 9))
;;          (#\e #\E)
;;          (opt (#\+ #\-))
;;          (plus (inter 0 9)))


;; [+-]? ( (([0-9]+.[0-9]∗|.[0-9]+)([eE][+-]?[0-9]+)?) | [0-9]+[eE][+-]?[0-9]+ )

(defparameter *test-regex* '(conc (opt (#\+ #\-))
			          (or (conc (or (conc (plus (inter 0 9))
						      (#\.)
					              (star (inter 0 9)))
					        (conc (#\.)
					              (plus (inter 0 9))))
				             (opt (conc (#\e #\E)
					                (opt (#\+ #\-))
					                (plus (inter 0 9)))))
			               (conc (plus (inter 0 9))
			                     (#\e #\E)
			                     (opt (#\+ #\-))
			                     (plus (inter 0 9))))))


(defmethod push-fragment ((regex-list list)
			  (NFA-instance NFA)
			  &key &allow-other-keys)
  (let ((regex-operation (car regex-list))
	(regex-arguments (cdr regex-list)))
    (apply #'push-fragment regex-operation NFA-instance regex-arguments)))

;; literal a
(defmethod push-fragment ((liter (eql 'liter))
			  (NFA-instance NFA)
			  &rest arguments
			  &key &allow-other-keys)
  nil)

;; concatenate ab
(defmethod push-fragment ((conc (eql 'conc))
			  (NFA-instance NFA)
			  &rest
			    arguments
			  &key &allow-other-keys)
  (let ((concat-arguments (list (list 'next 'next))))
    (push-fragment-3 'regex-concat
		     (dolist (argument arguments concat-arguments)
		       (multiple-value-bind (NFA-instance-pushed fragment-begin fragment-end)
			   (push-fragment argument NFA-instance)
			 (setf concat-arguments (append concat-arguments 
							(list (list fragment-begin fragment-end))))
					;This has to traverse concat-arguments every iteration.
			 (setf NFA-instance NFA-instance-pushed)))
		     NFA-instance))) 
      
;; or a|b
(defmethod push-fragment ((or (eql 'or))
			  (NFA-instance NFA)
			  &rest
			    arguments
			  &key &allow-other-keys)
  nil)

;; Kleene star (zero or more) *
(defmethod push-fragment ((star (eql 'star))
			  (NFA-instance NFA)
			  &rest
			    arguments
			  &key &allow-other-keys)
  nil)

;; plus (one or more) +
(defmethod push-fragment ((plus (eql 'plus))
			  (NFA-instance NFA)
			  &rest
			    arguments
			  &key &allow-other-keys)
  nil)			  

;; interval (a|b|c|...|z) a-z
(defmethod push-fragment ((inter (eql 'inter))
			  (NFA-instance NFA)
			  &rest
			    arguments
			  &key &allow-other-keys)
  nil)

;; optional ?
(defmethod push-fragment ((opt (eql 'opt))
			  (NFA-instance NFA)
			  &rest
			    arguments
			  &key &allow-other-keys)
  nil)
