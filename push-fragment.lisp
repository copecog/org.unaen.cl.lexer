;;;; lexer.lisp
;;;;
;;;; Copyright (C) 2017 Christopher H Cope
;;;; All rights reserved.
;;;;
;;;; This software may be modified and distributed under the terms
;;;; of the BSD license.  See the LICENSE file for details.

(in-package #:lexer)


;;; Generic Function Prototypes
(defgeneric push-fragment (regex-fragment-list FA &rest pass-forward-args)
  (:documentation "Push new finite state automaton fragment onto FA by type."))

(defgeneric push-fragment-2 (fragment-type specifications-list FA))

(defgeneric char-interval->list (char-start char-end))


;;; Method Definitions

;;;    push-fragment-2 is the work-horse: it actually processes lists of states and/or
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

;; (push-fragment-2 'regex-literal
;;                  '((fragment-literal-in fragment-literal-out)
;;                    (char-1 char-2 ... char-n))
;;                   NFA)
;;
;; ==> NFA fragment-literal-in fragment-literal-out

(defmethod push-fragment-2 ((fragment-type (eql 'regex-literal))
			    (argument-list list)
			    (NFA-instance NFA))
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

;; (push-fragment-2 'regex-ε
;;                  '((state-1-in state-1-out)
;;                    (state-2-in state-2-out)
;;                           ...
;;                    (state-n-in state-n-out))
;;                  NFA)
;;
;; --> (push-fragment-2 'regex-literal
;;                      '((state-1-in state-1-out) ('ε))
;;                      NFA)
;;         ...
;;     (push-fragment-2 'regex-literal
;;                      '((state-n-in state-n-out) ('ε))
;;                      NFA)
;;
;; ==> NFA state-1-in state-n-out

(defmethod push-fragment-2 ((fragment-type (eql 'regex-ε))
			    (argument-list list)
			    (NFA-instance NFA))
  (let ((fragment-ε-in-1st nil)
	(fragment-ε-out-n nil))
    (dolist (state-pair argument-list)
      (multiple-value-bind (NFA-instance-m fragment-ε-in-m fragment-ε-out-m)
	  (push-fragment-2 'regex-literal (list state-pair (list 'ε)) NFA-instance)
	(unless fragment-ε-in-1st
	  (setf fragment-ε-in-1st fragment-ε-in-m))
	(setf fragment-ε-out-n fragment-ε-out-m)
	(setf NFA-instance NFA-instance-m)))
    (values NFA-instance fragment-ε-in-1st fragment-ε-out-n)))

;; (push-fragment-2 'regex-concat
;;                  '((fragment-concat-in fragment-concat-out)
;;                    (frag-1-in frag-2-out)
;;                            ...
;;                    (frag-n-in frag-n-out))
;;                   NFA)
;;
;; --> (push-fragment-2 'regex-ε
;;                      '((fragment-concat-in frag-1-in)
;;                        (frag-2-out frag-3-in)
;;                              ...
;;                        (frag-n-out fragment-concat-out))
;;                       NFA)
;;
;; ==> NFA fragment-concat-in fragment-concat-out

(defmethod push-fragment-2 ((fragment-type (eql 'regex-concat))
			    (argument-list list)
			    (NFA-instance NFA))
  (let ((fragment-concat-in (caar argument-list))
	(fragment-concat-out (cadar argument-list))
	(fragment-1st-in (caadr argument-list))
	(fragments-to-concat (cdr argument-list)))
    (labels ((chain-pairs (old-list new-list)
	       (if (cdr old-list)
		   (chain-pairs (cdr old-list)
			        (push (list (cadar old-list)
					    (caadr old-list))
				      new-list))
		   (values (cadar old-list) (nreverse new-list)))))
      (multiple-value-bind (fragment-nth-out chain-pairs)
	  (chain-pairs fragments-to-concat (list))
	(push-fragment-2 'regex-ε
			 (nconc (list (list fragment-concat-in fragment-1st-in))
				chain-pairs
				(list (list fragment-nth-out fragment-concat-out)))
			 NFA-instance)))))

;; (push-fragment-2 'regex-or
;;                  '((fragment-or-in fragment-or-out)
;;                    (frag-1-in frag-1-out)
;;                             ...
;;                    (frag-n-in frag-n-out))
;;                  NFA)
;;
;; --> (push-fragment-2 'regex-ε
;;                      '((fragment-or-in frag-1-in)
;;                              ...
;;                        (fragment-or-in frag-n-in)
;;                        (frag-1-out fragment-or-out)
;;                              ...
;;                        (frag-n-out fragment-or-out))
;;                      NFA)
;;
;; ==> NFA fragment-or-in fragment-or-out

(defmethod push-fragment-2 ((fragment-type (eql 'regex-or))
			    (argument-list list)
			    (NFA-instance NFA))
  (let ((fragment-or-in (caar argument-list))
	(fragment-or-out (cadar argument-list))
	(frags-to-or (cdr argument-list))
	(frag-or-pairs (list)))
    (push-fragment-2 'regex-ε
		     (dolist (frag frags-to-or frag-or-pairs)
		       (push (list (second frag) fragment-or-out) frag-or-pairs)
		       (push (list fragment-or-in (first frag)) frag-or-pairs))
		     NFA-instance)))

;; (push-fragment-2 'regex-star
;;                  '((fragment-star-in fragment-star-out)
;;                    (frag-in frag-out))
;;                  NFA)
;;
;; --> (push-fragment-2 'regex-ε
;;                      '((fragment-star-in fragment-star-out)
;;                        (fragment-star-out fragment-star-in)
;;                        (fragment-star-in frag-in)
;;                        (frag-out fragment-star-out))
;;                      NFA)
;;
;; ==> NFA fragment-star-in fragment-star-out

(defmethod push-fragment-2 ((fragment-type (eql 'regex-star))
			    (argument-list list)
			    (NFA-instance NFA))
  (let ((fragment-star-in (caar argument-list))
	(fragment-star-out (cadar argument-list))
	(fragment-in (caadr argument-list))
	(fragment-out (cadadr argument-list)))
    (push-fragment-2 'regex-ε
		     (list (list fragment-star-in fragment-star-out)
			   (list fragment-star-out fragment-star-in)
			   (list fragment-star-in fragment-in)
			   (list fragment-out fragment-star-out))
		     NFA-instance)))

;; (push-fragment-2 'regex-plus
;;                  '((fragment-plus-in fragment-plus-out)
;;                    (frag-in frag-out))
;;                  NFA)
;;
;; --> (push-fragment-2 'regex-or
;;                      '((fragment-plus-in fragment-plus-out)
;;                        (frag-in frag-out))
;;                      NFA)
;;
;; ==> NFA state-begin state-end

(defmethod push-fragment-2 ((fragment-type (eql 'regex-plus))
			    (argument-list list)
			    (NFA-instance NFA))
  (push-fragment-2 'regex-or argument-list NFA-instance)) 

;; (push-fragment-2 'regex-interval
;;                  '((fragment-interval-in fragment-interval-out)
;;                    (interval-1-char-start interval-1-char-end)
;;                              ...
;;                    (interval-n-char-start interval-n-char-end))
;;                  NFA)
;;
;; --> (push-fragment-2 'regex-literal
;;                      '((fragment-interval-in fragment-interval-out)
;;                        (interval-1-char-start
;;                         interval-1-char-2
;;                            ...
;;                         interval-1-char-end
;;                            ...
;;                         interval-n-char-start
;;                         interval-n-char-2
;;                            ...
;;                         interval-n-char-end))
;;                      NFA)
;;
;; ==> NFA state-begin state-end

(defmethod push-fragment-2 ((fragment-type (eql 'regex-interval))
			    (argument-list list)
			    (NFA-instance NFA))
  (let ((start-end (car argument-list))
	(intervals-list (cdr argument-list))
	(char-list (list)))
    (push-fragment-2 'regex-literal
		     (list start-end
			   (dolist (interval intervals-list char-list)
			     (setf char-list
				   (nconc char-list
					  (char-interval->list (first interval)
							       (second interval))))))		     
		     NFA-instance)))

(defmethod char-interval->list ((char1 character) (char2 character))
  (when (char< char1 char2)
    (do ((char-iter char1 (code-char (1+ (char-code char-iter))))
	 (char-list (list) (push char-iter char-list)))
	((char> char-iter char2) (nreverse char-list)))))

;; (push-fragment-2 'regex-optional
;;                  '((fragment-optional-in fragment-optional-out)
;;                    (fragment-in fragment-out))
;;                  NFA)
;;
;; --> (push-fragment-2 'regex-ε
;;                      '((fragment-optional-in fragment-optional-out)
;;                        (fragment-optional-in fragment-in)
;;                        (fragment-out fragment-optional-out))
;;                      NFA)
;;
;; ==> NFA state-begin state-end

(defmethod push-fragment-2 ((fragment-type (eql 'regex-optional))
			    (argument-list list)
			    (NFA-instance NFA))
  (let ((fragment-optional-in (caar argument-list))
	(fragment-optional-out (cadar argument-list))
	(fragment-in (caadr argument-list))
	(fragment-out (cadadr argument-list)))
    (push-fragment-2 'regex-ε
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

(defparameter *test-regex-tree* '(conc (opt (#\+ #\-))
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
			                          (plus (inter #\0 #\9))))))


(defmethod push-fragment ((regex-list list)
			  (NFA-instance NFA)
			  &rest
			    pass-forward-args)
  (let ((regex-operation (car regex-list))
	(regex-arguments (cdr regex-list)))
    (apply #'push-fragment regex-operation NFA-instance regex-arguments)))

;; literal a
(defmethod push-fragment ((liter (eql 'liter))
			  (NFA-inst NFA)
			  &rest
			    pass-forward-args)
  (let ((liter-args (list (list 'next 'next))))
    (push-fragment-2 'regex-literal
		     (push-next-states (nconc liter-args
					      (list pass-forward-args))
				       NFA-inst)
		     NFA-inst)))

(defmethod push-fragment ((first-char character)
			  (NFA-inst NFA)
			  &rest
			    pass-forward-args)
  (push-fragment (cons 'liter (cons first-char pass-forward-args))
		 NFA-inst))

;; concatenate ab
(defmethod push-fragment ((conc (eql 'conc))
			  (NFA-inst NFA)
			  &rest
			    pass-forward-args)
  (let ((concat-args (list (list 'next 'next))))
    (push-fragment-2 'regex-concat
		     (push-next-states (dolist (arg pass-forward-args (nreverse concat-args))
					 (multiple-value-bind (NFA-inst-pushed frag-begin frag-end)
					     (push-fragment arg NFA-inst)
					   (setf NFA-inst NFA-inst-pushed)
					   (push (list frag-begin frag-end) concat-args)))
				       NFA-inst)
		     NFA-inst)))
      
;; or a|b
(defmethod push-fragment ((or (eql 'or))
			  (NFA-inst NFA)
			  &rest
			    pass-forward-args)
  (let ((or-args (list (list 'next 'next))))
    (push-fragment-2 'regex-or
		     (push-next-states (dolist (arg pass-forward-args (nreverse or-args))
					 (multiple-value-bind (NFA-inst-pushed frag-begin frag-end)
					     (push-fragment arg NFA-inst)
					   (setf NFA-inst NFA-inst-pushed)
					   (push (list frag-begin frag-end) or-args)))
				       NFA-inst)
		     NFA-inst)))

;; Kleene star (zero or more) *
(defmethod push-fragment ((star (eql 'star))
			  (NFA-inst NFA)
			  &rest
			    pass-forward-args)
  (let ((star-args (list (list 'next 'next))))
    (push-fragment-2 'regex-star
		     (push-next-states (multiple-value-bind (NFA-inst-pushed frag-begin frag-end)
					   (push-fragment pass-forward-args NFA-inst)
					 (setf NFA-inst NFA-inst-pushed)
					 (nconc star-args
						(list (list frag-begin frag-end))))
				       NFA-inst)
		     NFA-inst)))

;; plus (one or more) +
(defmethod push-fragment ((plus (eql 'plus))
			  (NFA-inst NFA)
			  &rest
			    pass-forward-args)
  (let ((plus-args (list (list 'next 'next))))
    (push-fragment-2 'regex-plus
		     (push-next-states (multiple-value-bind (NFA-inst-pushed frag-begin frag-end)
					   (push-fragment pass-forward-args NFA-inst)
					 (setf NFA-inst NFA-inst-pushed)
					 (nconc plus-args
						(list (list frag-begin frag-end))))
				       NFA-inst)
		     NFA-inst)))

;; interval (a|b|c|...|z) a-z
(defmethod push-fragment ((inter (eql 'inter))
			  (NFA-inst NFA)
			  &rest
			    pass-forward-args)
  (let ((inter-args (list (list 'next 'next))))
    (push-fragment-2 'regex-interval
		     (push-next-states (nconc inter-args (list->pairs pass-forward-args))
				       NFA-inst)
		     NFA-inst)))

(defun list->pairs (source-list)
  (labels ((pairs-iter (old-list new-list)
		       (if (second old-list)
			   (pairs-iter (cddr old-list)
				       (push (list (first old-list) (second old-list))
					     new-list))
			   new-list)))
	  (pairs-iter source-list (list))))
  
;; optional ?
(defmethod push-fragment ((opt (eql 'opt))
			  (NFA-inst NFA)
			  &rest
			    pass-forward-args)
  (let ((opt-args (list (list 'next 'next))))
    (push-fragment-2 'regex-optional
		     (push-next-states (multiple-value-bind (NFA-inst-pushed frag-begin frag-end)
					   (push-fragment pass-forward-args NFA-inst)
					 (setf NFA-inst NFA-inst-pushed)
					 (nconc opt-args
						(list (list frag-begin frag-end))))
				       NFA-inst)
		     NFA-inst)))







