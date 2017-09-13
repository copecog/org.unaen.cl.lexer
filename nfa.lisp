(in-package #:lexer)

;;(declaim (optimize (speed 3) (safety 0)))

;;; NFA Specific Utility Functions

;; Cons up a new parameter list with all of the new states "solidified".
(defmethod push-next-states ((states-tree list) (FA-inst FA))
  (cons (push-next-states (car states-tree) FA-inst)
        (push-next-states (cdr states-tree) FA-inst)))

(defmethod push-next-states ((next (eql 'next)) (FA-inst FA))
  (multiple-value-bind (FA-inst new-state)
      (push-state 'next FA-inst)
    (declare (ignore FA-inst))
    new-state))

(defmethod push-next-states ((next (eql nil)) (FA-inst FA))
  nil)

(defmethod push-next-states (next (FA-inst FA))
  next)

(defmethod push-transit-2 ((state-A integer)
			   (state-B integer)
			   (ε (eql 'ε))
			   (NFA-inst NFA)
			   (Σ (eql 'cl-utf)))
  (with-FA-slots NFA-inst
    (pushnew state-B
	     (gethash ε
		      (aref NFA-inst.Δ state-A)
		      nil))
    NFA-inst))

(defun truth (ignored-var)
  (declare (ignore ignored-var))
  t)

(defun false (ignored-var)
  (declare (ignore ignored-var))
  nil)

(defmethod push-state-new ((state-list list) (FA-inst FA) &key (final-p #'false) &allow-other-keys)
  (or (get-state state-list FA-inst)
      (multiple-value-bind (FA-inst state-int)
	  (push-state state-list
		      FA-inst
		      :final-p (funcall final-p state-list))
	(declare (ignore FA-inst))
	state-int)))

(defmethod push-state-new ((state-list (eql nil)) (FA-inst FA) &key &allow-other-keys)
  nil)

;;; NFA Building Functions

(defmethod push-fragment-2 ((fragment-type (eql 'regex-literal)) (argument-list list) (NFA-inst NFA))
  (let ((fragment-literal-in (caar argument-list))
	(fragment-literal-out (cadar argument-list))
	(transit-character-list (cadr argument-list)))
    (multiple-value-bind (NFA-inst fragment-literal-in)
	(push-state fragment-literal-in
		    NFA-inst)
      (multiple-value-bind (NFA-inst fragment-literal-out)
	  (push-state fragment-literal-out
	              NFA-inst)
	(values
	 (dolist (transit-character transit-character-list NFA-inst)
	   (setf NFA-inst (push-transit fragment-literal-in
					fragment-literal-out
					transit-character
					NFA-inst)))
	 fragment-literal-in
	 fragment-literal-out)))))

(defmethod push-fragment-2 ((fragment-type (eql 'regex-ε)) (argument-list list) (NFA-inst NFA))
  (let ((fragment-ε-in-1st nil)
	(fragment-ε-out-n nil))
    (dolist (state-pair argument-list)
      (multiple-value-bind (NFA-inst-m fragment-ε-in-m fragment-ε-out-m)
	  (push-fragment-2 'regex-literal (list state-pair (list 'ε)) NFA-inst)
	(unless fragment-ε-in-1st
	  (setf fragment-ε-in-1st fragment-ε-in-m))
	(setf fragment-ε-out-n fragment-ε-out-m)
	(setf NFA-inst NFA-inst-m)))
    (values NFA-inst fragment-ε-in-1st fragment-ε-out-n)))

(defmethod push-fragment-2 ((fragment-type (eql 'regex-concat)) (argument-list list) (NFA-inst NFA))
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
			 NFA-inst)))))

(defmethod push-fragment-2 ((fragment-type (eql 'regex-or)) (argument-list list) (NFA-inst NFA))
  (let ((fragment-or-in (caar argument-list))
	(fragment-or-out (cadar argument-list))
	(frags-to-or (cdr argument-list))
	(frag-or-pairs (list)))
    (push-fragment-2 'regex-ε
		     (dolist (frag frags-to-or frag-or-pairs)
		       (push (list (second frag) fragment-or-out) frag-or-pairs)
		       (push (list fragment-or-in (first frag)) frag-or-pairs))
		     NFA-inst)))

(defmethod push-fragment-2 ((fragment-type (eql 'regex-star)) (argument-list list) (NFA-inst NFA))
  (let ((fragment-star-in (caar argument-list))
	(fragment-star-out (cadar argument-list))
	(fragment-in (caadr argument-list))
	(fragment-out (cadadr argument-list)))
    (push-fragment-2 'regex-ε
		     (list (list fragment-star-in fragment-star-out)
			   (list fragment-star-out fragment-star-in)
			   (list fragment-star-in fragment-in)
			   (list fragment-out fragment-star-out))
		     NFA-inst)))

(defmethod push-fragment-2 ((fragment-type (eql 'regex-plus)) (argument-list list) (NFA-inst NFA))
  (push-fragment-2 'regex-or argument-list NFA-inst)) 

(defmethod push-fragment-2 ((fragment-type (eql 'regex-interval)) (argument-list list) (NFA-inst NFA))
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
		     NFA-inst)))

(defmethod push-fragment-2 ((fragment-type (eql 'regex-optional)) (argument-list list) (NFA-inst NFA))
  (let ((fragment-optional-in (caar argument-list))
	(fragment-optional-out (cadar argument-list))
	(fragment-in (caadr argument-list))
	(fragment-out (cadadr argument-list)))
    (push-fragment-2 'regex-ε
		     (list (list fragment-optional-in fragment-optional-out)
			   (list fragment-optional-in fragment-in)
			   (list fragment-out fragment-optional-out))
		     NFA-inst)))

(defmethod push-fragment ((regex-list list) (NFA-inst NFA) &rest pass-forward-args)
  (let ((regex-operation (car regex-list))
	(regex-arguments (cdr regex-list)))
    (apply #'push-fragment regex-operation NFA-inst regex-arguments)))

(defmethod push-fragment ((liter (eql 'liter)) (NFA-inst NFA) &rest pass-forward-args)
  (let ((liter-args (list (list 'next 'next))))
    (push-fragment-2 'regex-literal
		     (push-next-states (nconc liter-args
					      (list pass-forward-args))
				       NFA-inst)
		     NFA-inst)))

(defmethod push-fragment ((first-char character) (NFA-inst NFA) &rest pass-forward-args)
  (push-fragment (cons 'liter (cons first-char pass-forward-args))
		 NFA-inst))

(defmethod push-fragment ((conc (eql 'conc)) (NFA-inst NFA) &rest pass-forward-args)
  (let ((concat-args (list (list 'next 'next))))
    (push-fragment-2 'regex-concat
		     (push-next-states (dolist (arg pass-forward-args (nreverse concat-args))
					 (multiple-value-bind (NFA-inst-pushed frag-begin frag-end)
					     (push-fragment arg NFA-inst)
					   (setf NFA-inst NFA-inst-pushed)
					   (push (list frag-begin frag-end) concat-args)))
				       NFA-inst)
		     NFA-inst)))
      
(defmethod push-fragment ((or (eql 'or)) (NFA-inst NFA) &rest pass-forward-args)
  (let ((or-args (list (list 'next 'next))))
    (push-fragment-2 'regex-or
		     (push-next-states (dolist (arg pass-forward-args (nreverse or-args))
					 (multiple-value-bind (NFA-inst-pushed frag-begin frag-end)
					     (push-fragment arg NFA-inst)
					   (setf NFA-inst NFA-inst-pushed)
					   (push (list frag-begin frag-end) or-args)))
				       NFA-inst)
		     NFA-inst)))

(defmethod push-fragment ((star (eql 'star)) (NFA-inst NFA) &rest pass-forward-args)
  (let ((star-args (list (list 'next 'next))))
    (push-fragment-2 'regex-star
		     (push-next-states (multiple-value-bind (NFA-inst-pushed frag-begin frag-end)
					   (push-fragment pass-forward-args NFA-inst)
					 (setf NFA-inst NFA-inst-pushed)
					 (nconc star-args
						(list (list frag-begin frag-end))))
				       NFA-inst)
		     NFA-inst)))

(defmethod push-fragment ((plus (eql 'plus)) (NFA-inst NFA) &rest pass-forward-args)
  (let ((plus-args (list (list 'next 'next))))
    (push-fragment-2 'regex-plus
		     (push-next-states (multiple-value-bind (NFA-inst-pushed frag-begin frag-end)
					   (push-fragment pass-forward-args NFA-inst)
					 (setf NFA-inst NFA-inst-pushed)
					 (nconc plus-args
						(list (list frag-begin frag-end))))
				       NFA-inst)
		     NFA-inst)))

(defmethod push-fragment ((inter (eql 'inter)) (NFA-inst NFA) &rest pass-forward-args)
  (let ((inter-args (list (list 'next 'next))))
    (push-fragment-2 'regex-interval
		     (push-next-states (nconc inter-args (list->pairs pass-forward-args))
				       NFA-inst)
		     NFA-inst)))

(defmethod push-fragment ((opt (eql 'opt)) (NFA-inst NFA) &rest pass-forward-args)
  (let ((opt-args (list (list 'next 'next))))
    (push-fragment-2 'regex-optional
		     (push-next-states (multiple-value-bind (NFA-inst-pushed frag-begin frag-end)
					   (push-fragment pass-forward-args NFA-inst)
					 (setf NFA-inst NFA-inst-pushed)
					 (nconc opt-args
						(list (list frag-begin frag-end))))
				       NFA-inst)
		     NFA-inst)))

(defmethod regex-tree->nfa ((regex-expr-tree list))
  (multiple-value-bind (nfa-inst start-state end-state)
      (push-fragment regex-expr-tree (make-instance 'nfa))
    (setf (slot-value nfa-inst 'q0) (get-state start-state nfa-inst))
    (setf (aref (F nfa-inst) end-state) (get-state end-state nfa-inst))
    (values nfa-inst start-state end-state)))


