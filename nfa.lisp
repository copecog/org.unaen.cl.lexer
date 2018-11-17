(in-package #:org.unaen.src.cl.lexer)

;;(declaim (optimize (speed 3) (safety 0)))


;;; **** NFA Specific Atomic Operations *****************************************

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


;;; **** NFA Construction *******************************************************

;; begin-state[transit-char]-->end-state
;;
;; (push-fragment-2 'regex-literal
;;                  '((fragment-literal-in fragment-literal-out)
;;                    (char-1 char-2 ... char-n))
;;                  NFA)
;;
;; ==> NFA fragment-literal-in fragment-literal-out
;;
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

;; begin-state[ε]-->end-state
;;
;; (push-fragment-2 'regex-ε
;;                  '((state-1-in state-1-out)
;;                    (state-2-in state-2-out)
;;                      ...
;;                    (state-n-in state-n-out))
;;                  NFA)
;;
;; --> (push-fragment-2 'regex-literal '((state-1-in state-1-out) ('ε)) NFA)
;;       ...
;; ... (push-fragment-2 'regex-literal '((state-n-in state-n-out) ('ε)) NFA)
;;
;; ==> NFA state-1-in state-2-out
;;
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

;; begin[ε]-->A-in + A-out[ε]-->B-in + B-out[ε]-->end
;;
;; (push-fragment-2 'regex-concat
;;                  '((fragment-concat-in fragment-concat-out)
;;                    (frag-1-in frag-2-out)
;;                      ...
;;                    (frag-n-in frag-n-out))
;;                  NFA)
;;
;; --> (push-fragment-2 'regex-ε
;;                      '((fragment-concat-in frag-1-in)
;;                        (frag-2-out frag-3-in)
;;                          ...
;;                        (frag-n-out fragment-concat-out))
;;                      NFA)
;;
;; ==> NFA fragment-concat-in fragment-concat-out
;;
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

;; begin[ε] -->A-in A-out[ε]--> ||
;;    ||    -->B-in B-out[ε]--> end
;;
;; (push-fragment-2 'regex-or
;;                  '((fragment-or-in fragment-or-out)
;;                    (frag-1-in frag-1-out)
;;                      ...
;;                    (frag-n-in frag-n-out))
;;                  NFA)
;;
;; --> (push-fragment-2 'regex-ε
;;                      '((fragment-or-in frag-1-in)
;;                          ...
;;                        (fragment-or-in frag-n-in)
;;                        (frag-1-out fragment-or-out)
;;                          ...
;;                        (frag-n-out fragment-or-out))
;;                      NFA)
;;
;; ==> NFA fragment-or-in fragment-or-out
;;
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

;; begin*[ε]-->A-in A-out[ε]-->begin*
;; 
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
;;
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
;;
;;(defmethod push-fragment-2 ((fragment-type (eql 'regex-plus)) (argument-list list) (NFA-inst NFA))
;;  (push-fragment-2 'regex-or argument-list NFA-inst))

(defmethod push-fragment-2 ((fragment-type (eql 'regex-plus)) (argument-list list) (NFA-inst NFA))
  (let ((fragment-plus-in (caar argument-list))
	(fragment-plus-out (cadar argument-list))
	(fragment-in (caadr argument-list))
	(fragment-out (cadadr argument-list)))
    (push-fragment-2 'regex-ε
		     (list (list fragment-plus-in fragment-in)
			   (list fragment-plus-out fragment-plus-in)
			   (list fragment-out fragment-plus-out))
		     NFA-inst)))

;; (push-fragment-2 'regex-interval
;;                  '((fragment-interval-in fragment-interval-out)
;;                    (interval-1-char-start interval-1-char-end)
;;                      ...
;;                    (interval-n-char-start interval-n-char-end))
;;                  NFA)
;;
;; --> (push-fragment-2 'regex-literal
;;                      '((fragment-interval-in fragment-interval-out)
;;                        (interval-1-char-start interval-1-char-2 ... interval-1-char-end
;;                          ...
;;                         interval-n-char-start interval-n-char-2 ... interval-n-char-end))
;;                      NFA)
;;
;; ==> NFA state-begin state-end
;;
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
;;
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

;;; push-fragment 'evaluates' a regex tree by mapping each expression push-fragment-2

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


;;; **** NFA Toplevel Function **************************************************

(defmethod regex-tree->nfa ((regex-expr-tree list))
  (multiple-value-bind (nfa-inst start-state end-state)
      (push-fragment regex-expr-tree (make-instance 'nfa))
    (with-FA-slots NFA-inst
      (let ((start-name (aref NFA-inst.Q start-state))
	    (end-name (aref NFA-inst.Q end-state)))
	(setf nfa-inst.regex-tree regex-expr-tree)
	(push start-name nfa-inst.q0-prev)
	(setf nfa-inst.q0 start-name)
	(setf (aref nfa-inst.F end-state) end-name)
	(values nfa-inst start-state end-state)))))


