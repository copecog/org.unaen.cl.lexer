;;;; classes.lisp

(defpackage #:org.unaen.cl.lexer-attic
  (:use #:cl))

(in-package #:org.unaen.cl.lexer-attic)

;;(declaim (optimize (speed 3) (safety 0)))

;; regex tree for a floating point number
;; [+-]? ( ( ([0-9]+.[0-9]∗|.[0-9]+) ([eE][+-]?[0-9]+)? ) | [0-9]+[eE][+-]?[0-9]+ )
(defparameter *test-regex-tree-1*
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
	           (plus (inter #\0 #\9))))))

;; regex tree for "[abc][123]"
(defparameter *test-regex-tree-2*
  '(conc (#\a #\b #\c)
         (#\1 #\2 #\3)))

;; regex tree for "[ab]*ac"
(defparameter *test-regex-tree-3*
  '(conc (star (#\a #\b))
         (#\a)
         (#\c)))

(defclass state-names ()
  ((preface :initarg :preface
	    :initform "q_"
	    :reader preface
	    :documentation "A unique preface for each set of states Q.")
   (iterate :initarg :iterate
	    :initform 0
	    :reader iterate
	    :documentation "Each integer used for a single state q∊Q."))
  (:documentation "An object used to contain the state of parameters for state-name generation."))

(defclass FA ()
  ((regex-tree :initarg :regex-tree
	       :initform nil
	       :reader regex-tree
	       :documentation "Regular expression object that the current FA object is derived from.")
   (FA-prev :initarg :FA-prev
	    :initform nil
	    :reader FA-prev
	    :documentation "Previous FA object that currect FA object is derived from.")
   (Q-map :initarg :Q-map
	  :initform (make-state-vector 1)
	  :reader Q-map
	  :documentation "Map from current states of Q to FA-prev states of Q.")
   (Q :initarg :Q
      :initform (make-state-vector 1)
      :reader Q
      :documentation "A finite set of states.")
   (Σ :initarg :Σ
      :initform 'cl-utf
      :reader Σ
      :documentation "A finite set of input symbols.")
   (Σ-in-use :initarg :Σ-in-use
	     :initform (list)
	     :reader Σ-in-use
	     :documentation "The effective Σ that follows from the actual symbols in the regex-tree.")
   (Δ :initarg :Δ
      :initform (make-state-vector 1)
      :reader Δ
      :documentation "A transition function Δ: Q ✕ Σ → P(Q).")
   (q₀ :initarg :q₀
       :initform nil
       :reader q₀
       :documentation "An initial (or start) state q₀∊Q.")
   (q0-prev :initarg :q0-prev
	    :initform (list)
	    :reader q0-prev
	    :documentation "The start states from previous FA-prev's.")
   (F :initarg :F
      :initform (make-state-vector 1)
      :reader F
      :documentation "The accepting or final states F ⊆ Q")
   (dsn :initarg dsn
	:initform (make-instance 'state-names)
	:reader dsn
	:documentation "The parameters (object) for which the state-name's are generated from."))
  (:documentation "An object containing the 5-tuple (Q,Σ,Δ,q₀,F) that represents a Finite Automaton as well as auxillary information according to the implementation."))

(defmacro with-FA-slots (FA-inst &rest body)
  `(with-slots ((,(intern (string-upcase (concatenate 'string
						      (symbol-name FA-inst)
						      ".regex-tree")))
		  regex-tree)
		(,(intern (string-upcase (concatenate 'string
						      (symbol-name FA-inst)
						      ".FA-prev")))
		  FA-prev)
		(,(intern (string-upcase (concatenate 'string
						      (symbol-name FA-inst)
						      ".Q-map")))
		 Q-map)
		(,(intern (string-upcase (concatenate 'string
						      (symbol-name FA-inst)
						      ".Q")))
		 Q)
		(,(intern (string-upcase (concatenate 'string
						      (symbol-name FA-inst)
						      ".Σ")))
		 Σ)
		(,(intern (string-upcase (concatenate 'string
						      (symbol-name FA-inst)
						      ".Σ-in-use")))
		 Σ-in-use)
		(,(intern (string-upcase (concatenate 'string
						      (symbol-name FA-inst)
						      ".Δ")))
		 Δ)
		(,(intern (string-upcase (concatenate 'string
						      (symbol-name FA-inst)
						      ".q0")))
		 q₀)
		(,(intern (string-upcase (concatenate 'string
						      (symbol-name FA-inst)
						      ".q0-prev")))
		 q₀-prev)
		(,(intern (string-upcase (concatenate 'string
						      (symbol-name FA-inst)
						      ".F")))
		 F)
		(,(intern (string-upcase (concatenate 'string
						      (symbol-name FA-inst)
						      ".dsn")))
		 dsn))
       ,FA-inst
     ,@body))

(defclass NFA (FA) ()
  (:documentation "A Nondeterministic Finite Automaton derived from the Finite Automaton Class."))

(defclass DFA (FA) ()
  (:documentation "A Deterministric Finite Automaton derived from the Finite Automaton Class."))

(defgeneric make-state-vector (size &key &allow-other-keys))

(defgeneric make-Δ (Σ-type))

(defgeneric make-state-name (state-names))

(defgeneric push-state-2 (state FA-inst Δ-p start-p final-p))

(defgeneric push-state (state FA-inst &key &allow-other-keys))

(defgeneric get-transit-2 (state transit-char FA-inst))

(defgeneric get-transit (state transit-char Δ-inst))

(defgeneric push-transit-2 (state-A state-B transit-char FA-inst Σ-inst))

(defgeneric push-next-states (states-in-tree FA-inst))

(defgeneric push-fragment-2 (fragment-type specifications-list NFA-inst))

(defgeneric push-fragment (regex-fragment-tree NFA-inst &rest pass-forward-args))

(defgeneric regex-tree->NFA (regex-tree))

(defgeneric find-name-equal (thing1 thing2))

(defgeneric get-state (return-value state-property FA-inst))

(defgeneric push-state-new (state FA-inst &key &allow-other-keys))

(defgeneric ε-closure (state NFA-inst))

(defgeneric is-final-p (FA-inst))

(defgeneric NFA->DFA-map (NFA-inst))

(defgeneric state->group (DFA-inst state state-groups))

(defgeneric state-equal (DFA-inst state-A state-B &key &allow-other-keys))

(defgeneric group-consistent-p (DFA-inst state-groups))

(defgeneric push-group-states (state-groups DFA-inst &key &allow-other-keys))

(defgeneric is-start-p (FA-inst))

(defgeneric group-consistent (DFA-inst state-groups))
  
(defgeneric minimize-states (DFA-inst state-groups))
  
(defgeneric DFA->DFA-min-map (DFA-inst))
  
(defgeneric NFA->DFA (NFA-inst))
  
(defgeneric regex-tree->DFA (regex-tree))

(defgeneric map-group-transits (state-groups DFA-inst))

(defgeneric get-Δ (state FA-inst))

(defgeneric get-all-transit (transit-char FA-inst))

(defgeneric list-FA (FA-inst))

(defmethod make-state-vector ((size integer) &key
					       (initial-element nil)
					       (adjustable t)
					       (fill-pointer 0)
			      &allow-other-keys)
  (make-array size
	      :initial-element initial-element
	      :adjustable adjustable
	      :fill-pointer fill-pointer))

(defmethod make-Δ ((Σ-type (eql 'cl-utf)))
  (make-hash-table :test 'eql)) ;eql for cl char's
	   
(defmethod make-state-name ((state-names-inst state-names))
  (with-slots ((state-names-inst.preface preface) (state-names-inst.iterate iterate)) state-names-inst
    (format nil "~a~d" state-names-inst.preface (org.unaen.cl.util:plusplus state-names-inst.iterate))))

(defmethod push-state-2 (state (FA-inst FA) Δ-p start-p final-p)
  (with-FA-slots FA-inst
    (vector-push-extend state FA-inst.Q-map)
    (let ((state-name (make-state-name FA-inst.dsn)))
      (vector-push-extend state-name FA-inst.Q)
      (when Δ-p
	(vector-push-extend (make-Δ FA-inst.Σ) FA-inst.Δ))
      (when start-p
	(push state-name FA-inst.q0-prev)
	(setf FA-inst.q₀ state-name))
      (if final-p
	  (vector-push-extend state-name FA-inst.F)
	  (vector-push-extend nil FA-inst.F))
      (values FA-inst
	      (1- (fill-pointer FA-inst.Q))))))

(defmethod push-state ((next (eql 'next)) (FA-inst FA) &key (Δ-p t) (start-p nil) (final-p nil)
		       &allow-other-keys)
  (with-FA-slots FA-inst
    (push-state-2 nil
		  FA-inst
		  Δ-p
		  start-p
		  final-p)))

;(defmethod push-state ((state-name string) (FA-inst FA) &key (Δ-p t) (start-p nil) (final-p nil)
;		       &allow-other-keys)
;  (push-state-2 state-name FA-inst Δ-p start-p final-p))

(defmethod push-state ((state-list list) (FA-inst FA) &key (Δ-p t) (start-p nil) (final-p nil)
		       &allow-other-keys)
  (push-state-2 state-list FA-inst Δ-p start-p final-p))

(defmethod push-state ((state (eql nil)) (FA-inst FA) &key &allow-other-keys)
  (format t "push-state nil")
  (values FA-inst nil))

(defmethod push-state ((state integer) (FA-inst FA) &key &allow-other-keys)
  (with-FA-slots FA-inst
    (values FA-inst (if (<= state (fill-pointer FA-inst.Q))
			state
			(error "State not within existing states.")))))

(defmethod push-transit-2 ((state-A integer)
			   (state-B integer)
			   (transit-char character)
			   (FA-inst FA)
			   (Σ (eql 'cl-utf)))
  (with-FA-slots FA-inst
    (pushnew state-B
	     (gethash transit-char
		      (aref FA-inst.Δ state-A)
		      nil))
    (pushnew transit-char
	     FA-inst.Σ-in-use)
    FA-inst))

(defmethod push-transit-2 ((state-A integer)
			   (state-b (eql nil))
			   transit-char
			   (FA-inst FA)
			   (Σ (eql 'cl-utf)))
  nil)

(defmacro push-transit (state-A state-B transit-char FA-inst)
  `(push-transit-2 ,state-A ,state-B ,transit-char ,FA-inst (Σ ,FA-inst)))

(defmethod get-transit-2 ((state integer) transit-char (FA-inst FA))
  (with-FA-slots FA-inst
    (gethash transit-char (aref FA-inst.Δ state))))

(defmethod get-transit ((state integer) (transit-char character) (FA-inst FA))
  (get-transit-2 state transit-char FA-inst))

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
					  (org.unaen.cl.util:char-interval->list (first interval)
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
		     (push-next-states (nconc inter-args (org.unaen.cl.util:list->pairs pass-forward-args))
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

(defmethod regex-tree->NFA ((regex-expr-tree list))
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

;;; **** Some NFA Specific Atomic Operations ************************************

(defmethod find-name-equal ((thing1 integer) (thing2 integer))
  (equal thing1 thing2))

(defmethod find-name-equal ((thing1 string) (thing2 string))
  (equal thing1 thing2))

(defmethod find-name-equal ((thing1 list) (thing2 list))
  (alexandria:set-equal thing1 thing2))

(defun find-name-iter (state-name Q cell-iter)
  (if (< cell-iter (fill-pointer Q))
      (if (find-name-equal state-name
			   (aref Q cell-iter))
	  cell-iter
	  (find-name-iter state-name
			  Q
			  (1+ cell-iter)))))

(defmethod get-state ((return-value (eql 'integer)) (state-name string) (FA-inst FA))
  (with-FA-slots FA-inst
    (find-name-iter state-name FA-inst.Q 0)))

(defmethod get-state ((return-value (eql 'integer)) (state-map list) (FA-inst FA))
  (with-FA-slots FA-inst
    (find-name-iter state-map FA-inst.Q-map 0)))
 
(defmethod get-state ((return-value (eql 'map)) (state-int integer) (FA-inst FA))
  (with-FA-slots FA-inst
    (when (< state-int (fill-pointer FA-inst.Q-map))
      (aref FA-inst.Q-map state-int))))

(defmethod push-state-new ((new-state-map list) (FA-inst FA) &key (final-p #'org.unaen.cl.util:false) &allow-other-keys)
  (let ((state-int (get-state 'integer new-state-map FA-inst)))
    (if state-int
	(values FA-inst state-int)
	(push-state new-state-map
		    FA-inst
		    :final-p (funcall final-p new-state-map)))))

(defmethod push-state-new ((state-list (eql nil)) (FA-inst FA) &key &allow-other-keys)
  (values FA-inst nil))

;;; **** NFA to DFA *************************************************************

(defun ε-closure-2 (states-in states-out NFA-inst transit-char)
  (let ((state (pop states-in)))
    (if state
	(if (member state states-out)
	    (ε-closure-2 states-in
			 states-out
			 NFA-inst
			 transit-char)
	    (ε-closure-2 (append states-in (get-transit-2 state transit-char NFA-inst))
			 (push state states-out)
			 NFA-inst
			 transit-char))
	states-out)))

;; ==> (state-integer-a state-integer-b ... state-integer-n)
(defmethod ε-closure ((state integer) (NFA-inst NFA))
  (ε-closure-2 (list state) (list) NFA-inst 'ε))

(defmethod ε-closure ((states list) (NFA-inst NFA))
  (ε-closure-2 states (list) NFA-inst 'ε))

;; Part of hack to check final states when converting NFA to DFA.
(defmethod is-final-p ((FA-inst FA))
  (with-FA-slots FA-inst
    #'(lambda (states-check)
	(reduce #'(lambda (x y) (or x y))
		states-check
		:key #'(lambda (x) (aref FA-inst.F x))))))

(defmethod NFA->DFA-map ((NFA-inst NFA))
  (labels ((NFA->DFA-iter (NFA-inst DFA-map state-iter)
	     (cond ((< state-iter (fill-pointer (Q-map DFA-map)))
		    (dolist (transit-char (Σ-in-use NFA-inst))
		      (multiple-value-bind (DFA-map state-int)
			  (push-state-new (ε-closure (alexandria:mappend #'(lambda (x)
                                                                             (get-transit x transit-char NFA-inst))
                                                                         (aref (Q-map DFA-map) state-iter))
						     NFA-inst)
					  DFA-map
					  :final-p (is-final-p NFA-inst))
			(push-transit state-iter state-int transit-char DFA-map)))
		    (NFA->DFA-iter NFA-inst DFA-map (1+ state-iter)))
		   (t DFA-map))))
  (let ((DFA-map (make-instance 'DFA
				:regex-tree (regex-tree NFA-inst)
				:FA-prev NFA-inst)))
    (push-state (ε-closure (get-state 'integer
				      (q₀ NFA-inst)
				      NFA-inst)
			   NFA-inst)
		DFA-map
		:start-p t)
    (NFA->DFA-iter NFA-inst
		   DFA-map
		   0))))

;;; **** DFA Minimizer **********************************************************
;;;
;;;  "a-group-state" is an individual state (integer) in a group below.
;;;
;;;  "group-states" is a list of (integer) states that have been found equivalent.
;;; It is one or more elements in length depending on equivalency.
;;;
;;;  "a-state-group" is a newly pushed state (integer) which itself contains 
;;; the aforementioned "group-states" list in the DFA.
;;;
;;;  "state-groups" is a list of these newly pushed "a-state-group"'s (integers).

(defmethod state->group ((DFA-inst DFA) (state list) (state-groups list))
  (let ((state (first state)))
    (map 'nil
	 #'(lambda (a-state-group)
	     (when (member state (get-state 'map a-state-group DFA-inst))
	       (return-from state->group a-state-group)))
	 state-groups)
    state))

(defmethod state-equal ((DFA-inst DFA) (state-A integer) (state-B integer) &key state-groups)
  (let ((DFA-inst.Δ.state-A (aref (slot-value (slot-value DFA-inst 'FA-prev) 'Δ) state-A))
	(DFA-inst.Δ.state-B (aref (slot-value (slot-value DFA-inst 'FA-prev) 'Δ) state-B)))
    (if (eq DFA-inst.Δ.state-A
	    DFA-inst.Δ.state-B)
	t
	(if (= (hash-table-count DFA-inst.Δ.state-A)
	       (hash-table-count DFA-inst.Δ.state-B))
	    (loop :for transit-char :being :the :hash-keys :of DFA-inst.Δ.state-A
		  :when (not (equal (state->group DFA-inst
						  (gethash transit-char DFA-inst.Δ.state-A)
						  state-groups)
				    (state->group DFA-inst
						  (gethash transit-char DFA-inst.Δ.state-B)
						  state-groups)))
		    :do (return-from state-equal nil)
		  :finally (return t))))))

;; This works because state-groups membership is transitive.
(defmethod group-consistent-p ((DFA-inst DFA) (state-groups list))
  (dolist (a-state-group state-groups t)
    (let* ((group-states (get-state 'map a-state-group DFA-inst))
	   (first-state (first group-states))
	   (rest-of-group (rest group-states)))
      (dolist (state rest-of-group)
	(unless (state-equal DFA-inst
			     first-state
			     state
			     :state-groups state-groups)
	  (return-from group-consistent-p nil))))))

;; In this instance state-groups is a list of each state-group list.
(defmethod push-group-states ((state-groups list) (DFA-inst DFA) &key
								   (start-p #'org.unaen.cl.util:false)
								   (final-p #'org.unaen.cl.util:false))
  (with-FA-slots DFA-inst
    (loop :for a-state-group :in state-groups
	  :collect (multiple-value-bind (DFA-inst pushed-state)
		       (push-state a-state-group
				   DFA-inst
				   :start-p (funcall start-p a-state-group)
				   :final-p (funcall final-p a-state-group))
		     (declare (ignore DFA-inst)) ;Need to rewrite this to always use the returned DFA-inst.
		     pushed-state))))

;; Get string fa-inst.q0; Find integer of state from fa-inst.q; See if integer in a-state-group.
(defmethod is-start-p ((FA-inst FA))
  (with-FA-slots FA-inst
    (let ((start-state-integer (get-state 'integer FA-inst.q0 FA-inst)))
      #'(lambda (a-state-group)
	  (member start-state-integer a-state-group)))))

(defmethod group-consistent ((DFA-inst DFA) (state-groups list))
  (labels ((group-consistent-iter (DFA-inst state-groups state-groups-unmarked state-groups-marked)
	     (if state-groups-unmarked
		 (let ((a-state-group-unmarked (car state-groups-unmarked)))
		   (multiple-value-bind (a-state-group-marked a-state-group-unmarked)
		       (org.unaen.cl.util:separate-if #'(lambda (state)
                                                          (state-equal DFA-inst
                                                                       (first a-state-group-unmarked)
                                                                       state
                                                                       :state-groups state-groups))
                                                      a-state-group-unmarked)
		     (group-consistent-iter DFA-inst
					    state-groups
					    (if a-state-group-unmarked
						(cons a-state-group-unmarked
						      (cdr state-groups-unmarked))
						(cdr state-groups-unmarked))
					    (push a-state-group-marked
						  state-groups-marked))))
		 (values DFA-inst
			 (push-group-states state-groups-marked
					    DFA-inst
					    :start-p (is-start-p (slot-value DFA-inst 'FA-prev))
					    :final-p (is-final-p (slot-value DFA-inst 'FA-prev)))))))
    (group-consistent-iter DFA-inst
			   state-groups
			   (mapcar #'(lambda (state-int)
				       (get-state 'map state-int DFA-inst))
				   state-groups)
			   (list))))

(defmethod minimize-states ((DFA-inst DFA) (state-groups list))
  (labels ((minimize-states-iter (DFA-inst state-groups)
	     (if (group-consistent-p DFA-inst state-groups)
		 (map-group-transits state-groups DFA-inst)
		 (multiple-value-bind (DFA-inst state-groups)
		     (group-consistent DFA-inst state-groups)
		   (minimize-states-iter DFA-inst state-groups)))))
    (minimize-states-iter DFA-inst state-groups)))

(defmethod DFA->DFA-min-map ((DFA-inst DFA))
  (with-FA-slots DFA-inst
    (let ((DFA-min (make-instance 'DFA
				  :regex-tree DFA-inst.regex-tree
				  :FA-prev DFA-inst)))
      (with-FA-slots DFA-min
	(multiple-value-bind (non-final-states final-states)
	    (org.unaen.cl.util:vector->list-indices-nil/t DFA-inst.F)
	  (multiple-value-bind (DFA-min state-of-non-final-states) 
	      (push-state non-final-states DFA-min)
	    (multiple-value-bind (DFA-min state-of-final-states)
		(push-state final-states DFA-min)
	      (minimize-states DFA-min
			       (list state-of-non-final-states
				     state-of-final-states)))))))))

(defmethod NFA->DFA ((NFA-inst NFA))
  (dfa-map->dfa (nfa->dfa-map NFA-inst)))

(defmethod regex-tree->DFA ((regex-tree list))
  (nfa->dfa (regex-tree->nfa regex-tree)))

;; state-names  ->  preface  iterate
;; FA           ->  Q  Σ  Σ-in-use  Δ  q₀  q0-prev  F  dsln
(defmethod map-group-transits ((state-groups list) (DFA-inst DFA))
  (let ((DFA-inst.FA-prev.Δ (slot-value (slot-value DFA-inst 'FA-prev) 'Δ)))
    (loop :for a-state-group :in state-groups :do
      (loop :for a-group-state :in (get-state 'map a-state-group DFA-inst) :do
	(loop :for transit-char :being :the :hash-keys :of (aref DFA-inst.FA-prev.Δ a-group-state)
		:using (:hash-value transit-state)
	      :do (push-transit a-state-group (state->group DFA-inst
							    transit-state
							    state-groups)
				transit-char
				DFA-inst)))))
  DFA-inst)


;; For some debugging.
(defmethod get-Δ ((state integer) (FA-inst FA))
  (with-FA-slots FA-inst
    (aref FA-inst.Δ state)))

(defmethod get-all-transit (transit-char (FA-inst FA))
  (with-FA-slots FA-inst
    (do ((iter 0 (1+ iter))
	 (state-list (list) (push (append (list iter '->) (get-transit-2 iter transit-char FA-inst))
				  state-list)))
	((>= iter (fill-pointer FA-inst.Δ)) state-list))))

;; state-names -> preface iterate
;; FA          -> regex-tree FA-prev Q-map Q Σ Σ-in-use Δ q₀ q0-prev F dsn
(defmethod list-fa ((fa-inst fa))
  (list (list 'regex-tree '-> (regex-tree fa-inst))
        (list 'FA-prev '-> (FA-prev fa-inst))
        (list 'Q-map '-> (Q-map fa-inst))
        (list 'Q '-> (Q fa-inst))
	(list 'Σ '-> (Σ fa-inst))
	(list 'Σ-in-use '-> (Σ-in-use fa-inst))
	(list 'Δ '-> (Δ fa-inst))
	(list 'q₀ '-> (q₀ fa-inst))
	(list 'q0-prev '-> (q0-prev fa-inst))
	(list 'F '-> (F fa-inst))))
