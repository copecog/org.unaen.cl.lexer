(in-package #:lexer)

;;(declaim (optimize (speed 3) (safety 0)))

(defparameter *test-regex-tree-1* '(conc (opt (#\+ #\-))
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

(defparameter *test-regex-tree-2*
  '(conc (#\a #\b #\c)
         (#\1 #\2 #\3)))

(defparameter *test-regex-tree-3*
  '(conc (star (#\a #\b))
         (#\a)
         (#\c)))

(defclass state-names ()
  ((preface :initarg :preface
	    :initform "q_"
	    :reader preface)
   (iterate :initarg :iterate
	    :initform 0
	    :reader iterate)))

(defclass FA ()
  ((Q :initarg :Q
      :initform (make-state-vector 1)
      :reader Q)
   (Σ :initarg :Σ
      :initform 'cl-utf
      :reader Σ)
   (Σ-in-use :initarg :Σ-in-use
	     :initform (list)
	     :reader Σ-in-use)
   (Δ :initarg :Δ
      :initform (make-state-vector 1)
      :reader Δ)
   (q₀ :initarg :q₀
       :reader q₀)
   (F :initarg :F
      :initform (make-state-vector 1)
      :reader F)
   (dsn :initarg dsn
	:initform (make-instance 'state-names)
	:reader dsn)))

(defclass NFA (FA) ())

(defclass DFA (FA) ())

(defgeneric make-state-vector (size &key &allow-other-keys))

(defgeneric make-state-name (state-names))

(defgeneric make-Δ (Σ-type))

(defgeneric push-state (state finite-automaton &key &allow-other-keys))

(defgeneric push-next-states (states-in-tree FA))

(defgeneric push-transit-2 (state-A state-B transit-char FA Σ))

(defgeneric delete-transit (state-A state-B transit-char FA))

(defgeneric get-transit (state transit-char Δ))

(defgeneric ε-closure (state NFA))

(defgeneric get-state (state Q))

(defgeneric find-name-equal (thing1 thing2))

(defgeneric get-Δ (state Δ))

(defgeneric get-all-transit (transit-char Δ))

(defgeneric push-state-new (state FA &key &allow-other-keys))

(defgeneric push-fragment-2 (fragment-type specifications-list NFA))

(defgeneric push-fragment (regex-fragment-tree NFA &rest pass-forward-args))

(defgeneric char-interval->list (char-start char-end))

(defgeneric list->pairs (source-list))

(defgeneric regex-tree->nfa (regex-expr-tree))

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

(defmethod make-state-name ((state-names-instance state-names))
  (with-slots (preface iterate) state-names-instance
    (format nil "~a~d" preface (1- (incf iterate)))))

(defmethod push-state-2 (state (FA-inst FA) Δ-p start-p final-p)
  (with-slots (Q Σ Δ q₀ F) FA-inst
    (vector-push-extend state Q)
    (when Δ-p
      (vector-push-extend (make-Δ Σ) Δ))
    (when start-p
      (setf q₀ state))
    (if final-p
	(vector-push-extend state F)
	(vector-push-extend nil F))
    (values FA-inst
	    (1- (fill-pointer Q)))))

(defmethod push-state ((next (eql 'next)) (FA-inst FA) &key (Δ-p t) (start-p nil) (final-p nil)
		       &allow-other-keys)
  (push-state-2 (make-state-name (dsn FA-inst))	FA-inst	Δ-p start-p final-p))

(defmethod push-state ((state-name string) (FA-inst FA) &key (Δ-p t) (start-p nil) (final-p nil)
		       &allow-other-keys)
  (push-state-2 state-name FA-inst Δ-p start-p final-p))

(defmethod push-state ((state-list list) (FA-inst FA) &key (Δ-p t) (start-p nil) (final-p nil)
		       &allow-other-keys)
  (push-state-2 state-list FA-inst Δ-p start-p final-p))

(defmethod push-state ((state (eql nil)) (FA-inst FA) &key &allow-other-keys)
  (values FA-inst nil))

(defmethod push-state ((state integer) (FA-inst FA) &key &allow-other-keys)
  (values FA-inst (when (<= state (fill-pointer (slot-value FA-inst 'Q)))
		    state)))

;;; Cons up a new parameter list with all of the new states "solidified".
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
			   (transit-char character)
			   (FA-inst FA)
			   (Σ (eql 'cl-utf)))
  (with-slots (Δ Σ-in-use) FA-inst
    (pushnew state-B
	     (gethash transit-char (aref Δ state-A) nil))
    (pushnew transit-char
	     Σ-in-use)
    FA-inst))

(defmethod push-transit-2 ((state-A integer)
			   (state-B integer)
			   (ε (eql 'ε))
			   (FA-inst FA)
			   (Σ (eql 'cl-utf)))
  (with-slots (Δ Σ-in-use) FA-inst
    (pushnew state-B
	     (gethash ε (aref Δ state-A) nil))
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
  (gethash transit-char (aref (slot-value FA-inst 'Δ) state)))

(defmethod get-transit ((state integer)
			(transit-char character)
			(FA-inst FA))
  (get-transit-2 state transit-char FA-inst))

;;(defmethod get-transit ((states-in list)
;;			(transit-char character)
;;			(FA-inst FA))
;;(let ((states-out (list)))
;;  (dolist (state states-in states-out)
;;    (setf states-out (nunion (get-transit state transit-char FA-inst)
;;			       states-out)))))

(defun ε-closure-2 (states-in states-out Δ-inst transit-char)
  (let ((state (pop states-in)))
    (if state
	(if (member state states-out)
	    (ε-closure-2 states-in states-out Δ-inst transit-char)
	    (ε-closure-2 (append states-in (get-transit-2 state transit-char Δ-inst))
			 (push state states-out)
			 Δ-inst
			 transit-char))
	states-out)))

;; ==> (state-integer-a state-integer-b ... state-integer-n)
(defmethod ε-closure ((state integer)
		      (NFA-inst NFA))
  (ε-closure-2 (list state) (list) NFA-inst 'ε))

(defmethod ε-closure ((states list)
		      (NFA-inst NFA))
  (ε-closure-2 states (list) NFA-inst 'ε))

(defun find-name-iter (state-name Q cell-iter)
  (if (< cell-iter (fill-pointer Q))
      (if (find-name-equal state-name
			   (aref Q cell-iter))
	  cell-iter
	  (find-name-iter state-name Q (1+ cell-iter)))))

;; integer -> name;  name -> integer;  list -> integer 
(defmethod get-state ((state-name string)
		      (FA-inst FA))
  (find-name-iter state-name (slot-value FA-inst 'Q) 0))

(defmethod get-state ((state-list list)
		      (FA-inst FA))
  (find-name-iter state-list (slot-value FA-inst 'Q) 0))

(defmethod get-state ((state integer)
		      (FA-inst FA))
  (aref (slot-value FA-inst 'Q) state))

(defmethod get-state ((state (eql nil))
		      (FA-inst FA))
  nil)

(defmethod find-name-equal ((thing1 integer)
			    (thing2 integer))
  (equal thing1 thing2))

(defmethod find-name-equal ((thing1 string)
			    (thing2 string))
  (equal thing1 thing2))

(defmethod find-name-equal ((thing1 list)
			    (thing2 list))
  (set-equal thing1 thing2))
    
;; For some debugging.
(defmethod get-Δ ((state integer)
		  (FA-inst FA))
  (aref (Δ FA-inst) state))

(defmethod get-all-transit (transit-char
			    (FA-inst FA))
  (do ((iter 0 (1+ iter))
       (state-list (list) (push (append (list iter '->) (get-transit-2 iter transit-char FA-inst))
				state-list)))
      ((>= iter (fill-pointer (Δ FA-inst))) state-list)))

(defun truth (ignored-var)
  (declare (ignore ignored-var))
  t)

(defun false (ignored-var)
  (declare (ignore ignored-var))
  nil)

(defmethod push-state-new ((state-list list)
			   (FA-inst FA)
			   &key
			     (final-p #'false)
			   &allow-other-keys)
  (or (get-state state-list FA-inst)
      (multiple-value-bind (FA-inst state-int)
	  (push-state state-list FA-inst :final-p (funcall final-p state-list))
	(declare (ignore FA-inst))
	state-int)))

(defmethod push-state-new ((state-list (eql nil))
			   (FA-inst FA)
			   &key &allow-other-keys)
  nil)

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

(defmethod push-fragment-2 ((fragment-type (eql 'regex-plus))
			    (argument-list list)
			    (NFA-instance NFA))
  (push-fragment-2 'regex-or argument-list NFA-instance)) 

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

(defmethod push-fragment ((regex-list list)
			  (NFA-instance NFA)
			  &rest
			    pass-forward-args)
  (let ((regex-operation (car regex-list))
	(regex-arguments (cdr regex-list)))
    (apply #'push-fragment regex-operation NFA-instance regex-arguments)))

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

(defmethod push-fragment ((inter (eql 'inter))
			  (NFA-inst NFA)
			  &rest
			    pass-forward-args)
  (let ((inter-args (list (list 'next 'next))))
    (push-fragment-2 'regex-interval
		     (push-next-states (nconc inter-args (list->pairs pass-forward-args))
				       NFA-inst)
		     NFA-inst)))

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

(defmethod char-interval->list ((char1 character) (char2 character))
  (when (char< char1 char2)
    (do ((char-iter char1 (code-char (1+ (char-code char-iter))))
	 (char-list (list) (push char-iter char-list)))
	((char> char-iter char2) (nreverse char-list)))))

(defmethod list->pairs ((source-list list))
  (labels ((pairs-iter (old-list new-list)
		       (if (second old-list)
			   (pairs-iter (cddr old-list)
				       (push (list (first old-list) (second old-list))
					     new-list))
			   new-list)))
	  (pairs-iter source-list (list))))

;; I need to make push-fragment accept :start-p and :end-p
(defmethod regex-tree->nfa ((regex-expr-tree list))
  (multiple-value-bind (nfa-inst start-state end-state)
      (push-fragment regex-expr-tree (make-instance 'nfa))
    (setf (slot-value nfa-inst 'q0) (get-state start-state nfa-inst))
    (setf (aref (F nfa-inst) end-state) (get-state end-state nfa-inst))
    (values nfa-inst start-state end-state)))

;; Part of hack to check final states when converting NFA to DFA.
(defmethod is-NFA-final-p ((states-final sequence))
  #'(lambda (states-check)
      (reduce #'(lambda (x y) (or x y))
	      states-check
	      :key #'(lambda (x) (aref states-final x)))))

(defun NFA->DFA-iter (NFA-inst Q-map state-iter)
  (cond ((< state-iter (fill-pointer (Q Q-map)))
	 (dolist (transit-char (Σ-in-use NFA-inst))
	   (push-transit state-iter
			 (push-state-new (ε-closure (mappend #'(lambda (x)
								 (get-transit x
									      transit-char
									      NFA-inst))
							     (aref (Q Q-map)
								   state-iter))
						    NFA-inst)
					 Q-map
					 :final-p (is-NFA-final-p (F NFA-inst)))
			 transit-char
			 Q-map))
	 (NFA->DFA-iter NFA-inst Q-map (1+ state-iter)))
	(t Q-map)))

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

(defmethod DFA-map->DFA ((DFA-map DFA))
  (let ((DFA-inst (make-instance 'DFA
				 :Σ (Σ DFA-map)
				 :Σ-in-use (Σ-in-use DFA-map)
				 :Δ (Δ DFA-map))))
    (map-start-state DFA-map
		     (map-states DFA-map
				 DFA-inst
				 :push-Δ nil))))
      
(defmethod map-states :before ((FA-src FA) (FA-dest FA) &key (push-Δ t))
  (if (not (= (fill-pointer (Q FA-src))
	      (fill-pointer (Δ FA-src))
	      (fill-pointer (F FA-src))))
      (error "Inconsistent States!")
      (map 'nil
	   #'(lambda (final-p)
	       (push-state 'next
			   FA-dest
			   :Δ-p push-Δ
			   :final-p final-p))
	   (F FA-src))))

(defmethod map-states ((FA-src FA) (FA-dest FA) &key &allow-other-keys)
  FA-dest)

(defmethod map-start-state ((FA-src FA) (FA-dest FA))
  (setf (slot-value FA-dest 'q₀)
	(get-state (get-state (q₀ FA-src)
			      FA-src)
		   FA-dest))
  FA-dest)
  
;; Dirty--make list of slot-values to quickly examine FA state. 
(defmethod list-fa ((fa-inst fa))
  (list (list 'Q '-> (Q fa-inst))
	(list 'Σ '-> (Σ fa-inst))
	(list 'Σ-in-use '-> (Σ-in-use fa-inst))
	(list 'Δ '-> (Δ fa-inst))
	(list 'q₀ '-> (q₀ fa-inst))
	(list 'F '-> (F fa-inst))))

(defmethod vector->list-indices-nil/t ((v vector))
  (loop :for x :across v
	:for i :from 0
	:if x :collect i :into a
	  :else :collect i :into b
	:finally (return
		   (values b a))))

(defmethod hash-table->key-value-tree ((hashy hash-table) &key (map-value #'identity))
  (let ((key-value-list (list)))
    (maphash #'(lambda (key value)
		 (push (cons key (funcall map-value value))
		       key-value-list))
	     hashy)
    key-value-list))

(defmethod state= ((DFA-inst DFA) (state-A integer) (state-B integer) &rest more-states)
  (with-slots (Δ) DFA-inst
    (apply #'state=
	   DFA-inst
	   (aref Δ state-A)
	   (aref Δ state-B)
	   more-states)))

(defmethod state= ((DFA-inst DFA) (state-A hash-table) (state-B hash-table) &rest more-states)
  (apply #'state=
	 DFA-inst
	 (hash-table->key-value-tree state-A)
	 (hash-table->key-value-tree state-B)
	 more-states))   
	   
(defmethod state= ((DFA-inst DFA) (state-A list) (state-B list) &rest more-states)
  (let ((truth-so-far (set-equal state-A state-B :test #'equal)))
    (when truth-so-far
      (if more-states
	  (apply #'state=
		 DFA-inst
		 state-B
		 (car more-states)
		 (cdr more-states))
	  truth-so-far))))

(defmethod state= ((DFA-inst DFA) (state-A list) (state-B integer) &rest more-states)
  (with-slots (Δ) DFA-inst
    (apply #'state=
	   DFA-inst
	   state-A
	   (aref Δ state-B)
	   more-states)))

(defmethod state= ((DFA-inst DFA) (state-A list) (state-B hash-table) &rest more-states)
  (apply #'state=
	 DFA-inst
	 state-A
	 (hash-table->key-value-tree state-B)
	 more-states))

(defmethod DFA->DFA-min-map ((DFA-inst DFA))
  (multiple-value-bind (non-final-states final-states)
      (vector->list-indices-nil/t (F DFA-inst))
    (multiple-value-bind (DFA-inst non-final-states) 
	(push-state non-final-states DFA-inst)
      (multiple-value-bind (DFA-inst final-states)
	  (push-state final-states DFA-inst)
	nil))))

(defun DFA-min-map-iter (DFA-inst group-states)
  nil)

(defmethod group-states-consistent-p ((DFA-inst DFA) (states-group list))
  (maphash #'(lambda (transit-states) nil) nil) nil)

