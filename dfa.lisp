(in-package #:lexer)

;;(declaim (optimize (speed 3) (safety 0)))

;;; **** Some NFA Specific Atomic Operations ************************************

(defmethod find-name-equal ((thing1 integer) (thing2 integer))
  (equal thing1 thing2))

(defmethod find-name-equal ((thing1 string) (thing2 string))
  (equal thing1 thing2))

(defmethod find-name-equal ((thing1 list) (thing2 list))
  (set-equal thing1 thing2))

(defun find-name-iter (state-name Q cell-iter)
  (if (< cell-iter (fill-pointer Q))
      (if (find-name-equal state-name
			   (aref Q cell-iter))
	  cell-iter
	  (find-name-iter state-name
			  Q
			  (1+ cell-iter)))))

;; integer -> name;  name -> integer;  list -> integer 
(defmethod get-state ((state-name string) (FA-inst FA))
  (with-FA-slots FA-inst
    (find-name-iter state-name
		    FA-inst.Q
		    0)))

(defmethod get-state ((state-list list) (FA-inst FA))
  (with-FA-slots FA-inst
    (find-name-iter state-list
		    FA-inst.Q-map
		    0)))

(defmethod get-state ((state integer) (FA-inst FA))
  (with-FA-slots FA-inst
    (if (<= state (fill-pointer FA-inst.Q))
			(aref FA-inst.Q state)
			(error "State not within existing states."))))

(defmethod get-state ((state (eql nil)) (FA-inst FA))
  nil)

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
  (ε-closure-2 (list state)
	       (list)
	       NFA-inst
	       'ε))

(defmethod ε-closure ((states list) (NFA-inst NFA))
  (ε-closure-2 states
	       (list)
	       NFA-inst
	       'ε))

;; Part of hack to check final states when converting NFA to DFA.
(defmethod is-final-p ((FA-inst FA))
  (with-FA-slots FA-inst
    #'(lambda (states-check)
	(reduce #'(lambda (x y) (or x y))
		states-check
		:key #'(lambda (x) (aref FA-inst.F x))))))

(defun NFA->DFA-iter (NFA-inst DFA-map state-iter)
  (cond ((< state-iter (fill-pointer (Q-map DFA-map)))
	 (dolist (transit-char (Σ-in-use NFA-inst))
	   (push-transit state-iter
			 (push-state-new (ε-closure (mappend #'(lambda (x)
								 (get-transit x
									      transit-char
									      NFA-inst))
							     (aref (Q-map DFA-map)
								   state-iter))
						    NFA-inst)
					 DFA-map
					 :final-p (is-final-p NFA-inst))
			 transit-char
			 DFA-map))
	 (NFA->DFA-iter NFA-inst DFA-map (1+ state-iter)))
	(t DFA-map)))

(defmethod NFA->DFA-map ((NFA-inst NFA))
  (let ((DFA-map (make-instance 'DFA
				:regex-tree (regex-tree NFA-inst)
				:FA-prev NFA-inst)))
    (push-state (ε-closure (get-state (q₀ NFA-inst)
				      NFA-inst)
			   NFA-inst)
		DFA-map
		:start-p t)
    (NFA->DFA-iter NFA-inst
		   DFA-map
		   0)))

(defmethod map-states ((FA-src FA) (FA-dest FA) &key (push-Δ t))
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
	   (F FA-src)))
  FA-dest)

(defmethod map-start-state ((FA-src FA) (FA-dest FA))
  (with-FA-slots FA-src
    (with-FA-slots FA-dest
      (let ((start-name (get-state (get-state FA-src.q0 FA-src) FA-dest)))
	(push start-name FA-dest.q0-prev)
	(setf FA-dest.q0 start-name)
	FA-dest))))	

(defmethod DFA-map->DFA ((DFA-map DFA))
  (let ((DFA-inst (make-instance 'DFA
				 :Σ (Σ DFA-map)
				 :Σ-in-use (Σ-in-use DFA-map)
				 :Δ (Δ DFA-map))))
    (map-start-state DFA-map
		     (map-states DFA-map
				 DFA-inst
				 :push-Δ nil))))


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
	     (when (member state
			   (get-state a-state-group DFA-inst))
	       (return-from state->group a-state-group)))
	 state-groups)
    state))

(defmethod state-equal ((DFA-inst DFA) (state-A integer) (state-B integer) &key state-groups)
  (let ((DFA-inst.Δ.state-A (aref (slot-value DFA-inst 'Δ) state-A))
	(DFA-inst.Δ.state-B (aref (slot-value DFA-inst 'Δ) state-B)))
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

(defmethod group-consistent-p ((DFA-inst DFA) (state-groups list))
  (dolist (a-state-group state-groups t)
    (let* ((group-states (get-state a-state-group DFA-inst))
	   (first-state (first group-states))
	   (rest-of-group (cdr group-states)))
      (dolist (state rest-of-group)
	(unless (state-equal DFA-inst
			     first-state
			     state
			     :state-groups state-groups)
	  (return-from group-consistent-p nil))))))

;; In this instance state-groups is a list of each state-group list.
(defmethod push-group-states ((state-groups list) (DFA-inst DFA) &key
								   (start-p #'false)
								   (final-p #'false))
  (with-FA-slots DFA-inst
    (loop :for a-state-group :in state-groups
	  :collect (multiple-value-bind (DFA-inst pushed-state)
		       (push-state a-state-group
				   DFA-inst
				   :final-p (funcall final-p a-state-group))
		     (declare (ignore DFA-inst))
		     (when (funcall start-p a-state-group) ;only 
		       (push pushed-state DFA-inst.q0-prev))
		     pushed-state))))

;; Get string fa-inst.q0; Find integer of state from fa-inst.q; See if integer in a-state-group.
(defmethod is-start-p ((FA-inst FA))
  (with-FA-slots FA-inst
    (let ((start-state-integer (get-state FA-inst.q0 FA-inst)))
      #'(lambda (a-state-group)
	  (member start-state-integer a-state-group)))))

(defun group-consistent-iter (DFA-inst state-groups state-groups-unmarked state-groups-marked)
  (if state-groups-unmarked
      (let ((a-state-group-unmarked (car state-groups-unmarked)))
	(multiple-value-bind (a-state-group-marked a-state-group-unmarked)
	    (separate-if #'(lambda (state)
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
				 :start-p (is-start-p DFA-inst)
				 :final-p (is-final-p DFA-inst)))))

(defmethod group-consistent ((DFA-inst DFA) (state-groups list))
    (group-consistent-iter DFA-inst
			   state-groups
			   (mapcar #'(lambda (x) (get-state x DFA-inst)) state-groups)
			   (list)))

(defmethod minimize-states ((DFA-inst DFA) (state-groups list))
  (labels ((minimize-states-iter (DFA-inst state-groups)
	     (if (group-consistent-p DFA-inst state-groups)
		 (map-group-transits state-groups DFA-inst)
		 (multiple-value-bind (DFA-inst state-groups)
		     (group-consistent DFA-inst state-groups)
		   (minimize-states-iter DFA-inst state-groups)))))
    (minimize-states-iter DFA-inst state-groups)))

(defmethod DFA->DFA-min-map ((DFA-inst DFA))
  (let ((DFA-inst.F (slot-value DFA-inst 'F)))
    (multiple-value-bind (non-final-states final-states)
	(vector->list-indices-nil/t DFA-inst.F)
      (multiple-value-bind (DFA-inst state-of-non-final-states) 
	  (push-state non-final-states DFA-inst)
	(multiple-value-bind (DFA-inst state-of-final-states)
	    (push-state final-states DFA-inst)
	  (minimize-states DFA-inst
			   (list state-of-non-final-states
				 state-of-final-states)))))))

(defmethod NFA->DFA ((NFA-inst NFA))
  (dfa-map->dfa (nfa->dfa-map NFA-inst)))

(defmethod regex-tree->DFA ((regex-tree list))
  (nfa->dfa (regex-tree->nfa regex-tree)))

;; state-names  ->  preface  iterate
;; FA           ->  Q  Σ  Σ-in-use  Δ  q₀  q0-prev  F  dsln
(defmethod map-group-transits ((state-groups list) (DFA-inst DFA))
  (with-FA-slots DFA-inst
    (loop :for a-state-group :in state-groups :do
      (loop :for a-group-state :in (get-state a-state-group DFA-inst) :do
	(loop :for transit-char :being :the :hash-keys :of (aref DFA-inst.Δ a-group-state)
		:using (:hash-value transit-state)
	      :do (push-transit a-state-group (state->group DFA-inst
							    transit-state
							    state-groups)
				transit-char
				DFA-inst)))))
  DFA-inst)

