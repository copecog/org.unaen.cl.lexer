;;;; dfa.lisp


(in-package #:org.unaen.cl.lexer)

;;(declaim (optimize (speed 3) (safety 0)))

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
