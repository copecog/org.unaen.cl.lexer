(in-package #:lexer)

;;(declaim (optimize (speed 3) (safety 0)))

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
  (setf (slot-value FA-dest 'q₀)
	(get-state (get-state (q₀ FA-src)
			      FA-src)
		   FA-dest))
  FA-dest)

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
    (let* ((group (get-state a-state-group DFA-inst))
	   (first-state (first group))
	   (rest-of-group (cdr group)))
      (dolist (state rest-of-group)
	(unless (state-equal DFA-inst
			     first-state
			     state
			     :state-groups state-groups)
	  (return-from group-consistent-p nil))))))

(defmethod minimize-states ((DFA-inst DFA) (state-groups list))
  (labels ((minimize-states-iter (DFA-inst state-groups)
	     (if (group-consistent-p DFA-inst state-groups)
		 `(push-transits-for-groups ,state-groups ,DFA-inst)
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

(defmethod group-consistent ((DFA-inst DFA) (state-groups list))
    (group-consistent-iter DFA-inst
			   state-groups
			   (mapcar #'(lambda (x) (get-state x DFA-inst)) state-groups)
			   (list)))

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
				 DFA-inst))))

;(defmethod push-group-states ((state-groups list) (DFA-inst DFA))
;  (let ((pushed-state-groups (list)))
;    (dolist (a-state-group state-groups pushed-state-groups)
;      (multiple-value-bind (DFA-inst pushed-state)
;	  (push-state a-state-group DFA-inst)
;	(declare (ignore DFA-inst))
;	(push pushed-state pushed-state-groups)))))

(defmethod push-group-states ((state-groups list) (DFA-inst DFA))
  (loop :for a-state-group :in state-groups
	:collect (multiple-value-bind (DFA-inst pushed-state)
		     (push-state a-state-group DFA-inst)
		   (declare (ignore DFA-inst))
		   pushed-state)))

(defmethod DFA-min-map->DFA ((DFA-inst DFA))
  nil)

;(defmethod push-group-transits ((DFA-inst DFA) (state-groups list) &key (map-start t) (map-finals t))
;  (loop :for a-state-group :in state-groups
;	:if (cdr a-state-group) :do
;	  ))

;; state-names  ->  preface  iterate
;; FA           ->  Q  Σ  Σ-in-use  Δ  q₀  F  dsn

