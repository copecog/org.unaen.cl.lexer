(in-package #:lexer)

;;(declaim (optimize (speed 3) (safety 0)))


;;; **** Finite Automata Atomic Operations **************************************

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
    (format nil "~a~d" state-names-inst.preface (1- (incf state-names-inst.iterate)))))

(defmethod push-state-2 (state (FA-inst FA) Δ-p start-p final-p)
  (with-FA-slots FA-inst
    (vector-push-extend state FA-inst.Q)
    (when Δ-p
      (vector-push-extend (make-Δ FA-inst.Σ) FA-inst.Δ))
    (when start-p
      (push state FA-inst.q0-prev)
      (setf FA-inst.q₀ state))
    (if final-p
	(vector-push-extend state FA-inst.F)
	(vector-push-extend nil FA-inst.F))
    (values FA-inst
	    (1- (fill-pointer FA-inst.Q)))))

(defmethod push-state ((next (eql 'next)) (FA-inst FA) &key (Δ-p t) (start-p nil) (final-p nil)
		       &allow-other-keys)
  (with-FA-slots FA-inst
    (push-state-2 (make-state-name FA-inst.dsn)
		  FA-inst
		  Δ-p
		  start-p
		  final-p)))

(defmethod push-state ((state-name string) (FA-inst FA) &key (Δ-p t) (start-p nil) (final-p nil)
		       &allow-other-keys)
  (push-state-2 state-name FA-inst Δ-p start-p final-p))

(defmethod push-state ((state-list list) (FA-inst FA) &key (Δ-p t) (start-p nil) (final-p nil)
		       &allow-other-keys)
  (push-state-2 state-list FA-inst Δ-p start-p final-p))

(defmethod push-state ((state (eql nil)) (FA-inst FA) &key &allow-other-keys)
  (values FA-inst nil))

(defmethod push-state ((state integer) (FA-inst FA) &key &allow-other-keys)
  (with-FA-slots FA-inst
    (values FA-inst (if (<= state (fill-pointer FA-inst.Q))
			state
			(error "State not within existing states.")))))

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

;; integer -> name;  name -> integer;  list -> integer 
(defmethod get-state ((state-name string) (FA-inst FA))
  (with-FA-slots FA-inst
    (find-name-iter state-name
		    FA-inst.Q
		    0)))

(defmethod get-state ((state-list list) (FA-inst FA))
  (with-FA-slots FA-inst
    (find-name-iter state-list
		    FA-inst.Q
		    0)))

(defmethod get-state ((state integer) (FA-inst FA))
  (with-FA-slots FA-inst
    (if (<= state (fill-pointer FA-inst.Q))
			(aref FA-inst.Q state)
			(error "State not within existing states."))))

(defmethod get-state ((state (eql nil)) (FA-inst FA))
  nil)

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

;; state-names  ->  preface  iterate
;; FA           ->  Q  Σ  Σ-in-use  Δ  q₀  q0-prev  F  dsn
(defmethod traverse-copy-FA ((FA-inst FA))
  (let ((fa-copy (make-instance 'FA
				 :Q 'Q
				 :Σ 'Σ
				 :Σ-in-use 'Σ-in-use
				 :Δ 'Δ
				 :q0 'q0
				 :q0-prev 'q0-prev
				 :F 'F
				 :dsn 'dsn)))))


				 
