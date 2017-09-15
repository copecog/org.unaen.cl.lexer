(in-package #:lexer)

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

(defmethod list-fa ((fa-inst fa))
  (list (list 'Q '-> (Q fa-inst))
	(list 'Σ '-> (Σ fa-inst))
	(list 'Σ-in-use '-> (Σ-in-use fa-inst))
	(list 'Δ '-> (Δ fa-inst))
	(list 'q₀ '-> (q₀ fa-inst))
	(list 'q0-prev '-> (q0-prev fa-inst))
	(list 'F '-> (F fa-inst))))
