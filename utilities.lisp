;;;; utilities.lisp

(in-package #:org.unaen.cl.lexer)

;;; Program-Wide utility functions.

;; state-names -> preface iterate
;; FA          -> regex-tree FA-prev Q-map Q Σ Σ-in-use Δ q₀ q0-prev F dsn
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

(defmethod vector->list-indices-nil/t ((v vector))
  (loop :for x :across v
	:for i :from 0
	:if x :collect i :into a
	  :else :collect i :into b
	:finally (return (values b a))))

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

(defmethod separate-if ((predicate function) (sequence sequence) &rest rest)
  (let ((matched (list)))
    (values (apply #'remove-if
		   #'(lambda (x)
		       (let ((it (funcall predicate x)))
			 (unless it
			   (push x matched))))
		   sequence
		   rest)
	    (nreverse matched))))

(defmethod symbol->list-in-macro ((thing symbol))
  `(,thing))

(defmethod symbol->list-in-macro ((thing list))
  thing)

;;;; Ignoring existing copy functions

;;; Mutable

;; Cons Cells: LIST (not (null '(a list))), CONS
(defmethod copy-all ((obj cons))
  (loop :for obj->car :in obj :collect (copy-all obj->car)))

;; Vectors: [SIMPLE|BIT|SIMPLE-BIT]-?VECTOR, [SIMPLE|BASE|SIMPLE-BASE]-?STRING
(defmethod copy-all ((obj vector))
  (let* ((array-dimensions (array-dimensions obj))
	 (array-element-type (array-element-type obj))
	 (adjustable-array-p (adjustable-array-p obj))
	 (array-has-fill-pointer-p (array-has-fill-pointer-p obj))
	 (fill-pointer (when array-has-fill-pointer-p
			 (fill-pointer obj))))
    (loop :with new-vector = (make-array array-dimensions
					 :element-type array-element-type
					 :adjustable adjustable-array-p
					 :fill-pointer fill-pointer)
	  :for cell-data :across obj
	  :for cell-int :from 0 :to (if array-has-fill-pointer-p
					fill-pointer
					(first array-dimensions))
	  :do (setf (aref new-vector cell-int)
		    (copy-all cell-data))
	  :finally (return new-vector))))

;; Hash Tables: HASH-TABLE
(defmethod copy-all ((obj hash-table))
  (loop :with new-hash-table = (make-hash-table :test (hash-table-test obj)
						:size (hash-table-size obj)
						:rehash-size (hash-table-rehash-size obj)
						:rehash-threshold (hash-table-rehash-threshold obj))
	:for hash-key :being :the :hash-keys :of obj :using (:hash-value hash-value)
	:do
	   (setf (gethash (copy-all hash-key) new-hash-table)
		 (copy-all hash-value))
	:finally (return new-hash-table)))

;;; Immutable

;; Symbols: SYMBOL, NULL (reduce #'eq '() () nil 'nil), KEYWORD, BOOLEAN
(defmethod copy-all ((obj symbol))
  obj)

;; Numbers: NUMBER, COMPLEX, REAL, [SHORT|SINGLE|DOUBLE|LONG]-?FLOAT, RATIONAL, RATIO, INTEGER,
;;   [FIX|BIG]NUM, [SIGNED|UNSIGNED]-BYTE, BIT
(defmethod copy-all ((obj number))
  obj)

;; Characters: CHARACTER, [EXTENDED|BASE|STANDARD]-CHAR
(defmethod copy-all ((obj character))
  obj)

(defmacro plusplus (place)
  (let ((place-before (gensym)))
    `(let ((,place-before ,place))
       (incf ,place)
       ,place-before)))

(defun truth (ignored-var)
  (declare (ignore ignored-var))
  t)

(defun false (ignored-var)
  (declare (ignore ignored-var))
  nil)

(defmacro alias (old-call-name new-call-name)
  `(defmacro ,new-call-name (&rest args)
     `(,',old-call-name ,@args)))

