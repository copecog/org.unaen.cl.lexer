(in-package #:lexer)

;;; Program-Wide utility functions.

(defmacro with-FA-slots (FA-inst &rest body)
  `(with-slots ((,(intern (string-upcase (concatenate 'string (symbol-name FA-inst) ".Q"))) Q)
		(,(intern (string-upcase (concatenate 'string (symbol-name FA-inst) ".Σ"))) Σ)
		(,(intern (string-upcase (concatenate 'string
						      (symbol-name FA-inst)
						      ".Σ-in-use")))
		 Σ-in-use)
		(,(intern (string-upcase (concatenate 'string (symbol-name FA-inst) ".Δ"))) Δ)
		(,(intern (string-upcase (concatenate 'string (symbol-name FA-inst) ".q0"))) q₀)
		(,(intern (string-upcase (concatenate 'string
						      (symbol-name FA-inst)
						      ".q0-prev")))
		 q₀-prev)
		(,(intern (string-upcase (concatenate 'string (symbol-name FA-inst) ".F"))) F)
		(,(intern (string-upcase (concatenate 'string (symbol-name FA-inst) ".dsn"))) dsn))
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




